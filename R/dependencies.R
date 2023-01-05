#' Detect Application Dependencies
#'
#' Recursively detect all package dependencies for an application. This function
#' parses all \R files in the application directory to determine what packages
#' the application depends directly.
#'
#' Only direct dependencies are detected (i.e. no recursion is done to find the
#' dependencies of the dependencies).
#'
#' @param project Directory containing application. Defaults to current working
#'   directory.
#' @param implicit.packrat.dependency Include \code{packrat} as an implicit
#'   dependency of this project, if not otherwise discovered? This should be
#'   \code{FALSE} only if you can guarantee that \code{packrat} will be available
#'   via other means when attempting to load this project.
#'
#' @details Dependencies are determined by parsing application source code and
#'   looking for calls to \code{library}, \code{require}, \code{::}, and
#'   \code{:::}.
#'
#' @return Returns a list of the names of the packages on which R code in the
#'   application depends.
#'
#' @examples
#'
#' \dontrun{
#'
#' # dependencies for the app in the current working dir
#' appDependencies()
#'
#' # dependencies for an app in another directory
#' appDependencies("~/projects/shiny/app1")
#'
#' }
#' @keywords internal
appDependencies <- function(project = NULL,
                            available.packages = NULL,
                            fields = opts$snapshot.fields(),
                            implicit.packrat.dependency = TRUE) {

  if (is.null(available.packages))
    available.packages <- availablePackages()

  project <- getProjectDir(project)

  ## We want to search both local and global library paths for DESCRIPTION files
  ## in the recursive dependency lookup; hence we take a large (ordered) union
  ## of library paths. The ordering ensures that we search the private library first,
  ## and fall back to the local / global library (necessary for `packrat::init`)
  libPaths <- c(
    libDir(project),
    .libPaths(),
    .packrat_mutables$origLibPaths
  )

  ignores <- packrat::opts$ignored.packages()

  ## For R packages, we only use the DESCRIPTION file
  if (isRPackage(project)) {

    ## Make sure we get records recursively from the packages in DESCRIPTION
    parentDeps <-
      pkgDescriptionDependencies(file.path(project, "DESCRIPTION"))$Package

    # Strip out any dependencies the user has requested we do not track.
    parentDeps <- setdiff(parentDeps, ignores)

    ## For downstream dependencies, we don't grab their Suggests:
    ## Presumedly, we can build child dependencies without vignettes, and hence
    ## do not need suggests -- for the package itself, we should make sure
    ## we grab suggests, however
    childDeps <- recursivePackageDependencies(parentDeps,
                                              ignores,
                                              libPaths,
                                              available.packages,
                                              fields)
  } else {
    parentDeps <- setdiff(unique(c(dirDependencies(project))), "packrat")
    parentDeps <- setdiff(parentDeps, ignores)
    childDeps <- recursivePackageDependencies(parentDeps,
                                              ignores,
                                              libPaths,
                                              available.packages,
                                              fields)
  }

  result <- unique(c(parentDeps, childDeps))

  # should packrat be included as automatic dependency?
  if (implicit.packrat.dependency) {
    result <- unique(c(result, "packrat"))
  }

  # If this project is implicitly a shiny application, then
  # add that in as the previously run expression dependency lookup
  # won't have found it.
  if (!("shiny" %in% result) && isShinyApp(project))
    result <- c(result, "shiny")

  if (is.null(result))
    return(character())

  sorted <- sort_c(result)

  # some users have seen empty package names discovered here
  # although we don't know the underlying cause, we should
  # just filter these out as we know they can't be valid
  setdiff(sorted, "")
}

# detect all package dependencies for a directory of files
dirDependencies <- function(dir) {
  if (as.logical(getOption("packrat.dependency.discovery.disabled", default = FALSE))) {
    character()
  } else if (as.logical(getOption("packrat.dependency.discovery.renv", default = TRUE))) {
    dirDependenciesRenv(dir)
  } else {
    dirDependenciesBuiltIn(dir)
  }
}

# Return renv ignore patterns based on the packrat ignored.directories option.
# Each directory is returned as a rooted pattern for renv, meaning that it
# should only apply at the root directory of the project.
#
# Note: The "/data/" and "/inst/" directories are ignored by default.
#
# See: https://github.com/rstudio/renv/pull/866
#
# See: renv:::renv_renvignore_parse_impl
ignoresForRenv <- function(dir, ignoredDirectories) {
  ignores <- NULL
  if (length(ignoredDirectories) > 0) {
    ignores <- ignoredDirectories
    # Make sure all the directories end with a slash.
    ignores <- ifelse(
        substr(ignores, nchar(ignores), nchar(ignores)) != "/",
        paste0(ignores, "/"),
        ignores
    )
    # Make sure all the directories begin with a slash.
    ignores <- ifelse(
        substr(ignores, 1, 1) != "/",
        paste0("/", ignores),
        ignores
    )
    # Prepend the project root and quote.
    ignores <- paste0('^\\Q', dir, '\\E\\Q', ignores, '\\E$')
    # Tell renv that these rules do not need additional parsing.
    attr(ignores, "asis") <- TRUE
  }
  ignores
}

dirDependenciesRenv <- function(dir) {
  old_filebacked_cache <- options(renv.config.filebacked.cache = FALSE)
  on.exit(do.call(options, old_filebacked_cache), add = TRUE)

  project <- Sys.getenv("RENV_PROJECT", unset = NA)
  if (!is.na(project)) {
    Sys.unsetenv("RENV_PROJECT")
    on.exit(Sys.setenv(RENV_PROJECT = project), add = TRUE)
  }

  profile <- Sys.getenv("RENV_PROFILE", unset = NA)
  if (!is.na(profile)) {
    Sys.unsetenv("RENV_PROFILE")
    on.exit(Sys.setenv(RENV_PROFILE = profile), add = TRUE)
  }

  absDir <- normalizePath(dir, winslash = "/")

  old_ignored_packages <- options("renv.settings.ignored.packages" = opts$ignored.packages())
  on.exit(do.call(options, old_ignored_packages), add = TRUE)

  old_renv_exclude <- options("renv.renvignore.exclude" = ignoresForRenv(absDir, opts$ignored.directories()))
  on.exit(do.call(options, old_renv_exclude), add = TRUE)

  # TODO: add rsconnect as an ignored directory? May not be an issue for
  # bundling, since we don't include the rsconnect directory.

  deps <- renv$dependencies(path = absDir, root = absDir, quiet = TRUE)
  pkgs <- unique(deps$Package)
  ## Exclude recommended packages (and the artifical "R" package) if there is
  ## no package installed locally this places an implicit dependency on the
  ## system-installed version of a package
  pkgs <- dropSystemPackages(pkgs)
  pkgs
}

# detect all package dependencies for a directory of files
dirDependenciesBuiltIn <- function(dir) {
  dir <- normalizePath(dir, winslash = '/')

  # first get the packages referred to in source code
  pattern <- "[.](?:r|rmd|qmd|rnw|rpres)$"
  pkgs <- character()
  R_files <- list.files(dir,
                        pattern = pattern,
                        ignore.case = TRUE,
                        recursive = TRUE
  )

  ## Avoid anything within the packrat directory itself -- all inference
  ## should be done on user code
  packratDirRegex <- "(?:^|/)packrat"
  R_files <- grep(packratDirRegex, R_files, invert = TRUE, value = TRUE)

  ## Avoid anything on the list of ignored directories
  ignoredDir <- get_opts("ignored.directories")
  if (length(ignoredDir) > 0) {
    # Make sure all the directories end with a slash...
    ignoredDir <- ifelse(
      substr(ignoredDir, nchar(ignoredDir), nchar(ignoredDir)) != "/",
      paste0(ignoredDir, "/"),
      ignoredDir
    )

    # Make a regex to match any of them.
    ignoredDirRegex <- paste0(
      "(?:^",
      paste0(
        ignoredDir,
        collapse = ")|(?:^"
      ),
      ")"
    )
    R_files <- grep(ignoredDirRegex, R_files, invert = TRUE, value = TRUE)
  }

  sapply(R_files, function(file) {
    filePath <- file.path(dir, file)
    pkgs <<- append(pkgs, fileDependencies(file.path(dir, file)))

  })

  ## Exclude recommended packages if there is no package installed locally
  ## this places an implicit dependency on the system-installed version of a package
  dropSystemPackages(pkgs)

}

# detect all package dependencies for a source file (parses the file and then
# recursively examines all expressions in the file)

# ad-hoc dispatch based on the file extension
fileDependencies <- function(file) {
  file <- normalizePath(file, winslash = "/", mustWork = TRUE)
  fileext <- tolower(gsub(".*\\.", "", file))
  switch(fileext,
         r = fileDependencies.R(file),
         rmd = fileDependencies.Rmd(file),
         qmd = fileDependencies.Qmd(file),
         rnw = fileDependencies.Rnw(file),
         rpres = fileDependencies.Rpres(file),
         stop("Unrecognized file type '", file, "'")
  )
}

hasYamlFrontMatter <- function(content) {
  lines <- grep("^(---|\\.\\.\\.)\\s*$", content, perl = TRUE)
  1 %in% lines && length(lines) >= 2 && grepl("^---\\s*$", content[1], perl = TRUE)
}

yamlDeps <- function(yaml) {
  unique(c(
    "shiny"[any(grepl("runtime:[[:space:]]*shiny", yaml, perl = TRUE))],
    "shiny"[any(grepl("server:[[:space:]]*shiny", yaml, perl = TRUE))],
    "shiny"[any(grepl("[[:space:]]+type:[[:space:]]*shiny", yaml, perl = TRUE))],
    "rticles"[any(grepl("rticles::", yaml, perl = TRUE))]
  ))
}

stripAltEngines <- function(file, encoding) {
  contents <- readLines(file, encoding = encoding)

  # generate a list of all the headers
  engineHeaders <- which(grepl("^## --.*engine=", contents))
  allHeaders <- c(which(grepl("^## --", contents)), length(contents))

  # calculate the end of each alternate engine code block (the beginning of the
  # very next code block)
  engineEnds <- vapply(engineHeaders, function(x) {
    allHeaders[min(which(allHeaders > x))] - 1
  }, 0)

  # exclude the alternate engine code block lines
  regions <- rep.int(TRUE, length(contents))
  for (h in seq_along(engineHeaders)) {
    regions[engineHeaders[[h]]:engineEnds[[h]]] <- FALSE
  }

  writeLines(contents[regions], file)
}

# compute package dependencies for an *.qmd file. not all Quarto documents
# require R/rmarkdown.
#
# Quarto/rsconnect may independently indicate that this file needs the knitr
# engine and will communicate an implicit dependency on rmarkdown
fileDependencies.Qmd <- function(file) {
  fileDependencies.Markdown(file, implicit = NULL)
}

# compute package dependencies for an *.Rmd file. rmarkdown is an automatic,
# implicit dependency.
fileDependencies.Rmd <- function(file) {
  fileDependencies.Markdown(file, implicit = c("rmarkdown"))
}

fileDependencies.Markdown <- function(file, implicit = NULL) {

  deps <- c()
  if (!is.null(implicit)) {
    deps <- c(deps, implicit)
  }

  # try using an evaluate-based approach for dependencies
  if (knitrHasEvaluateHook()) {

    # attempt to load rmarkdown
    isRmarkdownLoaded <- "rmarkdown" %in% loadedNamespaces()
    if (requireNamespace("rmarkdown", quietly = TRUE)) {

      # unload rmarkdown after we're done with it if it
      # wasn't already loaded
      if (!isRmarkdownLoaded) {
        on.exit(
          try(unloadNamespace("rmarkdown"), silent = TRUE),
          add = TRUE
        )
      }

      # render with a custom evaluate hook to discover dependencies
      deps <- c(deps, fileDependencies.evaluate(file))
    }
  }

  # we don't know this file's encoding, so presume the default encoding
  encoding <- getOption("encoding")
  format <- NULL

  # check whether the default output format references a package
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    tryCatch({
      format <- rmarkdown::default_output_format(file)
    }, error = function(e) {
      # if we can't parse the YAML header with the default encoding, try UTF-8
      encoding <<- "UTF-8"
      format <<- rmarkdown::default_output_format(file, encoding)
    })
    components <- strsplit(format$name, "::")[[1]]
    if (length(components) == 2) {
      deps <- c(deps, components[[1]])
    }
  }

  # We need to check for and parse YAML frontmatter if necessary
  yamlDeps <- NULL
  content <- readLines(file, encoding = encoding, warn = FALSE)
  if (hasYamlFrontMatter(content)) {

    # Extract the YAML frontmatter.
    tripleDashesDots <- grep("^(---|\\.\\.\\.)\\s*$", content, perl = TRUE)
    start <- tripleDashesDots[[1]]
    end <- tripleDashesDots[[2]]
    yaml <- paste(content[(start + 1):(end - 1)], collapse = "\n")

    # Populate 'deps'.
    yamlDeps <- yamlDeps(yaml)
    deps <- c(deps, yamlDeps)

    # Extract additional dependencies from YAML parameters.
    if (requireNamespace("knitr", quietly = TRUE) &&
        packageVersion("knitr") >= "1.10.18")
    {
      # attempt to extract knitr params from yaml
      knitParams <- tryCatch(
        knitr::knit_params_yaml(yaml, evaluate = FALSE),
        error = function(e) {
          warning(e)
          NULL
        }
      )

      if (length(knitParams)) {
        deps <- c(deps, "shiny")
        for (param in knitParams) {
          if (!is.null(param$expr)) {
            parsed <- quietly(parse(text = param$expr))
            if (!inherits(parsed, "error"))
              deps <- c(deps, expressionDependencies(parsed))
          }
        }
      }

    }
  }

  # Escape hatch for empty .Rmd files
  if (!length(content) || identical(unique(gsub("[[:space:]]", "", content, perl = TRUE)), "")) {
    return(deps)
  }

  ## Unload knitr if needed only for the duration of this function call
  ## This prevents errors with e.g. `packrat::restore` performed after
  ## a `fileDependencies.Rmd` call on Windows, where having knitr loaded
  ## would prevent an installation of knitr to succeed
  knitrIsLoaded <- "knitr" %in% loadedNamespaces()
  on.exit({
    if (!knitrIsLoaded && "knitr" %in% loadedNamespaces()) {
      try(unloadNamespace("knitr"), silent = TRUE)
    }
  }, add = TRUE)

  if (requireNamespace("knitr", quietly = TRUE)) {
    deps <- c(
      deps,
      fileDependencies.tangle(file, encoding = encoding)
    )
  } else {
    warning("knitr is required to parse dependencies but is not available")
  }

  unique(deps)
}

fileDependencies.knitr <- function(...) {
  fileDependencies.Rmd(...)
}

fileDependencies.Rpres <- function(...) {
  fileDependencies.Rmd(...)
}

fileDependencies.Rnw <- function(file) {
  tempfile <- tempfile()
  on.exit(unlink(tempfile))
  tryCatch(silent({
    utils::Stangle(file, output = tempfile)
    fileDependencies.R(tempfile)
  }), error = function(e) {
    fileDependencies.knitr(file)
  })
}

fileDependencies.R <- function(file) {

  if (!file.exists(file)) {
    warning("No file at path '", file, "'.")
    return(character())
  }

  # build a list of package dependencies to return
  pkgs <- character()

  # parse file and examine expressions -- first attempt to
  # parse in system encoding, then try again with UTF-8
  exprs <- quietly(parse(file, n = -1L))
  if (inherits(exprs, "error"))
    exprs <- quietly(parse(file, n = -1L, encoding = "UTF-8"))

  # report parse errors to the user
  if (inherits(exprs, "error")) {
    warning(paste("Failed to parse", file, "; dependencies in this file will",
                  "not be discovered."))
    exprs <- NULL
  }

  # extract expression dependencies
  for (i in seq_along(exprs))
    pkgs <- append(pkgs, expressionDependencies(exprs[[i]]))

  # return packages
  setdiff(unique(pkgs), "")
}

anyOf <- function(object, ...) {
  predicates <- list(...)
  for (predicate in predicates)
    if (predicate(object))
      return(TRUE)
  FALSE
}

allOf <- function(object, ...) {
  predicates <- list(...)
  for (predicate in predicates)
    if (!predicate(object))
      return(FALSE)
  TRUE
}

recursiveWalk <- function(`_node`, fn, ...) {
  fn(`_node`, ...)
  if (is.recursive(`_node`)) {
    for (i in seq_along(`_node`)) {
      recursiveWalk(`_node`[[i]], fn, ...)
    }
  }
}

# Fills 'env' as a side effect
identifyPackagesUsed <- function(call, env) {

  if (!is.call(call))
    return()

  fn <- call[[1]]
  if (!anyOf(fn, is.character, is.symbol))
    return()

  fnString <- as.character(fn)

  # Check for '::', ':::'
  if (fnString %in% c("::", ":::")) {
    if (anyOf(call[[2]], is.character, is.symbol)) {
      pkg <- as.character(call[[2]])
      env[[pkg]] <- TRUE
      return()
    }
  }

  # Check for S4-related function calls (implying a dependency on methods)
  if (fnString %in% c("setClass", "setMethod", "setRefClass", "setGeneric", "setGroupGeneric")) {
    env[["methods"]] <- TRUE
    return()
  }

  # Check for package loaders.
  #
  # The library() and require() calls accept symbols directly as package
  # names, while loadNamespace() and requireNamespace() do not.
  liberalLoaders <- c("library", "require")
  strictLoaders <- c("loadNamespace", "requireNamespace")
  pkgLoaders <- c(strictLoaders, liberalLoaders)
  if (!fnString %in% pkgLoaders)
    return()

  # Try matching the call.
  loader <- tryCatch(
    get(fnString, envir = asNamespace("base")),
    error = function(e) NULL
  )

  if (!is.function(loader))
    return()

  matched <- match.call(loader, call)
  if (!"package" %in% names(matched))
    return()

  if (fnString %in% liberalLoaders) {
    # Protect against 'character.only = TRUE' + symbols.
    # This defends us against a construct like:
    #
    #    for (x in pkgs)
    #        library(x, character.only = TRUE)
    #
    if (!"character.only" %in% names(matched)) {
      if (anyOf(matched[["package"]], is.character, is.symbol)) {
        pkg <- as.character(matched[["package"]])
        env[[pkg]] <- TRUE
        return()
      }
    }
  }

  if (anyOf(matched[["package"]], is.character)) {
    pkg <- as.character(matched[["package"]])
    env[[pkg]] <- TRUE
    return()
  }
}

expressionDependencies <- function(e) {
  if (is.expression(e)) {
    return(unlist(lapply(e, function(call) {
      expressionDependencies(call)
    })))
  }

  else if (is.call(e)) {
    env <- new.env(parent = emptyenv())
    recursiveWalk(e, identifyPackagesUsed, env)
    return(ls(env, all.names = TRUE))
  }

  else character()

}

# Read a DESCRIPTION file into a data.frame
readDESCRIPTION <- function(path) {

  if (!file.exists(path))
    stop("No DESCRIPTION file at path '", path, "'")

  tryCatch(
    readDcf(file = path, all = TRUE),
    error = function(e) {
      return(data.frame())
    }
  )
}

isRPackage <- function(project) {

  descriptionPath <- file.path(project, "DESCRIPTION")
  if (!file.exists(descriptionPath))
    return(FALSE)

  DESCRIPTION <- readDESCRIPTION(descriptionPath)

  # If 'Type' is in the DESCRIPTION, ensure it's equal to 'Package'.
  if ("Type" %in% names(DESCRIPTION))
    return(identical(DESCRIPTION$Type, "Package"))

  # Some packages will have a DESCRIPTION file without the 'Type' field.
  # Check that these still declare themselves with the 'Package' field.
  if ("Package" %in% names(DESCRIPTION))
    return(TRUE)

  # DESCRIPTION for a non-R package (e.g. Shiny application?)
  FALSE

}

# Infer whether a project is (implicitly) a Shiny application,
# in the absence of explicit `library()` statements.
isShinyApp <- function(project) {

  # Check for a DESCRIPTION file with 'Type: Shiny'
  descriptionPath <- file.path(project, "DESCRIPTION")
  if (file.exists(descriptionPath)) {
    DESCRIPTION <- readDESCRIPTION(descriptionPath)
    if (length(DESCRIPTION$Type) && tolower(DESCRIPTION$Type) == "shiny")
      return(TRUE)
  }

  # Check for a server.r with a 'shinyServer' call
  serverPath <- file.path(project, "server.R")
  if (file.exists(file.path(project, "server.R"))) {
    contents <- paste(readLines(serverPath), collapse = "\n")
    if (grepl("shinyServer\\s*\\(", contents, perl = TRUE))
      return(TRUE)
  }

  # Check for a single-file application with 'app.R'
  appPath <- file.path(project, "app.R")
  if (file.exists(appPath)) {
    contents <- paste(readLines(appPath), collapse = "\n")
    if (grepl("shinyApp\\s*\\(", contents, perl = TRUE))
      return(TRUE)
  }

  return(FALSE)
}

knitrHasEvaluateHook <- function() {
  isKnitrLoaded <- "knitr" %in% loadedNamespaces()
  if (!requireNamespace("knitr", quietly = TRUE))
    return(FALSE)

  if (!isKnitrLoaded) {
    on.exit(
      try(unloadNamespace("knitr"), silent = TRUE),
      add = TRUE
    )
  }

  hooks <- knitr::knit_hooks$get()
  "evaluate" %in% names(hooks)
}


fileDependencies.evaluate <- function(file) {

  # discovered packages (to be updated by evaluate hook)
  deps <- list()

  # override any existing engines -- we don't want dependency discovery
  # to, say, run arbitrary bash scripts contained in the document!
  engines <- knitr::knit_engines$get()
  on.exit(knitr::knit_engines$restore(engines), add = TRUE)

  # generate overrides
  overrides <- replicate(length(engines), function(options) {}, FALSE)
  names(overrides) <- names(engines)

  # retain the regular R knitr hook, and treat Rscript chunks
  # the same way as "regular" R chunks
  overrides$R <- overrides$Rscript <- engines$R
  knitr::knit_engines$set(overrides)

  # save old hook and install our custom hook
  evaluate_hook <- knitr::knit_hooks$get("evaluate")
  on.exit(knitr::knit_hooks$set(evaluate = evaluate_hook), add = TRUE)
  knitr::knit_hooks$set(evaluate = function(code, ...) {
    try(silent = TRUE, {
      parsed <- parse(text = code, encoding = "UTF-8")
      deps <<- c(deps, expressionDependencies(parsed))
    })
  })

  # keep going on error
  chunkOptions <- knitr::opts_chunk$get()
  on.exit(knitr::opts_chunk$restore(chunkOptions), add = TRUE)
  knitr::opts_chunk$set(error = TRUE)

  # rudely override knitr's 'inline_exec' function so
  # that we can detect dependencies within inline chunks
  knitr <- asNamespace("knitr")
  if (exists("inline_exec", envir = knitr)) {

    inline_exec <- yoink("knitr", "inline_exec")
    do.call("unlockBinding", list("inline_exec", knitr))
    assign("inline_exec", function(block, ...) {

      # do our own special stuff
      try(silent = TRUE, {
        code <- paste(block$code, collapse = "\n")
        parsed <- parse(text = code, encoding = "UTF-8")
        deps <<- c(deps, expressionDependencies(parsed))
      })

      # return block input without evaluating anything
      block$input

    }, envir = knitr)

    on.exit({
      assign("inline_exec", inline_exec, envir = knitr)
      do.call("lockBinding", list("inline_exec", knitr))
    }, add = TRUE)

  }

  # attempt to render document with our custom hook active
  # TODO: do we want to report errors here? right now we're just
  # capturing and silently discarding render errors
  outfile <- tempfile()
  on.exit(unlink(outfile), add = TRUE)

  tryCatch(
    withCallingHandlers(
      rmarkdown::render(file, output_file = outfile, quiet = TRUE),
      warning = function(w) {

        # ignore warnings emitted by knitr::get_engine()
        get_engine <- yoink("knitr", "get_engine")
        for (i in seq_len(sys.nframe())) {
          fn <- sys.function(i)
          if (identical(fn, get_engine))
            invokeRestart("muffleWarning")
        }

      }
    ),
    error = identity
  )

  unique(unlist(deps, recursive = TRUE))
}




# Extract dependencies per chunk rather than per file.
# Packages like learnr have special R code chunks that are not evaluated at run time.
# While the .Rmd file can be rendered with rmarkdown, a raw tangled R file may not be able to be processed.
fileDependencies.tangle <- function(file, encoding = "UTF-8") {

  # discovered packages
  deps <- list()

  # unique key (line) to split R code with
  key <- paste0("###--packrat-", paste0(sample(letters, 10, replace = TRUE), collapse = ""), "\n")

  # rudely override knitr's 'label_code' function so
  # that we can detect dependencies within inline chunks
  knitr <- asNamespace("knitr")
  if (exists("label_code", envir = knitr)) {
    label_code <- yoink("knitr", "label_code")
    do.call("unlockBinding", list("label_code", knitr))
    assign("label_code", function(...) {
      # paste a known key to split the code chunks by
      paste0(key, label_code(...))
    }, envir = knitr)

    on.exit({
      assign("label_code", label_code, envir = knitr)
      do.call("lockBinding", list("label_code", knitr))
    }, add = TRUE)
  }

  # tangle out file
  outfile <- tempfile()
  on.exit({
    unlink(outfile)
  }, add = TRUE)

  # attempt to tangle document with our custom hook active
  tryCatch(silent(
    knitr::purl(
      file,
      output = outfile, # tangled file location
      quiet = TRUE,

      # `An integer specifying the level of documentation to add
      # to the tangled script. 1L (the default) means to add
      # the chunk headers to the code`
      documentation = 1L,
      encoding = encoding
    )
 ), error = function(e) {
   message("Unable to tangle file '", file, "'; cannot parse dependencies")
   character()
 })

  if (!file.exists(outfile)) {
    # nothing was created
    return(NULL)
  }

  stripAltEngines(outfile, encoding)

  # parse each r chunk independently to retrieve dependencies
  # allows for some chunks to be _broken_ but not stop retrieving dependencies
  r_chunks <- strsplit(paste0(readLines(outfile), collapse = "\n"), key)[[1]]
  for (r_chunk in r_chunks) {
    try(silent = TRUE, {
      parsed <- parse(text = r_chunk, encoding = encoding)
      deps <- c(deps, expressionDependencies(parsed))
    })
  }

  unique(unlist(deps, recursive = TRUE))
}
