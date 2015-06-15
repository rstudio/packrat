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
#' @return Returns a list of the names of the packages on which R code in the
#'   application depends.
#' @details Dependencies are determined by parsing application source code and
#'   looking for calls to \code{library}, \code{require}, \code{::}, and
#'   \code{:::}.
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
                            fields = c("Imports", "Depends", "LinkingTo")) {

  if (is.null(available.packages)) available.packages <- available.packages()

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

  ## For R packages, we only use the DESCRIPTION file
  if (isRPackage(project)) {

    ## Make sure we get records recursively from the packages in DESCRIPTION
    parentDeps <-
      pkgDescriptionDependencies(file.path(project, "DESCRIPTION"))$Package

    # Strip out any dependencies the user has requested we do not track.
    parentDeps <- setdiff(parentDeps, packrat::opts$ignored.packages())

    ## For downstream dependencies, we don't grab their Suggests:
    ## Presumedly, we can build child dependencies without vignettes, and hence
    ## do not need suggests -- for the package itself, we should make sure
    ## we grab suggests, however
    childDeps <- recursivePackageDependencies(parentDeps,
                                              libPaths,
                                              available.packages,
                                              fields)
  } else {
    parentDeps <- setdiff(unique(c(dirDependencies(project))), "packrat")
    parentDeps <- setdiff(parentDeps, packrat::opts$ignored.packages())
    childDeps <- recursivePackageDependencies(parentDeps,
                                              libPaths,
                                              available.packages,
                                              fields)
  }

  result <- unique(c(parentDeps, childDeps, "packrat"))

  # If this project is implicitly a shiny application, then
  # add that in as the previously run expression dependency lookup
  # won't have found it.
  if (!("shiny" %in% result) && isShinyApp(project))
    result <- c(result, "shiny")

  sort_c(result)
}

# detect all package dependencies for a directory of files
dirDependencies <- function(dir) {
  dir <- normalizePath(dir, winslash='/')

  # first get the packages referred to in source code
  pattern <- "\\.[rR]$|\\.[rR]md$|\\.[rR]nw$|\\.[rR]pres$"
  pkgs <- character()
  R_files <- list.files(dir,
                        pattern = pattern,
                        ignore.case = TRUE,
                        recursive = TRUE
  )

  ## Avoid anything within the packrat directory itself -- all inference
  ## should be done on user code
  packratDirRegex <- paste("^", .packrat$packratFolderName, sep = "")
  R_files <- grep(packratDirRegex, R_files, invert = TRUE, value = TRUE)


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
  fileext <- tolower(gsub(".*\\.", "", file))
  switch (fileext,
          r = fileDependencies.R(file),
          rmd = fileDependencies.Rmd(file),
          rnw = fileDependencies.Rnw(file),
          rpres = fileDependencies.Rpres(file),
          stop("Unrecognized file type '", file, "'")
  )
}

hasYamlFrontMatter <- function(content) {
  lines <- grep("^---\\s*$", content, perl = TRUE)
  1 %in% lines && length(lines) >= 2
}

yamlDeps <- function(yaml) {
  # Collapse with newlines -- makes regex parsing easier
  yaml <- paste(yaml, collapse = "\n")
  c(
    "rmarkdown",
    "shiny"[any(grepl("runtime:[[:space:]]*shiny", yaml, perl = TRUE))],
    "rticles"[any(grepl("rticles::", yaml, perl = TRUE))]
  )
}

fileDependencies.Rmd <- function(file) {

  # We need to check for and parse YAML frontmatter if necessary
  yamlDeps <- NULL
  content <- readLines(file)
  if (hasYamlFrontMatter(content)) {
    tripleDashes <- grep("^---\\s*$", content, perl = TRUE)
    start <- tripleDashes[[1]]
    end <- tripleDashes[[2]]
    yamlDeps <- yamlDeps(content[(start + 1):(end - 1)])
  }

  deps <- yamlDeps

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
  })

  if (requireNamespace("knitr", quietly = TRUE)) {
    tempfile <- tempfile()
    on.exit(unlink(tempfile))
    tryCatch(silent(
      knitr::knit(file, output = tempfile, tangle = TRUE)
    ), error = function(e) {
      message("Unable to tangle file '", file, "'; cannot parse dependencies")
      character()
    })
    c(deps, fileDependencies.R(tempfile))
  } else {
    warning("knitr is required to parse dependencies but is not available")
    deps
  }
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

  # parse file and examine expressions
  tryCatch({
    # parse() generates a warning when the file has an incomplete last line, but
    # it still parses the file correctly; ignore this and other warnings.
    # We'll still halt when parsing fails.
    exprs <- suppressWarnings(parse(file, n = -1L))
    for (i in seq_along(exprs))
      pkgs <- append(pkgs, expressionDependencies(exprs[[i]]))
  }, error = function(e) {
    warning(paste("Failed to parse", file, "; dependencies in this file will",
                  "not be discovered."))
  })

  # return packages
  unique(pkgs)
}

# detect the package dependencies of an expression (adapted from
# tools:::.check_packages_used)
#
# expressionDependencies(quote(library("h")))
# expressionDependencies(quote(library(10, package = "h")))
# expressionDependencies(quote(library(h)))
# expressionDependencies(quote({library(h); library(g)}))
# expressionDependencies(quote(h::f))
expressionDependencies <- function(e) {
  # base case
  if (is.atomic(e) || is.name(e)) return()

  # recursive case: expression (= list of calls)
  if (is.expression(e)) {
    return(unlist(lapply(e, expressionDependencies)))
  }

  ## traverse a call to find `::`, `:::`, `library`, `require` calls
  if (is.call(e)) {
    parent <- e
    child <- e[[1L]]
    while (is.call(child)) {
      parent <- parent[[1L]]
      child <- parent[[1L]]
    }
    if (as.character(child) %in% c("::", ":::")) {
      return(as.character(parent[[2L]]))
    } else if (as.character(child) %in% c("library", "require")) {
      # Match arguments
      call <- eval(call("match.call", child, quote(parent)))

      # If character.only is TRUE and 'package' is a symbol, don't return that
      # symbol
      pkg <- call[["package"]]
      co <- call[["character.only"]]
      if (isTRUE(co) && is.symbol(pkg)) {
        return()
      } else {
        return(as.character(pkg))
      }
    }
  }

  # base case: a call
  fname <- as.character(e[[1L]])
  # a refclass method call, so return
  if (length(fname) > 1) return()

  if (length(fname) == 1) {

    # base case: call to library/require
    if (fname %in% c("library", "require")) {
      mc <- match.call(get(fname, baseenv()), e)
      if (is.null(mc$package)) return(NULL)
      if (isTRUE(mc$character.only)) return(NULL)

      return(as.character(mc$package))
    }

    # base case: call to :: or :::
    if (fname %in% c("::", ":::"))
      return(as.character(e[[2L]]))

    # base case: methods functions
    if (fname %in% c("setClass", "setRefClass", "setMethod", "setGeneric")) {
      return("methods")
    }

  }

  # recursive case: all other calls
  children <- lapply(as.list(e[-1]), expressionDependencies)
  unique(unlist(children))
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

  # If 'Type' is missing from the DESCRIPTION file, then we implicitly assume
  # that it is an R package (#172)
  if (!("Type" %in% names(DESCRIPTION)))
    return(TRUE)

  # Otherwise, ensure that the type is `Package`
  Type <- unname(as.character(DESCRIPTION$Type))
  identical(Type, "Package")

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
