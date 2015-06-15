## When adding new options, be sure to update the VALID_OPTIONS list
## (define your own custom validators by assigning a function)
## and update the default_opts() function + documentation in 'get_opts()' below

VALID_OPTIONS <- list(
  auto.snapshot = function(x) x %in% c(TRUE, FALSE),
  use.cache = list(TRUE, FALSE),
  print.banner.on.startup = list(TRUE, FALSE, "auto"),
  vcs.ignore.lib = list(TRUE, FALSE),
  vcs.ignore.src = list(TRUE, FALSE),
  external.packages = function(x) {
    is.null(x) || is.character(x)
  },
  local.repos = function(x) {
    is.null(x) || is.character(x)
  },
  load.external.packages.on.startup = list(TRUE, FALSE),
  ignored.packages = function(x) {
    is.null(x) || is.character(x)
  }
)

default_opts <- function() {
  list(
    auto.snapshot = TRUE,
    use.cache = FALSE,
    print.banner.on.startup = "auto",
    vcs.ignore.lib = TRUE,
    vcs.ignore.src = FALSE,
    external.packages = Sys.getenv("R_PACKRAT_EXTERNAL_PACKAGES", unset = ""),
    local.repos = "",
    load.external.packages.on.startup = TRUE,
    ignored.packages = NULL
  )
}

initOptions <- function(project = NULL, options = default_opts()) {
  project <- getProjectDir(project)
  opts <- c(project = project, options)
  do.call(set_opts, opts)
}

##' Get/set packrat project options
##'
##' Get and set options for the current packrat-managed project.
##'
##' @section Valid Options:
##'
##' \itemize{
##' \item \code{auto.snapshot}: Perform automatic, asynchronous snapshots when running interactively?
##'   (\code{TRUE} / \code{FALSE}; defaults to \code{TRUE})
##' \item \code{use.cache}:
##'   Install packages into a global cache, which is then shared across projects? The
##'   directory to use is read through \code{Sys.getenv("R_PACKRAT_CACHE_DIR")}.
##'   (EXPERIMENTAL; defaults to \code{FALSE})
##' \item \code{print.banner.on.startup}:
##'   Print the banner on startup? Can be one of \code{TRUE} (always print),
##'   \code{FALSE} (never print), and \code{'auto'} (do the right thing)
##'   (defaults to \code{"auto"})
##' \item \code{vcs.ignore.lib}:
##'   Add the packrat private library to your version control system ignore?
##'   (\code{TRUE} / \code{FALSE}; defaults to \code{TRUE})
##' \item \code{vcs.ignore.src}:
##'   Add the packrat private sources to your version control system ignore?
##'   (\code{TRUE} / \code{FALSE}; defaults to \code{FALSE})
##' \item \code{external.packages}:
##'   Packages which should be loaded from the user library. This can be useful for
##'   very large packages which you don't want duplicated across multiple projects,
##'   e.g. BioConductor annotation packages, or for package development scenarios
##'   wherein you want to use e.g. \code{devtools} and \code{roxygen2} for package
##'   development, but do not want your package to depend on these packages.
##'   (EXPERIMENTAL; defaults to \code{Sys.getenv("R_PACKRAT_EXTERNAL_PACKAGES")})
##' \item \code{local.repos}:
##'   Ad-hoc local 'repositories'; i.e., directories containing package sources within
##'   sub-directories. (Character vector; empty by default)
##' \item \code{load.external.packages.on.startup}:
##'   Load any packages specified within \code{external.packages} on startup?
##'   (\code{TRUE} / \code{FALSE}; defaults to \code{TRUE})
##' \item \code{ignored.packages}:
##'   Prevent packrat from tracking certain packages. Dependencies of these packages
##'   will also not be tracked.
##' }
##'
##' @param options A character vector of valid option names.
##' @param simplify Boolean; \code{unlist} the returned options? Useful for when retrieving
##'   a single option.
##' @param project The project directory. When in packrat mode, defaults to the current project;
##'   otherwise, defaults to the current working directory.
##' @param ... Entries of the form \code{key = value}, used for setting packrat project options.
##' @rdname packrat-options
##' @name packrat-options
##' @export
##' @examples \dontrun{
##' ## use 'devtools' and 'knitr' from the user library
##' packrat::set_opts(external.packages = c("devtools", "knitr"))
##'
##' ## set local repository
##' packrat::set_opts(local.repos = c("~/projects/R"))
##'
##' ## get the set of 'external packages'
##' packrat::opts$external.packages()
##'
##' ## set the external packages
##' packrat::opts$external.packages(c("devtools", "knitr"))
##' }
get_opts <- function(options = NULL, simplify = TRUE, project = NULL) {

  project <- getProjectDir(project)

  cachedOptions <- get("options", envir = .packrat)
  if (is.null(cachedOptions)) {
    opts <- read_opts(project = project)
    assign("options", opts, envir = .packrat)
  } else {
    opts <- get("options", envir = .packrat)
  }

  if (is.null(options)) {
    opts
  } else {
    result <- opts[names(opts) %in% options]
    if (simplify) unlist(unname(result))
    else result
  }
}

make_setter <- function(name) {
  force(name)
  return( function(x) {
    if (missing(x)) return(get_opts(name))
    else do.call(set_opts, setNames(list(x), name))
  })
}

##' @rdname packrat-options
##' @name packrat-options
##' @export
set_opts <- function(..., project = NULL) {

  project <- getProjectDir(project)
  optsPath <- packratOptionsFilePath(project)

  if (!file.exists(optsPath)) {
    dir.create(dirname(optsPath), recursive = TRUE, showWarnings = FALSE)
    file.create(optsPath)
  }
  dots <- list(...)
  validateOptions(dots)
  keys <- names(dots)
  values <- dots
  opts <- read_opts(project = project)
  for (i in seq_along(keys)) {
    if (is.null(values[[i]]))
      opts[keys[[i]]] <- list(NULL)
    else
      opts[[keys[[i]]]] <- values[[i]]
  }
  write_opts(opts, project = project)
  updateSettings(project)
  invisible(opts)
}

##' @rdname packrat-options
##' @format NULL
##' @export
opts <- setNames(lapply(names(VALID_OPTIONS), function(x) {
  make_setter(x)
}), names(VALID_OPTIONS))

validateOptions <- function(opts) {
  for (i in seq_along(opts)) {
    key <- names(opts)[[i]]
    value <- opts[[i]]
    if (!(key %in% names(VALID_OPTIONS))) {
      stop("'", key, "' is not a valid packrat option", call. = FALSE)
    }
    opt <- VALID_OPTIONS[[key]]
    if (is.list(opt)) {
      if (!(value %in% opt)) {
        stop("'", value, "' is not a valid setting for packrat option '", key, "'", call. = FALSE)
      }
    } else if (is.function(opt)) {
      if (!opt(value)) {
        stop("'", value, "' is not a valid setting for packrat option '", key, "'", call. = FALSE)
      }
    }

  }
}

## Read an options file with fields unparsed
readOptsFile <- function(path) {
  content <- readLines(path)
  namesRegex <- "^[[:alnum:]\\_\\.]*:"
  namesIndices <- grep(namesRegex, content, perl = TRUE)
  if (!length(namesIndices)) return(list())
  contentIndices <- mapply(seq, namesIndices, c(namesIndices[-1] - 1, length(content)), SIMPLIFY = FALSE)
  if (!length(contentIndices)) return(list())
  result <- lapply(contentIndices, function(x) {
    if (length(x) == 1) {
      result <- sub(".*:\\s*", "", content[[x]], perl = TRUE)
    } else {
      first <- sub(".*:\\s*", "", content[[x[1]]])
      if (first == "") first <- NULL
      rest <- gsub("^\\s*", "", content[x[2:length(x)]], perl = TRUE)
      result <- c(first, rest)
    }
    result[result != ""]
  })
  names(result) <- unlist(lapply(strsplit(content[namesIndices], ":", fixed = TRUE), `[[`, 1))
  result
}

## Read and parse an options file
read_opts <- function(project = NULL) {
  project <- getProjectDir(project)
  path <- packratOptionsFilePath(project)
  if (!file.exists(path)) return(invisible(NULL))
  opts <- readOptsFile(path)
  if (!length(opts)) return(list())
  opts[] <- lapply(opts, function(x) {
    if (identical(x, "TRUE")) {
      return(TRUE)
    } else if (identical(x, "FALSE")) {
      return(FALSE)
    } else if (identical(x, "NA")) {
      return(NA)
    } else {
      x
    }
  })
  opts
}

write_opts <- function(options, project = NULL) {
  project <- getProjectDir(project)
  if (!is.list(options))
    stop("Expecting options as an R list of values")

  # Fill options that are left out
  defaultOpts <- default_opts()
  missingOptionNames <- setdiff(names(defaultOpts), names(options))
  for (optionName in missingOptionNames) {
    opt <- defaultOpts[[optionName]]
    if (is.null(opt)) {
      options[optionName] <- list(NULL)
    } else {
      options[[optionName]] <- opt
    }
  }

  # Preserve order
  options <- options[names(VALID_OPTIONS)]

  labels <- names(options)
  if ("external.packages" %in% names(options)) {
    oep <- as.character(options$external.packages)
    options$external.packages <-
      as.character(unlist(strsplit(oep, "\\s*,\\s*", perl = TRUE)))
  }

  # Update the in-memory options cache
  assign("options", options, envir = .packrat)

  sep <- ifelse(
    unlist(lapply(options, length)) > 1,
    ":\n",
    ": "
  )
  options[] <- lapply(options, function(x) {
    if (length(x) == 0) ""
    else if (length(x) == 1) as.character(x)
    else paste("    ", x, sep = "", collapse = "\n")
  })
  output <- character(length(labels))
  for (i in seq_along(labels)) {
    output[[i]] <- paste(labels[[i]], options[[i]], sep = sep[[i]])
  }
  cat(output, file = packratOptionsFilePath(project), sep = "\n")
}
