## When adding new options, be sure to update the VALID_OPTIONS list
## (define your own custom validators by assigning a function)
## and update the initOptions + documentation below

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
  }
)

default_opts <- function() {
  list(
    auto.snapshot = FALSE,
    use.cache = FALSE,
    print.banner.on.startup = "auto",
    vcs.ignore.lib = TRUE,
    vcs.ignore.src = FALSE,
    external.packages = Sys.getenv("R_PACKRAT_EXTERNAL_PACKAGES", unset = ""),
    local.repos = ""
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
##'   Packages which should be loaded from the user library upon entering packrat mode.
##'   This can be useful for very large packages which you don't want duplicated across
##'   multiple projects, e.g. Bioconductor annotation packages.
##'   (EXPERIMENTAL; defaults to \code{Sys.getenv("R_PACKRAT_EXTERNAL_PACKAGES")})
##' \item \code{local.repos}:
##'   Local 'repositories'; i.e., directories containing package sources either as
##'   folders or as package tarballs. (Character vector; empty by default)
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
get_opts <- function(options = NULL, simplify = TRUE, project = NULL) {
  project <- getProjectDir(project)
  opts <- read_opts(project = project)
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

opts <- setNames(lapply(names(VALID_OPTIONS), function(x) {
  make_setter(x)
}), names(VALID_OPTIONS))

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
    opts[[keys[[i]]]] <- paste(values[[i]], collapse = ", ")
  }
  opts[] <- lapply(opts, function(x) {
    if (!length(x)) "" else paste(x, collapse = ", ")
  })
  write_dcf(opts, file = optsPath)
  updateSettings(project)
  invisible(opts)
}

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

read_opts <- function(project = NULL) {
  project <- getProjectDir(project)
  path <- packratOptionsFilePath(project)
  if (!file.exists(path)) return(invisible(NULL))
  opts <- readDcf(path)
  if (!length(opts)) return(list())
  opts <- setNames(as.list(opts), colnames(opts))
  opts[] <- lapply(opts, function(x) {
    switch(x,
           "TRUE" = TRUE,
           "FALSE" = FALSE,
           "NA" = as.logical(NA),
           unlist(strsplit(x, ",[[:space:]]*", perl = TRUE))
    )
  })
  opts
}
