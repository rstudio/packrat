## When adding new options, be sure to update the VALID_OPTIONS list
## (define your own custom validators by assigning a function)
## and update the initOptions + documentation below

VALID_OPTIONS <- list(
  auto.snapshot = function(x) x %in% c(TRUE, FALSE),
  git.ignore.lib = list(TRUE, FALSE),
  git.ignore.src = list(TRUE, FALSE),
  svn.ignore.lib = list(TRUE, FALSE),
  svn.ignore.src = list(TRUE, FALSE)
)


initOptions <- function(project = NULL) {
  project <- getProjectDir(project)
  set_opts(
    auto.snapshot = TRUE,
    git.ignore.lib = TRUE,
    git.ignore.src = FALSE,
    svn.ignore.lib = TRUE,
    svn.ignore.src = FALSE
  )
}

##' Get/set packrat project options
##'
##' Get and set a number of options for the current packrat-managed project.
##'
##' \itemize{
##' \item \code{auto.snapshot}: Perform automatic, asynchronous snapshots when running interactively?
##' \item \code{git.ignore.lib}: Add the packrat private library to the git ignore?
##' \item \code{git.ignore.src}: Add the packrat private sources to the git ignore?
##' \item \code{svn.ignore.lib}: Add the packrat private library to the SVN ignore?
##' \item \code{svn.ignore.src}: Add the packrat private sources to the SVN ignore?
##' }
##' @param options A character vector of valid option names.
##' @param ... Entries of the form \code{key = value}, used for setting packrat project options.
##' @rdname options
##' @export
get_opts <- function(options = NULL) {
  opts <- read_opts()
  if (is.null(options)) {
    opts
  } else {
    opts[names(opts) %in% options]
  }
}

##' @rdname options
##' @export
set_opts <- function(...) {
  if (!file.exists(packratOptionsFilePath())) {
    dir.create(dirname(packratOptionsFilePath()), recursive = TRUE)
    file.create(packratOptionsFilePath())
  }
  dots <- list(...)
  validateOptions(dots)
  keys <- names(dots)
  values <- unname(unlist(dots))
  opts <- read_opts()
  for (i in seq_along(keys)) {
    opts[[keys[[i]]]] <- values[[i]]
  }
  write.dcf(opts, file = packratOptionsFilePath())
  updateSettings()
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
  path <- packratOptionsFilePath(project)
  if (!file.exists(path)) return(invisible(NULL))
  opts <- readDcf(path)
  as.list(apply(opts, 2, function(x) {
    eval(parse(text = x), envir = emptyenv())
  }))
}
