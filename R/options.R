## When adding new options, be sure to update the VALID_OPTIONS list
## (define your own custom validators by assigning a function)
## and update the initOptions + documentation below

VALID_OPTIONS <- list(
  auto.snapshot = function(x) x %in% c(TRUE, FALSE),
  vcs.ignore.lib = list(TRUE, FALSE),
  vcs.ignore.src = list(TRUE, FALSE),
  print.banner.on.startup = list(TRUE, FALSE, "auto")
)


initOptions <- function(project = NULL) {
  project <- getProjectDir(project)
  set_opts(
    project = project,
    auto.snapshot = TRUE,
    vcs.ignore.lib = TRUE,
    vcs.ignore.src = FALSE,
    print.banner.on.startup = "auto"
  )
}

##' Get/set packrat project options
##'
##' Get and set options for the current packrat-managed project.
##'
##' \itemize{
##' \item \code{auto.snapshot}: Perform automatic, asynchronous snapshots when running interactively?
##'   (\code{TRUE} / \code{FALSE})
##' \item \code{vcs.ignore.lib}:
##'   Add the packrat private library to your version control system ignore? (\code{TRUE} / \code{FALSE})
##' \item \code{vcs.ignore.src}:
##'   Add the packrat private sources to your version control system ignore? (\code{TRUE} / \code{FALSE})
##' \item \code{print.banner.on.startup}:
##'   Print the banner on startup? Can be one of \code{TRUE} (always print),
##'   \code{FALSE} (never print), and \code{'auto'} (do the right thing)
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
    if (simplify) unlist(result)
    else result
  }
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
  values <- unname(unlist(dots))
  opts <- read_opts(project = project)
  for (i in seq_along(keys)) {
    opts[[keys[[i]]]] <- values[[i]]
  }
  write.dcf(opts, file = optsPath)
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
           x)
  })
  opts
}
