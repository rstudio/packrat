#' Bundle a Packrat Project
#'
#' Bundle a packrat project, for easy sharing.
#'
#' @param project The project directory. Defaults to the currently activate
#'  project. By default, the current project active under \code{packratMode}
#'  is checked.
#' @param file The location to file the bundled file. By default, we write
#'  to a file with name \code{<package>-<date>.zip}.
#' @param include.src Include the packrat sources?
#' @param include.lib Include the packrat private library?
#' @param overwrite Boolean; overwrite the file at \code{file} if it already exists?
#' @param ... Optional arguments passed to \code{\link{tar}}.
#' @export
bundle <- function(project = NULL,
                   file = NULL,
                   include.src = TRUE,
                   include.lib = FALSE,
                   overwrite = FALSE,
                   ...) {

  project <- getProjectDir(project)
  stopIfNotPackified(project)

  # If file is NULL, write to a local file with the current date
  if (is.null(file)) {
    tarName <- paste(basename(project), Sys.Date(), sep = "-")
    tarName <- paste(tarName, ".tar.gz", sep = "")
    bundlesDir <- bundlesDir(project)
    if (!file.exists(bundlesDir)) {
      dir.create(bundlesDir, recursive = TRUE)
    }
    file <- file.path(bundlesDir(project), tarName)
  }

  file <- normalizePath(file, mustWork = FALSE)
  if (file.exists(file) && !overwrite) {
    stop("A file already exists at file location '", file, "'.")
  }

  # Regex negation patterns that we use to selectively leave some items out
  pattern <- c(
    "^(?!\\.Rproj\\.user)",
    if (!include.src) "^(?!packrat/src/)",
    if (!include.lib) "^(?!packrat/lib.*)"
  )

  ## Make sure the base folder name is inheritted from the project name
  ##
  ## The internal version of 'tar' used by R on Windows fails if one tries to include
  ## a file in a sub-directory, without actually including that subdirectory.
  ## To work around this, we are forced to copy the files we want to tar to
  ## a temporary directory, and then tar that. Sigh...
  dir_copy(
    from = getwd(),
    to = file.path(tempdir(), basename(project)),
    pattern = pattern,
    overwrite = TRUE
  )

  ## Now bundle up that copied directory, from the tempdir path
  setwd(tempdir())
  result <- tar(
    tarfile = file,
    files = basename(project),
    compression = "gzip",
    tar = Sys.getenv("TAR"),
    ...
  )

  if (result != 0) {
    stop("Failed to bundle the packrat project.")
  }
  message("The packrat project has been bundled at:\n- \"", file, "\"")
  invisible(file)
}

##' Unbundle a Packrat Project
##'
##' Unbundle a previously \code{\link{bundle}}d project.
##'
##' @param bundle Path to the bundled file.
##' @param where The directory where we will unbundle the project.
##' @param ... Optional arguments passed to \code{\link{tar}}.
##' @param restore Boolean; should we \code{\link{restore}} the library
##'   after \code{unbundle}-ing the project?
##' @export
unbundle <- function(bundle, where, ..., restore = TRUE) {

  bundle <- normalizePath(bundle, winslash = "/", mustWork = TRUE)
  if (!file.exists(where) && is_dir(where)) {
    dir.create(where, recursive = TRUE)
  }
  where <- normalizePath(where, winslash = "/", mustWork = TRUE)

  ## Get the list of files in the output directory -- we diff against it
  ## to figure out what the top-level directory name is
  owd <- getwd()
  on.exit(setwd(owd))

  setwd(where)
  whereFiles <- list.files()
  message("- Untarring '", basename(bundle), "' in directory '", where, "'...")
  untar(bundle, exdir = where, ...)
  dirName <- normalizePath(setdiff(list.files(), whereFiles), winslash = "/", mustWork = TRUE)

  if (restore) {
    setwd(dirName)
    if (length(dirName) != 1) {
      stop("Couldn't infer top-level directory name; cannot perform automatic restore")
    }
    ## Ensure the (empty) library directory is present before restoring
    dir.create(libDir(getwd()), recursive = TRUE, showWarnings = FALSE)
    message("- Restoring project library...")
    restore(project = getwd(), restart = FALSE)
    message("Done! The project has been unbundled and restored at:\n- \"", dirName, "\"")
  } else {
    message("Done! The packrat project has been unbundled at:\n- \"", dirName, "\"")
  }

  invisible(dirName)

}
