#' Bundle a Packrat Project
#'
#' Bundle a packrat project, for easy sharing.
#'
#' @param projDir The project directory. Defaults to the currently activate
#'  project. By default, the current project active under \code{packratMode}
#'  is checked.
#' @param file The location to file the bundled file. By default, we write
#'  to a file with name \code{<package>-<date>.zip}.
#' @param include.src Include the packrat sources?
#' @param include.lib Include the packrat private library?
#' @param overwrite Boolean; overwrite the file at \code{file} if it already exists?
#' @param ... Optional arguments passed to \code{\link{tar}}.
#' @export
bundle <- function(projDir = NULL,
                   file = NULL,
                   include.src = TRUE,
                   include.lib = FALSE,
                   overwrite = FALSE,
                   ...) {

  projDir <- getProjectDir(projDir)

  # Make sure this is actually a packrat project
  if (!checkPackified(projDir = projDir, quiet = TRUE)) {
    stop("The project at '", projDir, "' does not appear to be a packrat project.")
  }

  # If file is NULL, write to a local file with the current date
  if (is.null(file)) {
    tarName <- paste(basename(projDir), Sys.Date(), sep = "-")
    tarName <- paste(tarName, ".tar.gz", sep = "")
    bundlesDir <- bundlesDir(projDir)
    if (!file.exists(bundlesDir)) {
      dir.create(bundlesDir)
    }
    file <- file.path(bundlesDir(projDir), tarName)
  }

  file <- normalizePath(file, mustWork = FALSE)

  # Make sure we're in the project dir so relative paths are properly set
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(projDir)

  # Blacklist certain files / folders
  blackList <- c(
    "^\\.Rproj\\.user/"
  )

  # Collect all the files we want to zip up -- this ignores any dot files,
  # or files hidden in . folders
  projectFiles <- list.files(recursive = TRUE, all.files = TRUE)
  for (item in blackList) {
    projectFiles <- grep(item, projectFiles, perl = TRUE, invert = TRUE, value = TRUE)
  }


  # Exclude the packrat folder at this stage -- we re-add the components we
  # need piece by piece
  projectFiles <- projectFiles[
    !startswith(projectFiles, .packrat$packratFolderName)
  ]

  # Make sure we add packrat
  basePackratFiles <- list_files(
    .packrat$packratFolderName,
    all.files = TRUE,
    recursive = FALSE,
    full.names = TRUE
  )

  filesToZip <- c(projectFiles, basePackratFiles)

  # These need to be relative paths
  if (include.src) {
    packratSrc <- list_files(
      file.path(.packrat$packratFolderName, "src"),
      recursive = TRUE,
      full.names = TRUE
    )
    filesToZip <- c(filesToZip, packratSrc)
  }

  if (include.lib) {
    packratLib <- list.files(
      file.path(.packrat$packratFolderName, "lib"),
      recursive = TRUE,
      full.names = TRUE
    )
    filesToZip <- c(filesToZip, packratLib)
  }

  if (file.exists(file) && !overwrite) {
    stop("A file already exists at file location '", file, "'.")
  }

  ## Make sure the base folder name is inheritted from the project name
  setwd("../")
  tar(file, files = file.path(basename(projDir), filesToZip), compression = "gzip", tar = Sys.getenv("TAR"), ...)
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
##' @export
unbundle <- function(bundle, where, ...) {
  where <- normalizePath(where, winslash = "/", mustWork = FALSE)
  untar(bundle, exdir = where, ...)
  message("The packrat project has been unbundled at:\n- \"", file, "\"")
  invisible(file)
}
