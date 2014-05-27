#' Bundle a Packrat Project
#'
#' This function bundles a \code{packrat} project.
#'
#' @param projDir The project directory. Defaults to the currently activate
#'  project. By default, the current project active under \code{packratMode}
#'  is checked.
#' @param file The location to file the bundled file. By default, we write
#'  to a file with name \code{<package>-<date>.zip}.
#' @param includeSrc Include the packrat sources?
#' @param includeLib Include the packrat private library?
#' @param overwrite Boolean; overwrite the file at \code{file} if it already exists?
#' @param ... Optional arguments passed to \code{\link{zip}}.
#' @export
bundle <- function(projDir = NULL,
                   file = NULL,
                   includeSrc = TRUE,
                   includeLib = FALSE,
                   overwrite = FALSE,
                   ...) {

  projDir <- getProjectDir(projDir)

  # If file is NULL, write to a local file with the current date
  if (is.null(file)) {
    zipName <- paste(basename(projDir), Sys.Date(), sep = "-")
    zipName <- paste(zipName, ".zip", sep = "")
    bundlesDir <- bundlesDir(projDir)
    if (!file.exists(bundlesDir)) {
      dir.create(bundlesDir)
    }
    file <- file.path(bundlesDir(projDir), zipName)
  }

  file <- normalizePath(file, mustWork = FALSE)

  # Make sure we're in the project dir so relative paths are properly set
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(projDir)

  # Collect all the files we want to zip up -- this ignores any dot files,
  # or files hidden in . folders
  project_files <- list.files(recursive = TRUE)

  # Exclude the packrat folder at this stage -- we re-add the components we
  # need piece by piece
  project_files <- project_files[
    !startswith(project_files, .packrat$packratFolderName)
  ]

  # Make sure we add the .Rprofile file if it exists
  .Rprofile <- file.path(".Rprofile")
  if (file.exists(.Rprofile)) {
    project_files <- c(project_files, .Rprofile)
  }

  # Make sure we add packrat
  packrat_files_base <- list_files(
    .packrat$packratFolderName,
    all.files = TRUE,
    recursive = FALSE,
    full.names = TRUE
  )

  files_to_zip <- c(project_files, packrat_files_base)

  # These need to be relative paths
  if (includeSrc) {
    packrat_src <- list_files(
      file.path(.packrat$packratFolderName, "src"),
      recursive = TRUE,
      full.names = TRUE
    )
    files_to_zip <- c(files_to_zip, packrat_src)
  }

  if (includeLib) {
    packrat_lib <- list.files(
      file.path(.packrat$packratFolderName, "lib"),
      recursive = TRUE,
      full.names = TRUE
    )
    files_to_zip <- c(files_to_zip, packrat_lib)
  }

  if (file.exists(file) && !overwrite) {
    stop("A file already exists at file location '", file, "'.")
  }

  zip(file, files = files_to_zip, ...)
  message("The packrat project has been bundled at:\n- \"", file, "\"")

}
