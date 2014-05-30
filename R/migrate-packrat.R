##' Migrate to Packrat Mode
##'
##' Migrate a project using the older version of packrat + packrat structure
##' to the newer version.
##'
##' This function will:
##'
##' \enumerate{
##' \item Move the \code{library} folder to the \code{packrat/lib} folder,
##' \item Move the \code{packrat.sources} folder to the \code{packrat/src} folder,
##' \item Move the \code{packrat.lock} file to \code{packrat/packrat.lock},
##' \item Remove the \code{.Renviron} file as it is no longer needed,
##' \item Update the package \code{.Rprofile}.
##' }
##'
##' @param projDir The project directory.
##' @param ask Boolean, ask before removing the \code{.Renviron} file?
migrate <- function(projDir = ".", ask = TRUE) {

  owd <- getwd()
  on.exit(setwd(owd))
  setwd(projDir)

  ## Check and see if we're using an older packrat project
  if (!file.exists("packrat.lock")) {
    stop("No file 'packrat.lock', are you sure you are within an old-style packrat project directory?")
  }

  if (file.exists("packrat.lock")) {
    lockFile <- readLockFile(file = "packrat.lock")
    if (lockFile$packrat_format != "1.1") {
      stop("This function can only migrate from packrat format 1.1 to 1.2")
    }
  }

  ## Create the project local packrat dir
  if (file.exists(.packrat$packratFolderName)) {
    stop("A local packrat directory already exists!")
  }
  dir.create(.packrat$packratFolderName)

  ## Clean up on fail if we are generating the new library directory but
  ## copying fails
  needsCleanupOnFail <- !file.exists(libraryRootDir(projDir))

  ## Copy the library folder
  tryCatch({
    res <- dir_copy("library", libraryRootDir(projDir))
    if (!all(res)) {
      stop()
    }
    message("- Library folder successfully copied.")
  }, error = function(e) {
    if (needsCleanupOnFail) {
      unlink(libraryRootDir(projDir), recursive = TRUE)
    }
    stop("Could not successfully copy library")
  })

  ## Copy the packrat sources
  needsCleanupOnFail <- !file.exists(srcDir(projDir))

  tryCatch({
    res <- dir_copy("packrat.sources", srcDir(projDir))
    if (!all(res)) stop()
    message("- Source folder successfully copied.")
  }, error = function(e) {
    if (needsCleanupOnFail) {
      unlink(srcDir(projDir), recursive = TRUE)
    }
    stop("Could not successfully copy packrat sources")
  })

  ## Copy the lock file
  if (!file.copy("packrat.lock", file.path(.packrat$packratFolderName, "packrat.lock"))) {
    stop("Failed to copy the packrat lock file")
  }

  ## Update the packrat version in the lockfile
  lockFile <- readLines(lockFilePath())
  formatLine <- grep("PackratFormat:", lockFile)
  lockFile[formatLine] <- paste("PackratFormat:", .packrat$packratFormat)
  cat(lockFile, file = lockFilePath(), sep = "\n")
  message("- packrat.lock succesffuly copied and updated to version 1.2")

  ## Remove the .Renviron file
  if (file.exists(".Renviron")) {
    response <- readline("Do you want to remove the project .Renviron file (recommended)? (Y/n): ")
    if (substr(tolower(response), 1, 1) == "y") {
      file.remove(".Renviron")
      message("- .Renviron successfully removed")
    }
  }

  ## Update the .Rprofile
  if (file.exists(".Rprofile")) {
    .Rprofile <- readLines(".Rprofile")

    ## Remove the prior packrat autoloader
    oldStart <- which(.Rprofile == "# -- BEGIN PACKRAT --")
    oldEnd <- which(.Rprofile == "# -- END PACKRAT --")
    if (length(oldStart) == 1 && length(oldEnd) == 1) {
      .Rprofile <- .Rprofile[-c(oldStart:oldEnd)]
      cat(.Rprofile, file = ".Rprofile", sep = "\n")
    }

  }

  ## Now, update it with new packrat
  augmentRprofile(getwd())
  message("- .Rprofile successfully augmented")

  ## Clean up the old packrat directories
  unlink("packrat.sources", recursive = TRUE)
  unlink("library", recursive = TRUE)
  unlink("packrat.lock", recursive = TRUE)

  ## Make sure an updated version of packrat is installed in the user library
  oldLibPaths <- .libPaths()
  on.exit(.libPaths(oldLibPaths), add = TRUE)
  .libPaths(libDir())

  ## TODO -- update when merged to master -- or install a specific tag?
  installGithub("rstudio/packrat", ref = "feature/packrat-mode")

  message("- Packrat successfully updated. Please restart your R session to continue.")

}
