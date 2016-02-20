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
##' @param project The project directory.
##' @param ask Boolean, ask before removing the \code{.Renviron} file?
##' @export
migrate <- function(project = ".", ask = TRUE) {

  owd <- getwd()
  on.exit(setwd(owd))
  setwd(project)

  ## Check and see if we're using an older packrat project
  if (!file.exists("packrat.lock")) {
    stop("No file 'packrat.lock', are you sure you are within an old-style packrat project directory?")
  }

  if (file.exists("packrat.lock")) {
    lockFile <- readLines("packrat.lock")
    packrat_format <- grep("^PackratFormat:", lockFile, value = TRUE)
    packrat_format <- gsub("^PackratFormat:[[:space:]]*", "", packrat_format)
    if (packrat_format != "1.1") {
      stop("This function can only migrate from packrat format 1.1 to 1.3")
    }
  }

  ## Create the project local packrat dir
  if (file.exists("packrat")) {
    stop("A local packrat directory already exists!")
  }
  dir.create("packrat")

  ## Clean up on fail if we are generating the new library directory but
  ## copying fails
  needsCleanupOnFail <- !file.exists(libraryRootDir(project))

  ## Copy the library folder
  tryCatch({
    res <- dir_copy("library", libraryRootDir(project))
    if (!all(res)) {
      stop()
    }
    message("- Library folder successfully copied.")
  }, error = function(e) {
    if (needsCleanupOnFail) {
      unlink(libraryRootDir(project), recursive = TRUE)
    }
    stop("Could not successfully copy library")
  })

  ## Copy the packrat sources
  needsCleanupOnFail <- !file.exists(srcDir(project))

  tryCatch({
    res <- dir_copy("packrat.sources", srcDir(project))
    if (!all(res)) stop()
    message("- Source folder successfully copied.")
  }, error = function(e) {
    if (needsCleanupOnFail) {
      unlink(srcDir(project), recursive = TRUE)
    }
    stop("Could not successfully copy packrat sources")
  })

  ## Copy the lock file
  if (!file.copy("packrat.lock", file.path("packrat", "packrat.lock"))) {
    stop("Failed to copy the packrat lock file")
  }

  ## Update the packrat version in the lockfile
  lockFile <- readLines(lockFilePath())
  formatLine <- grep("PackratFormat:", lockFile)
  lockFile[formatLine] <- paste("PackratFormat:", .packrat$packratFormat)
  cat(lockFile, file = lockFilePath(), sep = "\n")
  message("- packrat.lock successfully copied and updated to version ", .packrat$packratFormat)

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

  ## Add the autoloader
  augmentRprofile(getwd())
  message("- .Rprofile successfully augmented")

  ## Add in the init.R script
  file.copy(
    system.file(package = "packrat", "resources", "init.R"),
    file.path("packrat", "init.R")
  )
  message("- packrat/init.R successfully updated")

  ## Update the repositories field in the lockfile
  lf <- readDcf(
    lockFilePath()
  )
  repos <- getOption("repos")
  lf[1, "Repos"] <- paste(
    names(repos),
    repos,
    sep = "=",
    collapse = ",\n"
  )
  write_dcf(lf, lockFilePath())

  ## Initialize packrat options
  initOptions(project = project)
  message("- packrat/packrat.opts successfully initialized")

  ## Clean up the old packrat directories
  unlink("packrat.sources", recursive = TRUE)
  unlink("library", recursive = TRUE)
  unlink("packrat.lock", recursive = TRUE)

  ## Make sure an updated version of packrat is installed in the user library
  oldLibPaths <- getLibPaths()
  on.exit(setLibPaths(oldLibPaths), add = TRUE)
  setLibPaths(libDir())

  if (requireNamespace("devtools")) {
    devtools::install_github("rstudio/packrat")
  }

  message("- Packrat successfully updated. Please restart your R session to continue.")

}
