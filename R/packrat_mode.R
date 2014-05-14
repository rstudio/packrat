packratModeOn <- function() {
  !is.na(Sys.getenv("R_PACKRAT_MODE", unset = NA))
}

togglePackratMode <- function(message = NULL) {

  if (!is.null(message) && interactive()) {
    message(message)
  }

  if (!packratModeOn()) {
    Sys.setenv("R_PACKRAT_MODE" = "1")
  } else {
    Sys.unsetenv("R_PACKRAT_MODE")
  }

}

##' Packrat Mode
##'
##' Use this function to switch \code{packrat} mode on and off. When within
##' \code{packrat} mode, the library is set to use a local packrat library
##' within \code{file.path(projDir, ".packrat/lib")}.
##'
##' @param projDir The directory in which packrat mode is launched -- this is
##'   where local libraries will be used and updated.
##' @export
packrat_mode <- function(projDir = ".") {

  appRoot <- normalizePath(projDir, winslash='/')
  localLib <- libDir(appRoot)

  # If we're not in packrat mode, check if we need to do some initialization steps
  if (!packratModeOn()) {

    # Create the private package library if it doesn't already exist
    newLocalLib <- FALSE
    if (!file.exists(localLib)) {
      message("Creating private package library at:\n> \"", localLib, "\"")
      dir.create(localLib, recursive=TRUE)
      newLocalLib <- TRUE
    }

    # Insert hooks to library modifying functions to auto-snapshot on change
    addTaskCallback(snapshotHook, name = "snapshotHook")

    # Set the library
    .libPaths(localLib)

    # Record the project directory, in case the user meanders around
    .packrat$projectDir <- appRoot

    # Give the user some visual indication that they're starting a packrat project
    msg <- paste0("Packrat mode initialized in directory:\n- \"", appRoot, "\"")
    togglePackratMode(msg)
    options(prompt = "pr> ")
    invisible(.libPaths())

  } else {

    # Disable hooks that were turned on before
    removeTaskCallback("snapshotHook")

    # Turn off packrat mode
    msg <- "Packrat mode off."
    togglePackratMode(msg)
    options(prompt = .packrat$promptOnLoad)

    # NULLify the project directory
    .packrat$projectDir <- NULL

    # Remove the local library
    .libPaths(.packrat$origLibPaths)
    invisible(.libPaths())

  }

}
