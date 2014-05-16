packratModeOn <- inPackratMode <- function() {
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

checkPackified <- function(projDir = NULL) {

  projDir <- getProjectDir(projDir)

  packratDir <- packratDir(projDir)
  if (!file.exists(packratDir)) {
    message("The packrat directory does not exist; this project has not been packified.")
    return(FALSE)
  }

  libraryRootDir <- libraryRootDir(projDir)
  if (!file.exists(libraryRootDir)) {
    message("The packrat library does not exist.")
    return(FALSE)
  }

  srcDir <- srcDir(projDir)
  if (!file.exists(srcDir)) {
    message("The packrat sources directory does not exist.")
    return(FALSE)
  }

  lockPath <- lockFilePath(projDir)
  if (!file.exists(lockPath)) {
    message("The packrat lock file does not exist.")
    return(FALSE)
  }

  TRUE
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
  libRoot <- libraryRootDir(projDir)
  localLib <- libDir(appRoot)

  # If we're not in packrat mode, check if we need to do some initialization steps
  if (!packratModeOn()) {

    # Create the private package library if it doesn't already exist
    newLocalLib <- FALSE
    if (!file.exists(localLib)) {
      message("Creating private package library at:\n- \"", localLib, "\"")
      dir.create(localLib, recursive=TRUE)
      newLocalLib <- TRUE
    }

    # If there's a new library (created to make changes to packages loaded in the
    # last R session), remove the old library and replace it with the new one.
    newLibRoot <- newLibraryDir(appRoot)
    if (file.exists(newLibRoot)) {
      message("Applying Packrat library updates ... ", appendLF = FALSE)
      succeeded <- FALSE
      if (file.rename(libRoot, oldLibraryDir(appRoot))) {
        if (file.rename(newLibRoot, libRoot)) {
          succeeded <- TRUE
        } else {
          # Moved the old library out of the way but couldn't move the new
          # in its place; move the old library back
          file.rename(oldLibraryDir(appRoot), libRoot)
        }
      }
      if (succeeded) {
        message("OK")
      } else {
        message("FAILED")
        cat("Packrat was not able to make changes to its local library at\n",
            localLib, ". Check this directory's permissions and run\n",
            "packrat::restore() to try again.\n", sep = "")
      }
    }

    # If the new library temporary folder exists, remove it now so we don't
    # attempt to reapply the same failed changes
    newLibDir <- newLibraryDir(projDir)
    if (file.exists(newLibDir)) {
      unlink(newLibDir, recursive = TRUE)
    }

    oldLibDir <- oldLibraryDir(projDir)
    if (file.exists(oldLibDir)) {
      unlink(oldLibDir, recursive = TRUE)
    }

    # Set the library
    .libPaths(localLib)

    # Record the project directory, in case the user meanders around
    .packrat$projectDir <- appRoot

    # Give the user some visual indication that they're starting a packrat project
    msg <- paste("Packrat mode on. Using library in directory:\n- \"", libDir(appRoot), "\"", sep = "")
    togglePackratMode(msg)
    # setPackratPrompt()

    # Insert hooks to library modifying functions to auto-snapshot on change
    addTaskCallback(snapshotHook, name = "packrat.snapshotHook")

    invisible(.libPaths())

  } else {

    # Disable hooks that were turned on before
    removeTaskCallback("packrat.snapshotHook")

    # Reset the original library paths
    .libPaths(.packrat$origLibPaths)

    # Turn off packrat mode
    msg <- c("Packrat mode off. Resetting library paths to:",
             paste("- \"", .libPaths(), "\"", sep = "")
    )
    togglePackratMode(paste(msg, collapse = "\n"))

    # Reset the prompt
    # options(prompt = .packrat$promptOnLoad)

    # Default back to the current working directory for packrat function calls
    .packrat$projectDir <- NULL

    invisible(.libPaths())

  }

}

setPackratPrompt <- function() {
  oldPromptLeftTrimmed <- gsub("^ *", "", getOption("prompt"))
  options(prompt = paste("pr", oldPromptLeftTrimmed, sep = ""))
}
