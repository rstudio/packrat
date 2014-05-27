isPackratModeOn <- function(projDir = NULL) {
  file.exists(packratModeFilePath(projDir))
}

setPackratModeOn <- function(projDir = NULL, bootstrap = TRUE, autoSnapshot = TRUE) {

  projDir <- getProjectDir(projDir)
  libRoot <- libraryRootDir(projDir)
  localLib <- libDir(projDir)
  dir.create(libRoot, recursive = TRUE, showWarnings = FALSE)
  file.create(packratModeFilePath(projDir))

  # If there's a new library (created to make changes to packages loaded in the
  # last R session), remove the old library and replace it with the new one.
  newLibRoot <- newLibraryDir(projDir)
  if (file.exists(newLibRoot)) {
    message("Applying Packrat library updates ... ", appendLF = FALSE)
    succeeded <- FALSE
    if (file.rename(libRoot, oldLibraryDir(projDir))) {
      if (file.rename(newLibRoot, libRoot)) {
        succeeded <- TRUE
      } else {
        # Moved the old library out of the way but couldn't move the new
        # in its place; move the old library back
        file.rename(oldLibraryDir(projDir), libRoot)
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

  # Try to bootstrap the directory if there is no packrat directory
  if (bootstrap && !file.exists(getPackratDir(projDir))) {
    bootstrap(projDir = projDir)
  }

  # If the library directory doesn't exist, create it
  if (!file.exists(localLib)) {
    dir.create(localLib, recursive = TRUE)
  }

  # Record the original library, directory, etc.
  .packrat_mutables$set(origLibPaths = .libPaths())
  .packrat_mutables$set(projDir = projDir)

  # Set the library
  .libPaths(localLib)

  # Give the user some visual indication that they're starting a packrat project
  msg <- paste("Packrat mode on. Using library in directory:\n- \"", libDir(projDir), "\"", sep = "")
  message(msg)

  # Insert hooks to library modifying functions to autoSnapshot on change
  if (autoSnapshot) {
    if (file.exists(getPackratDir(projDir))) {
      addTaskCallback(snapshotHook, name = "packrat.snapshotHook")
    } else {
      warning("this project has not been packified; cannot activate automatic snapshotting")
    }
  }

  invisible(.libPaths())

}

setPackratModeOff <- function(projDir = NULL) {

  path <- packratModeFilePath(projDir)
  if (file.exists(path)) file.remove(path)

  # Disable hooks that were turned on before
  removeTaskCallback("packrat.snapshotHook")

  # Reset the library paths to what one gets in a 'clean' session
  #   cmd <- paste(
  #     shQuote(file.path(R.home("bin"), "R")),
  #     "--vanilla",
  #     "--slave",
  #     "-e 'cat(.libPaths(), sep = \"\\\\n\")'"
  #   )
  #   libPaths <- system(cmd, intern = TRUE)
  libPaths <- .packrat_mutables$get("origLibPaths")
  if (!is.null(libPaths)) {
    .libPaths(libPaths)
  }

  # Turn off packrat mode
  msg <- paste(collapse = "\n",
               c("Packrat mode off. Resetting library paths to:",
                 paste("- \"", .libPaths(), "\"", sep = "")
               )
  )
  message(msg)

  # Reset the prompt
  # options(prompt = .packrat$promptOnLoad)

  # Default back to the current working directory for packrat function calls
  .packrat_mutables$set(projectDir = NULL)
  .packrat_mutables$set(origLibPaths = NULL)

  invisible(.libPaths())

}

checkPackified <- function(projDir = NULL) {

  projDir <- getProjectDir(projDir)
  packratDir <- getPackratDir(projDir)

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
##' Use these functions to switch \code{packrat} mode on and off. When within
##' \code{packrat} mode, the library is set to use a local packrat library
##' within the current project.
##'
##' @param projDir The directory in which packrat mode is launched -- this is
##'   where local libraries will be used and updated.
##' @param autoSnapshot Whether or not we should use automatic snapshotting.
##' @param bootstrap Whether or not we should try to bootstrap a project directory
##'   that has not yet been packified.
##' @rdname packratMode
##' @export
packratOn <- function(projDir = ".", autoSnapshot = TRUE, bootstrap = FALSE) {
  projDir <- normalizePath(projDir, winslash='/')
  setPackratModeOn(projDir)
}

##' @rdname packratMode
##' @export
packratOff <- function(projDir = NULL) {
  projDir <- getProjectDir(projDir)
  setPackratModeOff(projDir)
}

##' @rdname packratMode
##' @export
packratMode <- function(projDir = NULL, autoSnapshot = TRUE, bootstrap = FALSE) {
  if (isPackratModeOn()) {
    packratOff(projDir)
  } else {
    packratOn(projDir, autoSnapshot, bootstrap)
  }
}

setPackratPrompt <- function() {
  oldPromptLeftTrimmed <- gsub("^ *", "", getOption("prompt"))
  options(prompt = paste("pr", oldPromptLeftTrimmed, sep = ""))
}
