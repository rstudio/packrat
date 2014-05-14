packratModeOn <- function() {
  !is.na(Sys.getenv("R_PACKRAT_MODE", unset = NA))
}

togglePackratMode <- function(message = NULL) {

  if (!is.null(message)) {
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

    .libPaths(localLib)

    # Give the user some visual indication that they're starting a packrat project
    msg <- paste0("Packrat mode initialized in directory: '", appRoot, "'.")
    togglePackratMode(msg)
    options(prompt = "pr> ")
    invisible(.libPaths())

  } else {

    # Turn off packrat mode
    msg <- "Packrat mode off."
    togglePackratMode(msg)
    options(prompt = .packrat$promptOnLoad)

    # Remove the local library
    .libPaths(.packrat$origLibPaths)
    invisible(.libPaths())

  }

}
