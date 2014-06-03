isPackratModeOn <- function(project = NULL) {
  !is.na(Sys.getenv("R_PACKRAT_MODE", unset = NA))
}

setPackratModeOn <- function(project = NULL,
                             bootstrap = TRUE,
                             auto.snapshot = TRUE,
                             clean.search.path = TRUE) {

  project <- getProjectDir(project)
  libRoot <- libraryRootDir(project)
  localLib <- libDir(project)
  dir.create(libRoot, recursive = TRUE, showWarnings = FALSE)

  ## The item that denotes whether we're in packrat mode or not
  Sys.setenv("R_PACKRAT_MODE" = "1")

  # Override auto.snapshot if running under RStudio, as it has its own packrat
  # file handlers
  if (!is.na(Sys.getenv("RSTUDIO", unset = NA))) {
    auto.snapshot <- FALSE
  }

  # If snapshot.lock exists, assume it's an orphan of an earlier, crashed
  # R process -- remove it
  if (file.exists(snapshotLockFilePath(project))) {
    unlink(snapshotLockFilePath(project))
  }

  # If there's a new library (created to make changes to packages loaded in the
  # last R session), remove the old library and replace it with the new one.
  newLibRoot <- newLibraryDir(project)
  if (file.exists(newLibRoot)) {
    message("Applying Packrat library updates ... ", appendLF = FALSE)
    succeeded <- FALSE
    if (file.rename(libRoot, oldLibraryDir(project))) {
      if (file.rename(newLibRoot, libRoot)) {
        succeeded <- TRUE
      } else {
        # Moved the old library out of the way but couldn't move the new
        # in its place; move the old library back
        file.rename(oldLibraryDir(project), libRoot)
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
  newLibDir <- newLibraryDir(project)
  if (file.exists(newLibDir)) {
    unlink(newLibDir, recursive = TRUE)
  }

  oldLibDir <- oldLibraryDir(project)
  if (file.exists(oldLibDir)) {
    unlink(oldLibDir, recursive = TRUE)
  }

  # Try to bootstrap the directory if there is no packrat directory
  if (bootstrap && !file.exists(getPackratDir(project))) {
    bootstrap(project = project)
  }

  # If the library directory doesn't exist, create it
  if (!file.exists(localLib)) {
    dir.create(localLib, recursive = TRUE)
  }

  # Record the original library, directory, etc.
  .packrat_mutables$set(origLibPaths = .libPaths())
  .packrat_mutables$set(project = project)

  # Clean the search path up -- unload libraries that may have been loaded before
  if (clean.search.path) {
    cleanSearchPath(lib.loc = .libPaths())
  }

  # Hide the site libraries
  hideSiteLibraries()

  # Set the library
  .libPaths(localLib)

  # Give the user some visual indication that they're starting a packrat project
  if (interactive()) {
    msg <- paste("Packrat mode on. Using library in directory:\n- \"", libDir(project), "\"", sep = "")
    message(msg)
  }

  # Insert hooks to library modifying functions to auto.snapshot on change
  if (auto.snapshot) {
    if (file.exists(getPackratDir(project))) {
      addTaskCallback(snapshotHook, name = "packrat.snapshotHook")
    } else {
      warning("this project has not been packified; cannot activate automatic snapshotting")
    }
  }

  invisible(.libPaths())

}

setPackratModeOff <- function(project = NULL) {

  path <- packratModeFilePath(project)
  Sys.unsetenv("R_PACKRAT_MODE")

  # Disable hooks that were turned on before
  removeTaskCallback("packrat.snapshotHook")

  # Restore .Library.site
  restoreSiteLibraries()

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
  if (interactive()) {
    msg <- paste(collapse = "\n",
                 c("Packrat mode off. Resetting library paths to:",
                   paste("- \"", .libPaths(), "\"", sep = "")
                 )
    )
    message(msg)
  }

  # Reset the prompt
  # options(prompt = .packrat$promptOnLoad)

  # Default back to the current working directory for packrat function calls
  .packrat_mutables$set(project = NULL)
  .packrat_mutables$set(origLibPaths = NULL)

  invisible(.libPaths())

}

checkPackified <- function(project = NULL, quiet = FALSE) {

  project <- getProjectDir(project)
  packratDir <- getPackratDir(project)

  if (!file.exists(packratDir)) {
    if (!quiet) message("The packrat directory does not exist; this project has not been packified.")
    return(FALSE)
  }

  libraryRootDir <- libraryRootDir(project)
  if (!file.exists(libraryRootDir)) {
    if (!quiet) message("The packrat library does not exist.")
    return(FALSE)
  }

  srcDir <- srcDir(project)
  if (!file.exists(srcDir)) {
    if (!quiet) message("The packrat sources directory does not exist.")
    return(FALSE)
  }

  lockPath <- lockFilePath(project)
  if (!file.exists(lockPath)) {
    if (!quiet) message("The packrat lock file does not exist.")
    return(FALSE)
  }

  TRUE
}

##' Packrat Mode
##'
##' Use these functions to switch \code{packrat} mode on and off. When within
##' \code{packrat} mode, the \R session will use the private library generated
##' for the current project.
##'
##' @param on Turn packrat mode on (\code{TRUE}) or off (\code{FALSE}). If omitted, packrat mode
##'   will be toggled.
##' @param project The directory in which packrat mode is launched -- this is
##'   where local libraries will be used and updated.
##' @param bootstrap Bootstrap a project that has not yet been packified?
##' @param auto.snapshot Perform automatic, asynchronous snapshots?
##' @param clean.search.path Detach and unload any packages loaded from non-system
##'   libraries before entering packrat mode?
##' @name packrat-mode
##' @rdname packrat-mode
##' @export
packrat_mode <- function(on = NULL,
                         project = NULL,
                         bootstrap = FALSE,
                         auto.snapshot = TRUE,
                         clean.search.path = TRUE) {

  project <- getProjectDir(project)

  if (is.null(on)) {
    togglePackratMode(project = project,
                      bootstrap = bootstrap,
                      auto.snapshot = auto.snapshot,
                      clean.search.path = clean.search.path)
  } else if (identical(on, TRUE)) {
    setPackratModeOn(project = project,
                     bootstrap = bootstrap,
                     auto.snapshot = auto.snapshot,
                     clean.search.path = clean.search.path)
  } else if (identical(on, FALSE)) {
    setPackratModeOff(project = project)
  } else {
    stop("'on' must be one of TRUE, FALSE or NULL, was '", on, "'")
  }

}

togglePackratMode <- function(project, bootstrap, auto.snapshot, clean.search.path) {
  if (isPackratModeOn(project = project)) {
    setPackratModeOff(project)
  } else {
    setPackratModeOn(project = project,
                     bootstrap = bootstrap,
                     auto.snapshot = auto.snapshot,
                     clean.search.path)
  }
}

setPackratPrompt <- function() {
  oldPromptLeftTrimmed <- gsub("^ *", "", getOption("prompt"))
  options(prompt = paste("pr", oldPromptLeftTrimmed, sep = ""))
}
