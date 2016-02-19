isPackratModeOn <- function(project = NULL) {
  !is.na(Sys.getenv("R_PACKRAT_MODE", unset = NA))
}

setPackratModeEnvironmentVar <- function() {
  Sys.setenv("R_PACKRAT_MODE" = "1")
}

ensurePkgTypeNotBoth <- function() {
  oldPkgType <- getOption("pkgType")
  if (identical(oldPkgType, "both"))
    options(pkgType = .Platform$pkgType)
  oldPkgType
}

beforePackratModeOn <- function(project) {

  # Ensure that we leave packrat mode before transfering
  # to a new project.
  if (isPackratModeOn())
    off(print.banner = FALSE)

  project <- getProjectDir(project)

  ## Check and see if we need to generate default options
  if (!file.exists(packratOptionsFilePath(project = project)))
    initOptions(project = project)

  # Ensure that 'pkgType' is not set to 'both', since its defaults are
  # confusing and set up in such a way that packrat just breaks.
  oldPkgType <- ensurePkgTypeNotBoth()

  # If someone is going from packrat mode on in project A, to packrat mode on
  # in project B, then we only want to update the 'project' in the state --
  # we should just carry forward the other state variables
  if (!isPackratModeOn(project = project)) {
    state <- list(
      origLibPaths = getLibPaths(),
      .Library = .Library,
      .Library.site = .Library.site,
      project = project,
      oldPkgType = oldPkgType
    )
  } else {
    state <- .packrat_mutables$get()
    state$project <- project
  }

  state

}

afterPackratModeOn <- function(project,
                               auto.snapshot,
                               clean.search.path,
                               state,
                               print.banner) {

  project <- getProjectDir(project)
  libRoot <- libraryRootDir(project)
  localLib <- libDir(project)
  dir.create(libRoot, recursive = TRUE, showWarnings = FALSE)

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

  # If the library directory doesn't exist, create it
  if (!file.exists(localLib)) {
    dir.create(localLib, recursive = TRUE)
  }

  # Clean the search path up -- unload libraries that may have been loaded before
  if (clean.search.path) {
    unloadedSearchPath <- cleanSearchPath(lib.loc = getUserLibPaths())
  }

  # Hide the site libraries
  hideSiteLibraries()

  ## Symlink system libraries if possible; otherwise don't touch .Library
  if (symlinkSystemPackages(project = project)) {
    useSymlinkedSystemLibrary(project = project)
  }

  # Refresh the contents of 'lib-ext' if necessary
  symlinkExternalPackages(project = project)

  # Set the library
  if (!file.exists(libExtDir(project)))
    dir.create(libExtDir(project), recursive = TRUE)
  setLibPaths(c(localLib, libExtDir(project)))

  # Load any packages specified in external.packages
  if (isTRUE(opts$load.external.packages.on.startup())) {
    lapply(opts$external.packages(), function(x) {
      library(x, character.only = TRUE, quietly = TRUE)
    })
  }

  # If we unloaded packrat, reload the packrat namespace (don't need to attach)
  # and then reassign the mutables
  # TODO: reframe this logic since, if mutables change from version to version,
  # this could be problematic
  if (clean.search.path && "packrat" %in% unloadedSearchPath$package) {
    try(unloadNamespace("packrat"))
    if (!requireNamespace("packrat", lib.loc = localLib, quietly = TRUE)) {
      # We are forced to initialize the project to install packrat locally
      .__DONT_ENTER_PACKRAT_MODE__. <- TRUE
      source(file.path(project, "packrat", "init.R"), local = TRUE)
      if (!requireNamespace("packrat", quietly = TRUE)) {
        stop("FATAL: could not install a local version of packrat")
      }
    }
  }

  # Give the user some visual indication that they're starting a packrat project
  if (interactive() && print.banner) {
    msg <- paste("Packrat mode on. Using library in directory:\n- \"", prettyLibDir(project), "\"", sep = "")
    message(msg)
  }

  # Insert hooks to library modifying functions to auto.snapshot on change
  if (interactive() && isTRUE(auto.snapshot)) {
    if (file.exists(getPackratDir(project))) {
      addTaskCallback(snapshotHook, name = "packrat.snapshotHook")
    } else {
      warning("this project has not been packified; cannot activate automatic snapshotting")
    }
  }

  # Finally, update state in the current packrat package made available
  # Because we may have reloaded packrat, we make sure that we update the state
  # for whichever packrat we now have as a loaded namespace (which may not be
  # the version of packrat executing this function call!)
  mutables <- get(".packrat_mutables", envir = asNamespace("packrat"))
  mutables$set(state)

  # Set the repositories
  repos <- lockInfo(project = project, property = "repos", fatal = FALSE)
  if (length(repos)) {
    options(repos = repos)
  }

  # Set a secure download method if any of the repos URLs use https and
  # a secure download method has not already been set
  if (any(grepl("^(?:ht|f)tps", repos))) {
    downloadMethod <- getOption("download.file.method")
    if (is.null(downloadMethod) || identical(downloadMethod, "internal")) {
      method <- secureDownloadMethod()
      if (is.null(method)) {
        secureRepos <- grep("^https", repos, value = TRUE)
        pasted <- paste("-", shQuote(secureRepos), collapse = "\n")
        warning("The following repositories require a secure download method, but ",
                "no such method could be selected:\n", pasted)
      }
      options(download.file.method = method)
    }
  }

  # Update settings
  updateSettings(project = project)

  invisible(getLibPaths())

}

setPackratModeOn <- function(project = NULL,
                             auto.snapshot = get_opts("auto.snapshot"),
                             clean.search.path = TRUE,
                             print.banner = TRUE) {

  state <- beforePackratModeOn(project = project)
  setPackratModeEnvironmentVar()
  afterPackratModeOn(project = project,
                     auto.snapshot = auto.snapshot,
                     clean.search.path = clean.search.path,
                     state = state,
                     print.banner = print.banner)

}

setPackratModeOff <- function(project = NULL,
                              print.banner = TRUE) {

  # Restore .Library.site
  if (isPackratModeOn()) {
    restoreSiteLibraries()
    restoreLibrary(".Library")
  }

  Sys.unsetenv("R_PACKRAT_MODE")

  # Disable hooks that were turned on before
  removeTaskCallback("packrat.snapshotHook")

  # Reset the library paths
  libPaths <- .packrat_mutables$get("origLibPaths")
  if (is.null(libPaths))
    libPaths <- getDefaultLibPaths()

  if (length(libPaths))
    setLibPaths(libPaths)

  # Reset 'pkgType'
  oldPkgType <- .packrat_mutables$get("oldPkgType")
  if (!is.null(oldPkgType))
    options(pkgType = oldPkgType)

  # Turn off packrat mode
  if (interactive() && print.banner) {
    msg <- paste(collapse = "\n",
                 c("Packrat mode off. Resetting library paths to:",
                   paste("- \"", getLibPaths(), "\"", sep = "")
                 )
    )
    message(msg)
  }

  # Default back to the current working directory for packrat function calls
  .packrat_mutables$set(project = NULL)
  .packrat_mutables$set(origLibPaths = NULL)

  invisible(getLibPaths())

}

checkPackified <- function(project = NULL, quiet = FALSE) {

  project <- getProjectDir(project)

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
##' @param auto.snapshot Perform automatic, asynchronous snapshots?
##' @param clean.search.path Detach and unload any packages loaded from non-system
##'   libraries before entering packrat mode?
##' @param print.banner Print the packrat banner when entering / exiting packrat mode?
##'   The packrat banner informs you of the new packrat mode state, as well as the library
##'   path in use.
##' @name packrat-mode
##' @rdname packrat-mode
##' @export
packrat_mode <- function(on = NULL,
                         project = NULL,
                         auto.snapshot = get_opts("auto.snapshot"),
                         clean.search.path = TRUE) {

  project <- getProjectDir(project)

  if (is.null(on)) {
    togglePackratMode(project = project,
                      auto.snapshot = auto.snapshot,
                      clean.search.path = clean.search.path)
  } else if (identical(on, TRUE)) {
    setPackratModeOn(project = project,
                     auto.snapshot = auto.snapshot,
                     clean.search.path = clean.search.path)
  } else if (identical(on, FALSE)) {
    setPackratModeOff(project = project)
  } else {
    stop("'on' must be one of TRUE, FALSE or NULL, was '", on, "'")
  }

}

##' @rdname packrat-mode
##' @name packrat-mode
##' @export
on <- function(project = NULL,
               auto.snapshot = get_opts("auto.snapshot"),
               clean.search.path = TRUE,
               print.banner = TRUE) {

  project <- getProjectDir(project)

  # If there is no lockfile already, perform an init
  if (!file.exists(lockFilePath(project = project)))
    return(init(project = project))

  setPackratModeOn(project = project,
                   auto.snapshot = auto.snapshot,
                   clean.search.path = clean.search.path,
                   print.banner = print.banner)

}

##' @rdname packrat-mode
##' @name packrat-mode
##' @export
off <- function(project = NULL, print.banner = TRUE) {
  project <- getProjectDir(project)
  setPackratModeOff(project = project,
                    print.banner = print.banner)
}

togglePackratMode <- function(project, auto.snapshot, clean.search.path) {
  if (isPackratModeOn(project = project)) {
    setPackratModeOff(project)
  } else {
    setPackratModeOn(project = project,
                     auto.snapshot = auto.snapshot,
                     clean.search.path)
  }
}

setPackratPrompt <- function() {
  oldPromptLeftTrimmed <- gsub("^ *", "", getOption("prompt"))
  options(prompt = paste("pr", oldPromptLeftTrimmed, sep = ""))
}
