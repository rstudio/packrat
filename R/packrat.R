#' Packrat: Reproducible dependency management
#'
#' Packrat is a tool for managing the \R packages your project depends on in
#' an isolated, portable, and reproducible way.
#'
#' Use packrat to make your \R projects more:
#'
#' \itemize{
#' \item \strong{Isolated}: Installing a new or updated package for one project
#' won't break your other projects, and vice versa. That's because packrat gives
#' each project its own private package library.
#' \item \strong{Portable}: Easily transport your projects from one computer to
#' another, even across different platforms. Packrat makes it easy to install the
#' packages your project depends on.
#' \item \strong{Reproducible}: Packrat records the exact package versions you
#' depend on, and ensures those exact versions are the ones that get installed
#' wherever you go.}
#'
#' Use \code{\link{init}} to create a new packrat project,
#' \code{\link{snapshot}} to record changes to your project's library, and
#' \code{\link{restore}} to recreate your library the way it was the last time you
#' (or anyone!) took a snapshot.
#'
#' Using these simple functions and sharing packrat's files lets you collaborate
#' in a shared, consistent environment with others as your project grows and
#' changes, and provides an easy way to share your results when you're done.
#'
#' @section Anatomy of a packrat project:
#'
#' A packrat project contains a few extra files and directories. The
#' \code{\link{init}} function creates these files for you, if they don't
#' already exist.
#'
#' \describe{
#'   \item{\code{packrat/lib/}}{Private package library for this project.}
#'   \item{\code{packrat/src/}}{Source packages of all the dependencies that
#'packrat has been made aware of.}
#'
#'   \item{\code{packrat/packrat.lock}}{Lists the precise package versions that were used
#' to satisfy dependencies, including dependencies of dependencies. (This file
#' should never be edited by hand!)}
#'
#'   \item{\code{.Rprofile}}{Directs \R to use the private package
#' library (when it is started from the project directory).}
#' }
#'
#' @section Using packrat with version control:
#'
#' Packrat is designed to work hand in hand with Git, Subversion, or any other
#' version control system. Be sure to check in the \code{.Rprofile},
#' \code{packrat.lock} files, and everything under
#' \code{packrat/src/}. You can tell your VCS to ignore \code{packrat/lib/} (or
#' feel free to check it in if you don't mind taking up some extra space in your
#' repository).
#'
#' @examples
#' \dontrun{
#' # Create a new packrat project from an existing directory of \R code
#' init()
#'
#' # Install a package and take a snapshot of the new state of the library
#' install.packages("TTR")
#' snapshot()
#'
#' # Accidentally remove a package and restore to add it back
#' remove.packages("TTR")
#' restore()
#' }
#'
#' @docType package
#' @name packrat
#' @import utils
#' @author RStudio, Inc.
NULL

#' Initialize Packrat on a new or existing \R project
#'
#' Given a project directory, makes a new packrat project in the directory.
#'
#' \code{init} works as follows:
#'
#' \enumerate{
#'
#' \item Application dependencies are computed by examining the \R code
#' throughout the project for \code{library} and \code{require} calls.
#'
#' \item A snapshot is taken of the version of each package currently used by
#' the project as described in \code{\link{snapshot}}, and each package's
#' sources are downloaded.
#'
#' \item A private library is created in the directory.
#'
#' \item The snapshot is applied to the directory as described in
#' \code{\link{restore}}. } When \code{init} is finished, all the packages
#' on which the project depends are installed in a new, private library located
#' inside the project directory.
#'
#' \strong{You must restart your \R session in the given project directory after
#' running \code{init} in order for the changes to take effect!}
#'
#' When \R is started in the directory, it will use the new, private library.
#' Calls to \code{\link{require}} and \code{\link{library}} will load packages
#' from the private library (except for 'base' or 'recommended' \R packages,
#' which are found in the system library), and functions such as \code{\link{install.packages}}
#' will modify that private library. You can sync this private library with
#' packrat using \code{\link{snapshot}} and \code{\link{restore}}.
#'
#' @param project The directory that contains the \R project.
#' @param options An \R \code{list} of options, as specified in
#'   \code{\link{packrat-options}}.
#' @param enter Boolean, enter packrat mode for this project after finishing a init?
#' @param restart If \code{TRUE}, restart the R session after init.
#'
#' @note
#'
#' The \code{restart} parameter will only result in a restart of R when the
#' R environment packrat is running within makes available a restart function
#' via \code{getOption("restart")}.
#'
#' @seealso \link{packrat} for a description of the files created by
#'   \code{init}.
#' @examples \dontrun{
#'
#' ## initialize a project using a local repository of packages
#' packrat::init(options = list(local.repos = "~/projects/R"))
#'
#' }
#' @export
init <- function(project = '.',
                 options = NULL,
                 enter = TRUE,
                 restart = enter) {

  opts <- get_opts(project = project)
  if (is.null(opts))
    opts <- default_opts()

  # Read custom Packrat options and apply them
  customDefaultOptions <- getOption("packrat.default.project.options")
  if (!is.null(customDefaultOptions)) {

    # Validate the options (will stop on failure)
    validateOptions(customDefaultOptions)

    # Set the options
    for (i in seq_along(customDefaultOptions)) {
      name <- names(customDefaultOptions)[[i]]
      opts[[name]] <- customDefaultOptions[[name]]
    }
  }

  # NOTE: Explicitly set options should override default options set by
  # the user
  if (!is.null(options)) {
    for (i in seq_along(options)) {
      name <- names(options)[[i]]
      opts[[name]] <- options[[name]]
    }
  }

  ## Get the initial directory structure, so we can rewind if necessary
  project <- normalizePath(project,
                           winslash = '/',
                           mustWork = TRUE)
  message("Initializing packrat project in directory:\n- ", surround(prettyDir(project), "\""))

  ## A set of files that packrat might generate as part of init -- we
  ## enumerate them here to assist with later cleanup
  prFiles <- c(
    file.path(project, ".gitignore"),
    file.path(project, ".Rprofile"),
    file.path(project, "packrat"),
    file.path(project, "packrat", "lib"),
    file.path(project, "packrat", "lib-R"),
    file.path(project, "packrat", "src"),
    file.path(project, "packrat", "packrat.lock"),
    file.path(project, "packrat", "packrat.opts")
  )

  priorStructure <- setNames(
    file.exists(prFiles),
    prFiles
  )

  withCallingHandlers(

    expr = {

      ## Force packrat mode off
      if (isPackratModeOn())
        off()

      ## We always re-packify so that the current version of packrat present can
      ## insert the appropriate auto-loaders
      packify(project = project, quiet = TRUE)

      ## Make sure the .Rprofile is up to date
      augmentRprofile(project)
      options <- initOptions(project, opts) ## writes out packrat.opts and returns generated list

      # Take a snapshot
      snapshotImpl(project,
                   available.packages(contrib.url(activeRepos(project))),
                   lib.loc = NULL,
                   ignore.stale = TRUE,
                   fallback.ok = TRUE)

      # Use the lockfile to copy sources and install packages to the library
      restore(project, overwrite.dirty = TRUE, restart = FALSE)

      # Copy init.R so a user can 'start from zero' with a project
      file.copy(
        instInitFilePath(),
        file.path(project, "packrat", "init.R")
      )

      # Update project settings -- this also involves updating the .gitignore,
      # etc
      updateSettings(project, options)

      ## Symlink system libraries always
      symlinkSystemPackages(project = project)

      message("Initialization complete!")

      if (enter) {

        setwd(project)

        # Restart R if the environment is capable of it (otherwise enter packrat mode)
        if (!restart || !attemptRestart())
          on(project = project, clean.search.path = TRUE)
      }

      invisible()

    }, ## expr

    error = function(e) {

      # Undo any changes to the directory that did not exist previously
      for (i in seq_along(priorStructure)) {
        file <- names(priorStructure)[[i]]
        fileExistedBefore <- priorStructure[[i]]
        fileExistsNow <- file.exists(file)
        if (!fileExistedBefore && fileExistsNow) {
          unlink(file, recursive = TRUE)
        }
      }

    } ## error

  )

}

#' Apply the most recent snapshot to the library
#'
#' Applies the most recent snapshot to the project's private library.
#'
#' \code{restore} works by adding, removing, and changing packages so that the
#' set of installed packages and their versions matches the snapshot exactly.
#'
#' There are three common use cases for \code{restore}:
#' \itemize{
#'   \item \strong{Hydrate}: Use \code{restore} after copying a project to a new
#' machine to populate the library on that machine.
#'
#'   \item \strong{Sync}: Use \code{restore} to apply library changes made by a
#' collaborator to your own library. (In general, you want to run \code{restore}
#' whenever you pick up a change to \code{packrat.lock})
#'
#'   \item \strong{Rollback}: Use \code{restore} to undo accidental changes made
#' to the library since the last snapshot.
#' }
#'
#' \code{restore} cannot make changes to packages that are currently loaded. If
#' changes are necessary to currently loaded packages, you will need to restart
#' \R to apply the changes (\code{restore} will let you know when this is
#' necessary). It is recommended that you do this as soon as possible, because
#' any library changes made between running \code{restore} and restarting \R will
#' be lost.
#'
#' @note
#' \code{restore} can be destructive; it will remove packages that were not in
#' the snapshot, and it will replace newer packages with older versions if
#' that's what the snapshot indicates. \code{restore} will warn you before
#' attempting to remove or downgrade a package (if \code{prompt} is
#' \code{TRUE}), but will always perform upgrades and new installations without
#' prompting.
#'
#' \code{restore} works only on the private package library created by packrat;
#' if you have other libraries on your path, they will be unaffected.
#'
#' The \code{restart} parameter will only result in a restart of R when the
#' R environment packrat is running within makes available a restart function
#' via \code{getOption("restart")}.
#'
#' @param project The project directory. When in packrat mode, if this is \code{NULL},
#' then the directory associated with the current packrat project is used. Otherwise,
#' the project directory specified is used.
#' @param overwrite.dirty A dirty package is one that has been changed since the
#' last snapshot or restore. Packrat will leave these alone by default. If you
#' want to guarantee that \code{restore} will put you in the exact state
#' represented by the snapshot being applied, use \code{overwrite.dirty = TRUE}.
#' @param prompt \code{TRUE} to prompt before performing potentially destructive
#' changes (package removals or downgrades); \code{FALSE} to perform these
#' operations without confirmation.
#' @param dry.run If \code{TRUE}, compute the changes to your packrat state that
#'   would be made if a restore was performed, without actually executing them.
#' @param restart If \code{TRUE}, restart the R session after restoring.
#'
#' @seealso
#' \code{\link{snapshot}}, the command that creates the snapshots applied with
#' \code{restore}.
#'
#' \code{\link{status}} to view the differences between the most recent snapshot
#' and the library.
#'
#' @export
restore <- function(project = NULL,
                    overwrite.dirty = FALSE,
                    prompt = interactive(),
                    dry.run = FALSE,
                    restart = !dry.run) {

  project <- getProjectDir(project)
  stopIfNotPackified(project)

  if (!dry.run) {
    callHook(project, "restore", TRUE)
    on.exit(callHook(project, "restore", FALSE), add = TRUE)
  }

  # Ensure the .libPaths() is set for the duration of this restore.
  # Because it's possible for a user to attempt to restore a particular
  # project while _not_ within packrat mode, we do not want the new
  # .libPaths() to be persistent -- so we unset them at the conclusion
  # of the restore. This is done to ensure downstream calls to e.g.
  # `system.file()` are successful.
  if (!file.exists(libDir(project)))
    dir.create(libDir(project), recursive = TRUE)

  oldLibPaths <- .libPaths()
  .libPaths(c(libDir(project), oldLibPaths))
  on.exit(.libPaths(oldLibPaths), add = TRUE)

  # RTools cp.exe (invoked during installation) can warn on Windows since we
  # use paths of the format c:/foo/bar and it prefers /cygwin/c/foo/bar.
  # Unfortunately, R's implementation of tar treats this warning output as
  # though it were part of the list of files in the archive.
  cygwin <- Sys.getenv("CYGWIN", unset = NA)
  if (Sys.info()["sysname"] == "Windows" && length(grep("nodosfilewarning", cygwin)) == 0) {
    Sys.setenv("CYGWIN" = paste(cygwin, "nodosfilewarning"))
    on.exit(Sys.setenv("CYGWIN" = cygwin), add = TRUE)
  }

  packages <- lockInfo(project)
  r_version <- lockInfo(project, 'r_version')
  if (!identical(as.character(getRversion()), r_version)) {
    warning('The most recent snapshot was generated using R version ',
            r_version)
  }

  # Make sure the library directory exists
  libDir <- libDir(project)
  if (!file.exists(libDir)) {
    dir.create(libDir, recursive = TRUE)
  }

  # See if any of the packages that are currently in the library are dirty.
  # Dirty packages that are represented in the snapshot will be either ignored
  # (with a message) or overwritten, depending on the value of the
  # overwrite.dirty flag. Dirty packages that are not represented in the snapshot
  # (like git untracked) will be silently ignored in all cases.

  libPkgNames <- rownames(installed.packages(libDir, noCache = TRUE))
  dirty <- !installedByPackrat(libPkgNames, libDir, TRUE)
  dirtyPackageNames <- libPkgNames[dirty]

  if (!isTRUE(overwrite.dirty)) {
    prettyPrint(
      packages[pkgNames(packages) %in% dirtyPackageNames],
      'The following packages were not installed by packrat and will be ignored:',
      'If you would like to overwrite them, call restore again with\noverwrite.dirty = TRUE.'
    )
    # Keep all dirty packages
    pkgsToIgnore <- dirtyPackageNames
  } else {
    # Even if overwrite.dirty is TRUE, we still want to keep packages that are
    # dirty and NOT represented in the list of packages to install (this is akin
    # to "untracked" files in git).
    pkgsToIgnore <- dirtyPackageNames[!dirtyPackageNames %in% pkgNames(packages)]
  }

  # Install each package from CRAN or github, from binaries when available and
  # then from sources.
  repos <- lockInfo(project, 'repos')
  restoreImpl(project, repos, packages, libDir,
              pkgsToIgnore = pkgsToIgnore, prompt = prompt,
              dry.run = dry.run,
              restart = restart)
}

#' Remove Packages from the Library
#'
#' Remove packages from the given library.
#'
#' @param packages A set of package names to remove from the project. When
#'   \code{NULL}, \code{\link{unused_packages}} is used to find packages
#'   unused in the project.
#' @param project The project directory. Defaults to current working
#' directory.
#' @param lib.loc The library to clean. Defaults to the private package library
#' associated with the project directory.
#' @param dry.run Perform a dry run, returning records on which packages would
#'   have been moved by the current clean action.
#' @param force Force package removal, even if they are still in use within the project?
#'
#' @examples \dontrun{
#'
#' # Get unused package records
#' unused_packages()
#'
#' # Clean all unused packages
#' clean()
#'
#' # Clean specific packages
#' clean("foo")
#'
#' }
#' @export
clean <- function(packages = NULL,
                  project = NULL,
                  lib.loc = libDir(project),
                  dry.run = FALSE,
                  force = FALSE) {

  project <- getProjectDir(project)
  stopIfNotPackified(project)

  callHook(project, "clean", TRUE)
  on.exit(callHook(project, "clean", FALSE), add = TRUE)

  cleanableRecords <- unused_packages(project = project, lib.loc = lib.loc)
  cleanable <- sapply(cleanableRecords, "[[", "name")

  if (is.null(packages)) {
    packages <- cleanable
  }

  pkgsUnsafeToRemove <- setdiff(packages, cleanable)
  if (length(pkgsUnsafeToRemove) && !force && !dry.run) {
    stop("The following packages are in use in your project and are unsafe to remove:\n- ",
         paste(shQuote(pkgsUnsafeToRemove), collapse = ", "),
         "\nUse clean(..., force = TRUE) to force removal")
  }

  if (dry.run) {

    if (identical(packages, cleanable)) {
      pkgRecords <- cleanableRecords
    } else {
      pkgRecords <- getPackageRecords(packages,
                                      project = project,
                                      available = NULL,
                                      recursive = FALSE,
                                      lib.loc = lib.loc)
    }
    actions <- rep("remove", length(packages))
    names(actions) <- packages
    invisible(list(pkgRecords = pkgRecords,
                   actions = actions))
  } else {

    result <- removePkgs(project = project,
                         pkgNames = packages,
                         lib.loc = lib.loc)

    if (length(result)) {
      message("The following packages have been removed:\n- ",
              paste(shQuote(result), collapse = ", "))
    } else {
      message("The packrat private library is already clean.")
    }

    invisible(result)

  }

}

##' Find Unused Packages in a Project
##'
##' Unused packages are those still contained within your project library, but
##' are unused in your project.
##'
##' @param project The project directory.
##' @param lib.loc The library to check.
##' @export
unused_packages <- function(project = NULL,
                            lib.loc = libDir(project)) {

  project <- getProjectDir(project)
  packagesInUse <- appDependencies(project)

  installedPkgNames <- row.names(installed.packages(
    lib.loc = lib.loc,
    priority = c('NA', 'recommended'), noCache = TRUE
  ))

  orphans <- setdiff(installedPkgNames,
                     packagesInUse)

  ## Exclude 'manipulate', 'rstudio'
  orphans <- setdiff(orphans, c("manipulate", "rstudio"))
  orphanRecs <- getPackageRecords(orphans,
                                  project = project,
                                  available = NULL,
                                  recursive = FALSE,
                                  lib.loc = lib.loc)
  orphanRecs

}

#' Automatically Enter Packrat Mode on Startup
#'
#' Install/augment the \code{.Rprofile} in a project, so that all \R sessions
#' started in this directory enter \code{packrat mode}, and use the local
#' project library.
#'
#' It is not normally necessary to call \code{packify} directly; these files are
#' normally installed by \code{\link{init}}. \code{packify} can be used to
#' restore the files if they are missing (for instance, if they were not added to
#' source control, or were accidentally removed).
#'
#' You'll need to restart \R in the specified directory after running
#' \code{packify} in order to start using the private package library.
#'
#' @param project The directory in which to install the \code{.Rprofile} file.
#' @param quiet Be chatty?
#' @export
packify <- function(project = NULL, quiet = FALSE) {

  project <- getProjectDir(project)
  packratDir <- getPackratDir(project)

  if (!file.exists(packratDir)) {
    dir.create(packratDir)
  }

  libraryRootDir <- libraryRootDir(project)
  if (!file.exists(libraryRootDir)) {
    dir.create(libraryRootDir)
  }

  srcDir <- srcDir(project)
  if (!file.exists(srcDir)) {
    dir.create(srcDir)
  }

  ## Copy over the packrat autoloader
  augmentRprofile(project = project)

  ## Copy in packrat/init.R
  file.copy(
    instInitFilePath(),
    file.path(project, "packrat", "init.R"),
    overwrite = TRUE
  )

  invisible()
}

lockInfo <- function(project, property='packages', fatal=TRUE) {

  project <- getProjectDir(project)

  # Get and parse the lockfile
  lockFilePath <- lockFilePath(project)
  if (!file.exists(lockFilePath)) {
    if (fatal) {
      stop(paste(lockFilePath, " is missing. Run packrat::init('",
                 project, "') to generate it.", sep = ""))
    } else {
      return(list())
    }
  }
  readLockFile(lockFilePath)[[property]]
}

