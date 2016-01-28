#' Capture and store the packages and versions in use
#'
#' Finds the packages in use in the project, and stores a list
#' of those packages, their sources, and their current versions in packrat.
#'
#' @param project The project directory. Defaults to current working
#'   directory.
#' @param available A database of available packages.
#' @param lib.loc The library to snapshot. Defaults to the private library
#'   associated with the given directory.
#' @param ignore.stale Stale packages are packages that are different from the
#'   last snapshot, but were installed by packrat. Typically, packages become
#'   stale when a new snapshot is available, but you haven't applied it yet with
#'   \code{\link{restore}}. By default, packrat will prevent you from taking a
#'   snapshot when you have stale packages to prevent you from losing changes
#'   from the unapplied snapshot. If your intent is to overwrite the last
#'   snapshot without applying it, use \code{ignore.stale = TRUE} to skip this
#'   check.
#' @param dry.run Computes the changes to your packrat state that would be made
#'   if a snapshot were performed, and prints them to the console.
#' @param prompt \code{TRUE} to prompt before performing snapshotting package
#'   changes that might be unintended; \code{FALSE} to perform these operations
#'   without confirmation. Potentially unintended changes include snapshotting
#'   packages at an older version than the last snapshot, or missing despite
#'   being present in the last snapshot.
#'
#' @note \code{snapshot} modifies the project's \code{packrat.lock} file, and
#' the sources stored in the project's \code{packrat/src} directory. If you
#' are working with a version control system, your collaborators can sync the
#' changes to these files and then use \code{\link{restore}} to apply your
#' snapshot.
#'
#' @seealso
#' \code{\link{restore}} to apply a snapshot.
#' \code{\link{status}} to view the differences between the most recent snapshot
#' and the library.
#' @examples
#' \dontrun{
#' # Take a snapshot of the current project
#' snapshot()
#'
#' # See what changes would be included in a snapshot
#' snapshot(dry.run = TRUE)
#'
#' }
#' @export
snapshot <- function(project = NULL,
                     available = NULL,
                     lib.loc = libDir(project),
                     ignore.stale = FALSE,
                     dry.run = FALSE,
                     prompt = interactive()) {

  if (is.null(available))
  {
    available <- if (dry.run)
      availablePackagesSkeleton()
    else
      available.packages()
  }

  project <- getProjectDir(project)

  # Prompt the user to initialize if the project has not yet been initialized
  stopIfNotPackified(project)

  if (file.exists(snapshotLockFilePath(project))) {
    stop("An automatic snapshot is currently in progress -- cannot proceed")
  }

  if (!dry.run) {
    callHook(project, "snapshot", TRUE)
    on.exit(callHook(project, "snapshot", FALSE), add = TRUE)
  }

  snapshotResult <- snapshotImpl(project,
                                 available,
                                 lib.loc,
                                 dry.run,
                                 ignore.stale = ignore.stale,
                                 prompt = prompt && !dry.run)

  if (dry.run)
    return(invisible(snapshotResult))

}

#' Internal Snapshot Implementation
#'
#' This is the internal implementation for \code{\link{snapshot}}. Most users
#' should prefer calling \code{\link{snapshot}}.
#'
#' @inheritParams snapshot
#' @param auto.snapshot Internal use -- should be set to \code{TRUE} when this
#'   is an automatic snapshot.
#' @param verbose Print output to the console while \code{snapshot}-ing?
#' @param fallback.ok Fall back to the latest CRAN version of a package if the
#'   locally installed version is unavailable?
#' @param snapshot.sources Download the tarball associated with a particular
#'   package?
#' @param implicit.packrat.dependency Include \code{packrat} as an implicit
#'   dependency of this project, if not otherwise discovered? This should be
#'   \code{FALSE} only if you can guarantee that \code{packrat} will be available
#'   via other means when attempting to load this project.
#' @keywords internal
#' @rdname snapshotImpl
#' @export
.snapshotImpl <- function(project,
                          available = NULL,
                          lib.loc = libDir(project),
                          dry.run = FALSE,
                          ignore.stale = FALSE,
                          prompt = interactive(),
                          auto.snapshot = FALSE,
                          verbose = TRUE,
                          fallback.ok = FALSE,
                          snapshot.sources = TRUE,
                          implicit.packrat.dependency = TRUE) {

  if (is.null(available))
  {
    available <- if (dry.run)
      availablePackagesSkeleton()
    else
      available.packages()
  }

  # ensure packrat directory available
  packratDir <- getPackratDir(project)
  if (!file.exists(packratDir))
    dir.create(packratDir, recursive = TRUE)

  # When snapshotting, we take the union of:
  #
  # 1. Inferred dependencies (packages that appear to be in use in your code), and
  # 2. The current state of your private library.
  #
  # When packages are inferred from the code, the version taken is the most
  # current available in the current set of repositories.

  ## If we are using packrat alongside an R package, then we should
  ## ignore the package itself
  ignore <- NULL
  if (isRPackage(project = project)) {
    desc <- readDcf(file.path(project, "DESCRIPTION"))
    if ("Package" %in% colnames(desc)) {
      # R packages are not guaranteed to have a "Package" field (see isRPackge)
      ignore <- unname(desc[, "Package"])
    }
  }

  ## rstudio, manipulate needs ignoring
  ignore <- c(ignore, c("manipulate", "rstudio"))

  libPkgs <- setdiff(list.files(libDir(project)), ignore)
  inferredPkgs <- sort_c(appDependencies(project,
                                         available.packages = available,
                                         implicit.packrat.dependency = implicit.packrat.dependency))

  inferredPkgsNotInLib <- setdiff(inferredPkgs, libPkgs)

  # Packages currently available in the library should have package records
  # available, so we don't overload the missing.package argument of
  # getPackageRecords and let it fail if something goes wrong
  libPkgRecords <- getPackageRecords(libPkgs,
                                     project = project,
                                     available = available,
                                     lib.loc = lib.loc)

  # For inferred packages (ie. packages within the code), we try to construct
  # records first from the lockfile, and then from other sources if possible
  # (CRAN, GitHub, source repository)
  inferredPkgRecords <- getPackageRecords(inferredPkgsNotInLib,
                                          project = project,
                                          available = available,
                                          check.lockfile = TRUE,
                                          fallback.ok = fallback.ok)

  allRecords <- c(
    libPkgRecords,
    inferredPkgRecords
  )

  allRecordsFlat <- c(
    flattenPackageRecords(libPkgRecords, depInfo = TRUE, sourcePath = TRUE),
    flattenPackageRecords(inferredPkgRecords, depInfo = TRUE, sourcePath = TRUE)
  )

  # Compare the new records we wish to write against the current lockfile
  # (last snapshot)
  lockPackages <- lockInfo(project, fatal = FALSE)
  diffs <- diff(lockPackages, allRecords)

  # Don't remove packages that are specified as part of external.packages
  # This means 'external.packages' is 'sticky', in that we only remove a package
  # from the lockfile that's an external package if it's also removed from that field
  ## TODO: Think about 'downgrade', 'crossgrade', 'upgrade' -- but presumedly this
  ## shouldn't happen for external.packages within the context of a packrat project
  diffs <- diffs[!(names(diffs) %in% opts$external.packages())]
  mustConfirm <- any(c('downgrade', 'remove', 'crossgrade') %in% diffs)

  if (!ignore.stale) {
    # If any packages are installed, different from what's in the lockfile, and
    # were installed by packrat, that means they are stale.
    stale <- names(diffs)[!is.na(diffs) & installedByPackrat(names(diffs), lib.loc, FALSE)]
    if (length(stale) > 0 && verbose) {
      prettyPrint(
        getPackageRecords(stale,
                          project = project,
                          NULL,
                          recursive = FALSE, lib.loc = lib.loc),
        'The following packages are stale:',
        c('These packages must be updated by calling packrat::restore() before\n',
          'snapshotting. If you are sure you want the installed versions of these\n',
          'packages to be snapshotted, call packrat::snapshot() again with\n',
          'ignore.stale=TRUE.')
      )
      message('--\nSnapshot operation was cancelled, no changes were made.')
      return(invisible())
    }
  }

  if (verbose) {
    summarizeDiffs(diffs, lockPackages, allRecords,
                   'Adding these packages to packrat:',
                   'Removing these packages from packrat:',
                   'Upgrading these packages already present in packrat:',
                   'Downgrading these packages already present in packrat:',
                   'Modifying these packages already present in packrat:')
  }

  ## For use by automatic snapshotting -- only perform the automatic snapshot
  ## if it's a 'safe' action; ie, escape early if we would have prompted
  if (mustConfirm && isTRUE(auto.snapshot))
    return(invisible())

  ## Short-circuit if we know that there is nothing to be updated.
  if (file.exists(lockFilePath(project)) && all(is.na(diffs))) {

    # Check to see if the current repositories + the snapshotted
    # repositories are in sync.
    lockfile <- readLockFile(lockFilePath(project))
    lockfileRepos <- lockfile$repos
    reposInSync <- identical(sort_c(getOption("repos")),
                             sort_c(lockfileRepos))

    # Check to see whether all of the installed packages are currently
    # tracked by packrat.
    if (!reposInSync) {

      allTracked <-
        is.null(lib.loc) ||
        all(installedByPackrat(pkgNames(allRecordsFlat), lib.loc, FALSE))

      if (allTracked) {

        # Ensure a packrat lockfile is available
        if (!file.exists(lockFilePath(project)))
          writeLockFile(lockFilePath(project), allRecords)
        else if (verbose)
          message("Already up to date.")

        return()

      }
    }
  }

  if (prompt && mustConfirm) {
    answer <- readline('Do you want to continue? [Y/n]: ')
    answer <- gsub('^\\s*(.*?)\\s*$', '\\1', answer)
    if (nzchar(answer) && tolower(answer) != 'y') {
      return(invisible())
    }
  }

  if (!dry.run) {

    if (snapshot.sources)
      snapshotSources(project, activeRepos(project), allRecordsFlat)

    writeLockFile(
      lockFilePath(project),
      allRecords
    )

    for (record in allRecordsFlat) {
      name <- record$name
      path <- file.path(libDir(project), name, "DESCRIPTION")
      if (file.exists(path)) {
        annotatePkgDesc(record, project, libDir(project))
      }
    }

    moveInstalledPackagesToCache(project)

    if (verbose) {
      message('Snapshot written to ',
              shQuote(normalizePath(lockFilePath(project), winslash = '/'))
      )
    }
  }

  return(invisible(list(pkgRecords = lockPackages,
                        actions = diffs[!is.na(diffs)],
                        pkgsSnapshot = allRecords)))
}

# NOTE: `.snapshotImpl` is exported as an 'internal' function that may be
# used by other packages, but we keep an (unexported) version of `snapshotImpl`
# around for compatibility with older Packrat versions.
snapshotImpl <- .snapshotImpl

getBiocRepos <- function() {
  BiocInstaller::biocinstallRepos()
}

# Returns a vector of all active repos, including CRAN (with a fallback to the
# RStudio CRAN mirror if none is specified) and Bioconductor if installed.
activeRepos <- function(project) {
  repos <- getOption("repos")
  repos[repos == "@CRAN@"] <- "http://cran.rstudio.com/"

  # Check to see whether Bioconductor is installed. Bioconductor maintains a
  # private set of repos, which we need to expose here so we can download
  # sources to Bioconducter packages.
  if (isTRUE(nchar(find.package("BiocInstaller", quiet = TRUE)) > 0)) {
    biocRepos <- getBiocRepos()
    biocRepoNames <- names(biocRepos)
    oldRepos <- repos[!(names(repos) %in% biocRepoNames)]
    repos <- c(oldRepos, biocRepos)
  }

  return(repos)
}
