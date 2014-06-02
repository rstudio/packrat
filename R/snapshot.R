#' Capture and store the packages and versions in use
#'
#' Finds the packages in use by the R code in the project, and stores a list
#' of those packages, their sources, and their current versions in packrat.
#' @param projDir The project directory. Defaults to current working
#'   directory.
#' @param available A database of available packages, as returned by
#'   \code{\link{available.packages}}. It is only necessary to supply this
#'   parameter if the state being snapshotted includes packages not installed
#'   locally, which is rare.
#' @param lib.loc The library to snapshot. Defaults to the private library
#'   associated with the given directory.
#' @param source.packages A character vector of directories containing R
#'   package sources. It is only necessary to supply this parameter when using a
#'   package for which sources exist on neither CRAN or GitHub.
#' @param orphan.check \code{TRUE} to check for orphaned packages; \code{FALSE}
#'   to skip the check. Packrat only considers packages used by your code and
#'   packages which are dependencies of packages used by your code. Any other
#'   package in the private library is considered an orphan.  If the packages
#'   are truly orphans, they can be removed with \code{\link{clean}}; if they
#'   are not, you can make packrat aware that your project needs them by adding
#'   a \code{require} statement to any R file.
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
#' # Take a snapshot of a project that includes a custom package
#' snapshot(source.packages = c(
#'   "~/R/MyCustomPackage",
#'   "~/R/MyOtherPackage_0.1.0.tar.gz"
#' ))
#' }
#' @export
snapshot <- function(projDir = NULL, available = NULL, lib.loc = libDir(projDir),
                     source.packages = NULL, orphan.check = TRUE,
                     ignore.stale = FALSE, dry.run = FALSE,
                     prompt = interactive()) {

  projDir <- getProjectDir(projDir)

  # Prompt the user to bootstrap if the project has not yet been bootstrapped
  stopIfNotPackified(projDir)

  if (file.exists(snapshotLockFilePath(projDir))) {
    stop("An automatic snapshot is currently in progress -- cannot proceed")
  }

  source.packages <- getSourcePackageInfo(source.packages)
  appPackages <- snapshotImpl(projDir, available, lib.loc,
                              source.packages, dry.run,
                              orphan.check = orphan.check,
                              ignore.stale = ignore.stale,
                              prompt = prompt && !dry.run)

  if (dry.run) return(invisible())

  # Check to see if any of the packages we just snapshotted are not, in fact,
  # located in the private library, and install them if necessary
  appPackageNames <- pkgNames(flattenPackageRecords(appPackages))
  privatelyInstalled <- rownames(installed.packages(lib.loc, noCache=TRUE))
  pkgsToInstall <- appPackageNames[!(appPackageNames %in% privatelyInstalled)]
  for (pkgToInstall in pkgsToInstall) {
    message("Installing ", pkgToInstall, "... ", appendLF = FALSE)
    type <- installPkg(searchPackages(appPackages, pkgToInstall)[[1]],
                       projDir, NULL, activeRepos(projDir), lib.loc)
    message("OK (", type, ")")
  }
  for (pkgRecord in flattenPackageRecords(appPackages)) {
    annotatePkgDesc(pkgRecord, projDir=projDir, lib=lib.loc)
  }

}

snapshotImpl <- function(projDir, available = NULL, lib.loc = libDir(projDir),
                         source.packages = NULL, dry.run = FALSE,
                         orphan.check = FALSE, ignore.stale = FALSE,
                         prompt = interactive(),
                         auto.snapshot = FALSE,
                         verbose = TRUE) {
  lockPackages <- lockInfo(projDir, fatal=FALSE)

  # Get the package records for dependencies of the app. It's necessary to
  # include .libPaths in the list of library locations because it's possible that
  # the user installed a package that relies on a recommended package, which
  # would be used by the app but not present in the private library.
  appPackages <- getPackageRecords(sort(appDependencies(projDir)),
                                   available,
                                   source.packages,
                                   lib.loc = unique(c(lib.loc, .libPaths())),
                                   missing.package=function(package, lib.loc) {
                                     NULL
                                   })
  appPackagesFlat <- flattenPackageRecords(appPackages, sourcePath = TRUE)

  allLibPkgs <- row.names(installed.packages(lib.loc = lib.loc, noCache = TRUE))

  orphans <- setdiff(allLibPkgs, pkgNames(appPackagesFlat))

  if (orphan.check && verbose) {
    on.exit({
      prettyPrint(
        getPackageRecords(orphans, NULL, source.packages = source.packages,
                          recursive = FALSE, lib.loc = lib.loc),
        '--\nThe following packages are orphaned (they are in your private library\nbut are not referenced from your R code).',
        'You can remove them using packrat::clean(), or include them in packrat\nby adding a library or require call to your R code, and running\nsnapshot again.')
    })
  }

  diffs <- diff(lockPackages, appPackages)
  mustConfirm <- any(c('downgrade', 'remove', 'crossgrade') %in% diffs)

  if (!ignore.stale) {
    # If any packages are installed, different from what's in the lockfile, and
    # were installed by packrat, that means they are stale.
    stale <- names(diffs)[!is.na(diffs) & installedByPackrat(names(diffs), lib.loc, FALSE)]
    if (length(stale) > 0 && verbose) {
      prettyPrint(
        getPackageRecords(stale, NULL, source.packages = source.packages,
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
    summarizeDiffs(diffs, lockPackages, appPackages,
                   'Adding these packages to packrat:',
                   'Removing these packages from packrat:',
                   'Upgrading these packages already present in packrat:',
                   'Downgrading these packages already present in packrat:',
                   'Modifying these packages already present in packrat:')
  }

  if (all(is.na(diffs))) {
    if (!verbose) message("Already up to date.")
    if (is.null(lib.loc) ||
          all(installedByPackrat(pkgNames(appPackagesFlat), lib.loc, FALSE))) {
      # If none of the packages/versions differ, and all of the packages in the
      # private library were installed by packrat, then we can short-circuit.
      # If the package/versions differ, we obviously need to continue, so we can
      # write the new lockfile. If packages were installed NOT by packrat, we
      # need to mark them as installed by packrat so they no longer are
      # considered "dirty" changes in need of snapshotting.
      return()
    }
  }

  ## For use by automatic snapshotting -- only perform the automatic snapshot
  ## if it's a 'safe' action
  if (mustConfirm && auto.snapshot) return(invisible())

  if (prompt && mustConfirm) {
    answer <- readline('Do you want to continue? [Y/n] ')
    answer <- gsub('^\\s*(.*?)\\s*$', '\\1', answer)
    if (nzchar(answer) && tolower(answer) != 'y') {
      return(invisible())
    }
  }

  if (!dry.run) {
    snapshotSources(projDir, activeRepos(projDir), appPackagesFlat)
    writeLockFile(
      lockFilePath(projDir),
      appPackages
    )
    if (verbose) {
      message('Snapshot written to ',
              shQuote(normalizePath(lockFilePath(projDir), winslash = '/'))
      )
    }
  }

  return(invisible(appPackages))
}

# Returns a vector of all active repos, including CRAN (with a fallback to the
# RStudio CRAN mirror if none is specified) and Bioconductor if installed.
activeRepos <- function(projDir) {
  repos <- lockInfo(projDir, 'repos', fatal = FALSE)
  if (length(repos) > 0)
    return(strsplit(repos, '\\s*,\\s*')[[1]])

  repos <- as.vector(getOption("repos"))
  repos[repos == "@CRAN@"] <- "http://cran.rstudio.com/"

  # Check to see whether Bioconductor is installed. Bioconductor maintains a
  # private set of repos, which we need to expose here so we can download
  # sources to Bioconducter packages.
  if (isTRUE(nchar(find.package("BiocInstaller", quiet = TRUE)) > 0)) {
    # Bioconductor repos may include repos already accounted for above
    repos <- unique(c(repos, as.vector(BiocInstaller::biocinstallRepos())))
  }
  return(repos)
}
