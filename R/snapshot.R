#' Capture and store the packages and versions in use
#' 
#' Finds the packages in use by the R code in the application, and stores a list
#' of those packages, their sources, and their current versions in packrat.
#' @param appDir Directory containing application. Defaults to current working 
#'   directory.
#' @param available A database of available packages, as returned by 
#'   \code{\link{available.packages}}. It is only necessary to supply this 
#'   parameter if the state being snapshotted includes packages not installed 
#'   locally, which is rare.
#' @param lib.loc The library to snapshot. Defaults to the private library 
#'   associated with the given directory.
#' @param sourcePackagePaths A character vector of directories containing R 
#'   package sources. It is only necessary to supply this parameter when using a
#'   package for which sources exist on neither CRAN or Github.
#' @param orphan.check \code{TRUE} to check for orphaned packages; \code{FALSE} 
#'   to skip the check. Packrat only considers packages used by your code and 
#'   packages which are dependencies of packages used by your code. Any other 
#'   package in the private library is considered an orphan.  If the packages
#'   are truly orphans, they can be removed with \code{\link{clean}}; if they
#'   are not, you can make packrat aware that your project needs them by adding
#'   a \code{require} statement to any R file (see
#'   \code{\link{appDependencies}}).
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
#' the sources stored in the project's \code{packrat.sources} directory. If you
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
#' snapshot(sourcePackagePaths = "~/R/MyCustomPackage")
#' }
#' @export
snapshot <- function(appDir = ".", available = NULL, lib.loc = libdir(appDir),
                     sourcePackagePaths = NULL, orphan.check = TRUE,
                     ignore.stale = FALSE, dry.run = FALSE, 
                     prompt = interactive()) {
  appDir <- normalizePath(appDir, winslash='/', mustWork=TRUE)

  sourcePackages <- getSourcePackageInfo(sourcePackagePaths)
  appPackages <- snapshotImpl(appDir, available, lib.loc, sourcePackages, dry.run,
                              orphan.check = orphan.check,
                              ignore.stale = ignore.stale, 
                              prompt = prompt && !dry.run)
  
  if (!dry.run) {
    # Check to see if any of the packages we just snapshotted are not, in fact,
    # located in the private library, and install them if necessary
    appPackageNames <- pkgNames(flattenPackageRecords(appPackages))
    privatelyInstalled <- rownames(installed.packages(lib.loc, noCache=TRUE))
    pkgsToInstall <- appPackageNames[!(appPackageNames %in% privatelyInstalled)]
    if (length(pkgsToInstall) > 0) {
      installPkgs(appDir,
                  activeRepos(),
                  searchPackages(appPackages, appPackageNames),
                  lib.loc, prompt)
    }
  }
}

snapshotImpl <- function(appDir = '.', available = NULL, lib.loc = libdir(appDir),
                         sourcePackages = NULL, dry.run = FALSE,
                         orphan.check = FALSE, ignore.stale = FALSE,
                         prompt = interactive()) {
  lockPackages <- lockInfo(appDir, fatal=FALSE)
  
  # Get the package records for dependencies of the app. It's necessary to 
  # include .libPaths in the list of library locations because it's possible that
  # the user installed a package that relies on a recommended package, which
  # would be used by the app but not present in the private library.
  appPackages <- getPackageRecords(sort(appDependencies(appDir)), available,
                                   sourcePackages, 
                                   lib.loc = unique(c(lib.loc, .libPaths())))
  
  allLibPkgs <- row.names(installed.packages(lib.loc = lib.loc, noCache = TRUE))
  
  orphans <- setdiff(allLibPkgs, pkgNames(flattenPackageRecords(appPackages)))
  
  if (orphan.check) {
    on.exit({
      prettyPrint(
        getPackageRecords(orphans, NULL, sourcePackages = sourcePackages,
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
    if (length(stale) > 0) {
      prettyPrint(
        getPackageRecords(stale, NULL, sourcePackages = sourcePackages,
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
  
  summarizeDiffs(diffs, lockPackages, appPackages, 
                 'Adding these packages to packrat:', 
                 'Removing these packages from packrat:', 
                 'Upgrading these packages already present in packrat:',
                 'Downgrading these packages already present in packrat:',
                 'Modifying these packages already present in packrat:')
  
  if (all(is.na(diffs))) {
    message("Already up to date")
    if (is.null(lib.loc) || 
          all(installedByPackrat(pkgNames(flattenPackageRecords(appPackages)),
                                 lib.loc, FALSE))) {
      # If none of the packages/versions differ, and all of the packages in the
      # private library were installed by packrat, then we can short-circuit.
      # If the package/versions differ, we obviously need to continue, so we can
      # write the new lockfile. If packages were installed NOT by packrat, we
      # need to mark them as installed by packrat so they no longer are
      # considered "dirty" changes in need of snapshotting.
      return()
    }
  }
  
  if (prompt && mustConfirm) {
    answer <- readline('Do you want to continue? [Y/n] ')
    answer <- gsub('^\\s*(.*?)\\s*$', '\\1', answer)
    if (nzchar(answer) && tolower(answer) != 'y') {
      return(invisible())
    }
  }
  
  if (!dry.run) {
    snapshotSources(appDir, activeRepos(), makeInstallList(appPackages))
    writeLockFile(file.path(appDir, "packrat.lock"),
                  appPackages)
    cat('Snapshot written to', 
        normalizePath(file.path(appDir, "packrat.lock"), winslash = '/'), '\n')

    if (!is.null(lib.loc)) {
      for (pkgRecord in flattenPackageRecords(appPackages)) {
        annotatePkgDesc(pkgRecord, appDir=appDir, lib=lib.loc)
      }
    }
  }
  
  return(invisible(appPackages))
}

# Returns a vector of all active repos, including CRAN (with a fallback to the
# RStudio CRAN mirror if none is specified) and Bioconductor if installed.
activeRepos <- function() {
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
