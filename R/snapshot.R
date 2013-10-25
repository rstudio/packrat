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
