#' @export
snapshot <- function(appDir = ".", available = NULL, lib.loc = libdir(appDir),
                     sourcePackagePaths = NULL, orphan.check = TRUE,
                     dry.run = FALSE) {

  sourcePackages <- getSourcePackageInfo(sourcePackagePaths)
  snapshotImpl(appDir, available, lib.loc, sourcePackages, dry.run,
               orphan.check = orphan.check)
}

snapshotImpl <- function(appDir = '.', available = NULL, lib.loc = libdir(appDir),
                         sourcePackages = NULL, dry.run = FALSE,
                         orphan.check = FALSE) {
  
  lockPackages <- lockInfo(appDir, fatal=FALSE)
  
  appPackages <- getPackageRecords(sort(appDependencies(appDir)), available,
                                   sourcePackages, lib.loc = lib.loc)
  
  allLibPkgs <- row.names(installed.packages(lib.loc = lib.loc, priority = 'NA',
                                             noCache = TRUE))
  
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
  
  prettyPrint(
    searchPackages(appPackages, names(diffs)[!is.na(diffs) & diffs == 'add']),
    'Adding these packages to packrat:'
  )
  prettyPrint(
    searchPackages(lockPackages, names(diffs)[!is.na(diffs) & diffs == 'remove']),
    'Removing these packages from packrat:'
  )
  prettyPrintPair(
    searchPackages(lockPackages, names(diffs)[!is.na(diffs) & diffs == 'upgrade']),
    searchPackages(appPackages, names(diffs)[!is.na(diffs) & diffs == 'upgrade']),
    'Upgrading these packages already present in packrat:'
  )
  prettyPrintPair(
    searchPackages(lockPackages, names(diffs)[!is.na(diffs) & diffs == 'downgrade']),
    searchPackages(appPackages, names(diffs)[!is.na(diffs) & diffs == 'downgrade']),
    'Downgrading these packages already present in packrat:'
  )
  prettyPrintPair(
    searchPackages(lockPackages, names(diffs)[!is.na(diffs) & diffs == 'crossgrade']),
    searchPackages(appPackages, names(diffs)[!is.na(diffs) & diffs == 'crossgrade']),
    'Modifying these packages already present in packrat:'
  )
  
  if (all(is.na(diffs))) {
    message("Already up to date")
    return()
  }
  
  if (interactive() && mustConfirm) {
    answer <- readline('Do you want to continue? [Y/n] ')
    answer <- gsub('^\\s*(.*?)\\s*$', '\\1', answer)
    if (nzchar(answer) && tolower(answer) != 'y') {
      return()
    }
  }
  
  # Compute the package dependency information from the DESCRIPTION and write 
  # the lock file
  if (!dry.run) {
    writeLockFile(file.path(appDir, "packrat.lock"),
                  appPackages)
    cat('Snapshot written to', normalizePath(file.path(appDir, "packrat.lock")), '\n')
  }
  
  return(invisible())
}