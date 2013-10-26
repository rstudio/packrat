#' @importFrom RJSONIO fromJSON toJSON
NULL

#' Initialize Packrat on a new or existing R project
#' 
#' Given an application directory, computes the application's 
#' dependencies, and places the application's dependencies under packrat
#' control.
#' 
#' You must restart your R session after running bootstrap in order for
#' the changes to take effect.
#' 
#' @param appDir The directory that contains the R project.
#' @param sourcePackagePaths Optional list of paths to unpacked R
#'   package source directories.
#'   
#' @export
bootstrap <- function(appDir = '.', sourcePackagePaths = character()) {
  appDir <- normalizePath(appDir, winslash='/', mustWork=TRUE)
  
  if (nzchar(Sys.getenv("R_PACKRAT"))) {
    stop("This project is already running under packrat!")
  }
  
  descriptionFile <- file.path(appDir, 'DESCRIPTION')
  
  if (file.exists(descriptionFile)) {
    description <- as.data.frame(read.dcf(descriptionFile))
    package <- description$Package
    if (!is.null(package)) {
      stop("This project appears to be an R package. Packrat doesn't work on ",
           "packages.")
    }
  }
  
  # Get the inferred set of dependencies and take a snapshot
  inferredDependencies <- appDependencies(appDir)
  sourcePackages <- getSourcePackageInfo(sourcePackagePaths)
  snapshotImpl(appDir, available.packages(contrib.url(activeRepos())),
               sourcePackages=sourcePackages, lib.loc = NULL, ignore.stale=TRUE)
  
  # Use the lockfile to copy sources and install packages to the library
  restore(appDir, overwriteDirty=TRUE)
  
  # Write the .Rprofile and .Renviron files
  packify(appDir)
  
  invisible()
}

#' @export
restore <- function(appDir = '.', overwriteDirty = FALSE, 
                    prompt = interactive()) {
  appDir <- normalizePath(appDir, winslash='/', mustWork=TRUE)
  
  # RTools cp.exe (invoked during installation) can warn on Windows since we
  # use paths of the format c:/foo/bar and it prefers /cygwin/c/foo/bar. 
  # Unfortunately, R's implementation of tar treats this warning output as
  # though it were part of the list of files in the archive. 
  cygwin <- Sys.getenv("CYGWIN")
  if (length(grep("nodosfilewarning", cygwin)) == 0) {
    Sys.setenv("CYGWIN" = paste(cygwin, "nodosfilewarning"))
    on.exit(Sys.setenv("CYGWIN" = cygwin), add = TRUE)
  }
  
  packages <- lockInfo(appDir)
  r_version <- lockInfo(appDir, 'r_version')
  if (!identical(as.character(getRversion()), r_version)) {
    warning('The most recent snapshot was generated using R version ',
            r_version)
  }
  
  # Generate the list of packages to install
  installList <- makeInstallList(packages)
  
  # Make sure the library directory exists 
  libDir <- libdir(appDir)
  if (!file.exists(libDir)) {
    dir.create(libDir, recursive=TRUE)
  }
  
  if (!isTRUE(overwriteDirty)) {
    installListNames <- pkgNames(installList)
    dirty <- !installedByPackrat(installListNames, libDir, NA)
    dirty[is.na(dirty)] <- FALSE  # Anything that is not installed is not dirty
    dirtyPackages <- getPackageRecords(installListNames[dirty], recursive=FALSE,
                                       lib.loc=libDir)
    installList <- installList[!dirty]
    prettyPrint(
      dirtyPackages,
      'The following packages are dirty and will not be overwritten:',
      'If you would like to overwrite them, call restore again with\noverwriteDirty = TRUE.'
    )
    dirtyPackageNames <- installListNames[dirty]
  } else {
    dirtyPackageNames <- character(0)
  }

  # Install each package from CRAN or github, from binaries when available and 
  # then from sources.
  repos <- lockInfo(appDir, 'repos')
  installPkgs(appDir, repos, installList, libDir,
              pkgsToKeep = dirtyPackageNames, prompt = prompt)
}

#' @export
status <- function(appDir = '.', lib.loc = libdir(appDir), quiet = FALSE) {
  appDirDefault <- identical(appDir, '.')
  appDir <- normalizePath(appDir, winslash='/', mustWork=TRUE)

  ### Step 1: Collect packages from three sources: packrat.lock, code inspection
  ### (using lib.loc and .libPaths() to find packages/dependencies), and by
  ### enumerating the packages in lib.loc.
  
  # Packages from the lockfile
  packratPackages <- lockInfo(appDir, fatal=FALSE)
  if (length(packratPackages) == 0) {
    bootstrapArg <- if (appDirDefault) '' else deparse(appDir)
    message('This directory does not appear to be using packrat.\n',
            'Call packrat::bootstrap(', bootstrapArg, ') to initialize ',
            'packrat.')
    return(invisible())
  }
  packratNames <- pkgNames(flattenPackageRecords(packratPackages))
  
  # Packages that are inferred from the code
  allAppPackageNames <- pkgNames(flattenPackageRecords(getPackageRecords(
    appDependencies(appDir), NULL, lib.loc = c(lib.loc, .libPaths()))))
  
  # (Non-recursive) packages from the library--i.e., installed packages
  libPackages <- getPackageRecords(
    as.vector(installed.packages(lib.loc, noCache=TRUE)[,'Package']), NULL,
    lib.loc=lib.loc, recursive=FALSE)
  libPackageNames <- pkgNames(libPackages)
  
  ### Step 2: Now that we have all the packages, we'll figure out which of them
  ### are dirty, are orphans, and/or don't have consistent versions between the
  ### lockfile and the library.

  # Orphan: Exists in the library, not in the code.
  orphanNames <- setdiff(pkgNames(libPackages), allAppPackageNames)
  # Dirty: Not installed by packrat.
  # TODO: Should non-lib.loc (e.g. non-installed recommended) be included in dirtyNames?
  dirtyNames <- libPackageNames[!installedByPackrat(libPackageNames, lib.loc, FALSE)]
  # Changed: Different versions represented in lockfile vs. library (including
  # added/removed packages)
  diffs <- diff(packratPackages, libPackages)
  changedNames <- names(diffs[!is.na(diffs)])
  
  ### Step 3: Create logical vectors indicating set membership, then use those
  ### vectors to classify each package.
  
  allNames <- sort(unique(c(packratNames, allAppPackageNames, libPackageNames)))
  isChanged <- allNames %in% changedNames
  #isInstalled <- allNames %in% libPackageNames
  isDirty <- allNames %in% dirtyNames
  isOrphan <- allNames %in% orphanNames
  
  # classify will have NA, restore, snapshot, and clean as possible values; the
  # rownames will be package names
  classify <- as.character(rep.int(NA, length(allNames)))
  names(classify) <- allNames
  
  # It's intentional that each assignment may overwrite previously written
  # values (e.g. in the case that something is both dirty and an orphan, it will
  # be classified as "clean").
  classify[isChanged] <- 'restore'
  classify[isDirty] <- 'snapshot'
  classify[isOrphan] <- 'clean'
  
  ### Step 4: Print the results
  
  if (!quiet) {
    fetch <- function(classification) {
      names(subset(classify, classify == classification))
    }
    
    prettyPrint(
      searchPackages(libPackages, fetch('clean')),
      "The following packages are installed but not needed:",
      c("Use packrat::clean() to remove them. Or, if they are actually needed\n",
        "by your project, add `library(packagename)` calls to a .R file\n",
        "somewhere in your project.")
    )
    
    prettyPrintPair(
      searchPackages(packratPackages, fetch('restore')),
      searchPackages(libPackages, fetch('restore')),
      "The following packages are missing from your library, or are out of date:",
      "Use packrat::restore() to install/remove the appropriate packages.",
      "packrat",
      "installed"
    )
    
    prettyPrintPair(
      searchPackages(libPackages, fetch('snapshot')),
      searchPackages(packratPackages, fetch('snapshot')),
      c("The following packages have been updated in your library, but have not\n",
        "been recorded in packrat:"),
      c("Use packrat::snapshot() to record these packages in packrat."),
      "installed",
      "packrat"
    )
  }
  
  if (!any(isChanged) && !any(isDirty) && !any(isOrphan)) {
    message('Up to date.')
    return(invisible(TRUE))
  }

  return(invisible(FALSE))
}

extractVersions <- function(packages, packageNames) {
  as.character(lapply(
    searchPackages(packages, packageNames),
    function(pkg) {
      if (is.null(pkg))
        return(NA)
      else
        return(pkg$version)
    }
  ))
}

prettyPrint <- function(packages, header, footer = NULL) {
  if (length(packages) > 0) {
    cat('\n')
    if (!is.null(header)) {
      cat(paste(header, collapse=''))
      cat('\n')
    }
    print.simple.list(lapply(packages, function(pkg) {
      result <- ifelse(is.na(pkg$version), '', pkg$version)
      result <- paste(" ", result)
      names(result) <- paste("   ", pkg$name)
      result
    }))
    if (!is.null(footer)) {
      cat(paste(footer, collapse=''))
    }
    cat('\n')
  }
}

summarizeDiffs <- function(diffs, pkgsA, pkgsB, addMessage, 
                           removeMessage, upgradeMessage, downgradeMessage, 
                           crossgradeMessage)
{
  prettyPrint(
    searchPackages(pkgsB, names(diffs)[!is.na(diffs) & diffs == 'add']),
    addMessage
  )
  prettyPrint(
    searchPackages(pkgsA, names(diffs)[!is.na(diffs) & diffs == 'remove']),
    removeMessage
  )
  prettyPrintPair(
    searchPackages(pkgsA, names(diffs)[!is.na(diffs) & diffs == 'upgrade']),
    searchPackages(pkgsB, names(diffs)[!is.na(diffs) & diffs == 'upgrade']),
    upgradeMessage
  )
  prettyPrintPair(
    searchPackages(pkgsA, names(diffs)[!is.na(diffs) & diffs == 'downgrade']),
    searchPackages(pkgsB, names(diffs)[!is.na(diffs) & diffs == 'downgrade']),
    downgradeMessage
  )
  prettyPrintPair(
    searchPackages(pkgsA, names(diffs)[!is.na(diffs) & diffs == 'crossgrade']),
    searchPackages(pkgsB, names(diffs)[!is.na(diffs) & diffs == 'crossgrade']),
    crossgradeMessage
  )
}

prettyPrintPair <- function(packagesFrom, packagesTo, header, footer = NULL,
                            fromLabel = 'from', toLabel = 'to') {
  if (length(packagesFrom) > 0) {
    if (any(pkgNames(packagesFrom) != pkgNames(packagesTo)))
      stop('Invalid arguments--package records list mistmatch')
    
    cat('\n')
    if (!is.null(header)) {
      cat(paste(header, collapse=''))
      cat('\n')
    }
    
    df <- data.frame(paste(" ", sapply(packagesFrom, pick("version"))),
                     paste(" ", sapply(packagesTo, pick("version"))))
    names(df) <- c(fromLabel, toLabel)
    row.names(df) <- paste("   ", pkgNames(packagesFrom))
    print(df)

    if (!is.null(footer)) {
      cat(paste(footer, collapse=''))
    }
    cat('\n')
  }
}

#' Remove unused packages
#' @export
clean <- function(appDir = ".", lib.loc = libdir(appDir),
                  prompt = interactive()) {

  appDir <- normalizePath(appDir, winslash='/', mustWork=TRUE)

  rootDeps <- appDependencies(appDir)
  packagesInUse <- getPackageRecords(rootDeps, available=NULL,
                                     sourcePackages=NULL,
                                     recursive=TRUE,
                                     lib.loc=c(lib.loc, .libPaths()))
  
  installedPkgNames <- row.names(installed.packages(
    lib.loc=lib.loc, priority=c('NA', 'recommended'), noCache=TRUE))
  
  orphans <- setdiff(installedPkgNames, 
                     pkgNames(flattenPackageRecords(packagesInUse)))

  if (length(orphans) > 0) {
    orphanRecs <- getPackageRecords(orphans, available=NULL,
                                    sourcePackages=NULL,
                                    recursive=FALSE,
                                    lib.loc=lib.loc)
    
    prettyPrint(orphanRecs,
                'The following packages will be removed:')
    
    if (prompt) {
      answer <- readline('Do you want to continue? [Y/n] ')
      answer <- gsub('^\\s*(.*?)\\s*$', '\\1', answer)
      if (nzchar(answer) && tolower(answer) != 'y') {
        return(invisible())
      }
    }
    
    remove.packages(orphans, lib=lib.loc)
    return(invisible(orphans))
  }
}

wipe <- function(appDir = getwd()) {

  deaugmentFile(file.path(appDir, ".Rprofile"))
  deaugmentFile(file.path(appDir, ".Renviron"))
  
  files <- "packrat.lock"
  dirs <- c("library", "library.old", "library.new", "packrat.sources")
  
  # Clean up dependency information
  unlink(file.path(appDir, files))
  # Clean up downloaded sources and library directories
  unlink(file.path(appDir, dirs), recursive = TRUE)
  
  return(invisible())
}

#' Install .Rprofile and .Renviron files in the given directory to make it
#' use a private package library.
#' 
#' @export
packify <- function(dir = '.') {
  dir <- normalizePath(dir, winslash='/', mustWork = TRUE)
  rprofile <- file.path(dir, '.Rprofile')
  renviron <- file.path(dir, '.Renviron')
  
  augmentFile(system.file('Rprofile', package='packrat'), rprofile, TRUE)
  augmentFile(system.file('Renviron', package='packrat'), renviron, FALSE)
  
  message('Packrat startup directives installed. Please quit and restart your R session.')
  
  invisible()
}

# Add the contents of srcFile into targetFile, with "magic" comments bracketing
# the contents. Should be safe to run multiple times--each subsequent call will
# replace the contents between magic comments.
augmentFile <- function(srcFile, targetFile, preferTop) {
  header <- '# -- BEGIN PACKRAT --\n'
  footer <- '# -- END PACKRAT --'
  headerFooterRegex <- '# -- BEGIN PACKRAT --\\s*\n.*?# -- END PACKRAT --'
  emptyHeaderFooter <- paste(header, footer, sep='')
  
  src <- paste(readLines(srcFile, warn=FALSE), collapse='\n')
  target <- if (file.exists(targetFile)) {
    paste(readLines(targetFile, warn=FALSE), collapse='\n')
  } else {
    ''
  }
  
  target <- gsub(headerFooterRegex,
                 emptyHeaderFooter,
                 target)
  
  if (!isTRUE(grepl(paste(header, footer, sep=''), target, fixed = TRUE))) {
    if (preferTop) {
      if (nzchar(target) > 0)
        target <- paste(emptyHeaderFooter, target, sep='\n')
      else
        target <- emptyHeaderFooter
    } else {
      if (nzchar(target) > 0)
        target <- paste(target, emptyHeaderFooter, sep='\n')
      else
        target <- emptyHeaderFooter
    }
  }
  
  target <- gsub(headerFooterRegex,
                 paste(header, src, '\n', footer, sep=''),
                 target)
  
  writeLines(target[[1]], targetFile)
  
  invisible()
}

deaugmentFile <- function(file, delete.if.empty=TRUE) {
  if (!file.exists(file))
    return()
  headerFooterRegex <- '# -- BEGIN PACKRAT --\\s*\n.*?# -- END PACKRAT --\\s*?(\n|$)'
  contents <- paste(readLines(file, warn=FALSE), collapse='\n')
  contents <- gsub(headerFooterRegex, '', contents)
  if (delete.if.empty && isTRUE(grepl('^[\r\n]*$', contents))) {
    unlink(file)
  } else {
    writeLines(contents, file)
  }
  return(invisible())
}

#' @export
libdir <- function(appDir = ".") {
  file.path(normalizePath(appDir, winslash='/', mustWork=TRUE), 'library', 
            R.version$platform, getRversion())
}

lockInfo <- function(appDir, property='packages', fatal=TRUE) {
  # Get and parse the lockfile
  lockFilePath <- file.path(appDir, "packrat.lock")
  if (!file.exists(lockFilePath)) {
    if (fatal) {
      stop(paste(lockFilePath, " is missing. Run packrat::bootstrap('",
                 appDir, "') to generate it.", sep = ""))
    } else {
      return(list())
    }
  }
  return(readLockFile(lockFilePath)[[property]])
}

