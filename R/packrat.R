#' Given an application directory, computes the application's dependencies, 
#' and places the application's dependencies under packrat control. 
#' 
#' @export
bootstrap <- function(appDir = '.', sourcePackagePaths = character()) {
  
  descriptionFile <- file.path(appDir, 'DESCRIPTION')
  
  if (file.exists(descriptionFile)) {
    description <- as.data.frame(read.dcf(descriptionFile))
    package <- description$Package
    if (!is.null(package)) {
      stop("This project appears to be an R package. Packrat doesn't work on ",
           "packages.")
    }
  }
  
  # Get the inferred set of dependencies
  inferredDependencies <- appDependencies(appDir)
  
  # Since source packages are manually specified, assume that they are 
  # dependencies even if we didn't detect R code that uses them
  sourcePackages <- getSourcePackageInfo(sourcePackagePaths)
  for (sourcePackage in rownames(sourcePackages)) {
    if (!(as.character(sourcePackage) %in% inferredDependencies)) {
      inferredDependencies <- c(inferredDependencies, 
                                as.character(sourcePackage))
    }
  }

  repos <- as.vector(getOption("repos"))
  
  # Check to see whether Bioconductor is installed. Bioconductor maintains a 
  # private set of repos, which we need to expose here so we can download 
  # sources to Bioconducter packages.
  if (isTRUE(nchar(find.package("BiocInstaller", quiet = TRUE)) > 0)) {
    # Bioconductor repos may include repos already accounted for above 
    repos <- unique(c(repos, as.vector(BiocInstaller::biocinstallRepos())))                   
  } 
  
  # Get the inferred set of dependencies and write the lockfile
  dependencies <- data.frame(Source = paste(repos, collapse=", "),
                             Depends = paste(inferredDependencies,
                                             collapse=", "),
                             Type = "Packrat Application")
  setDescription(appDir, dependencies)
  snapshot(appDir, getOption("repos"), sourcePackages)
  
  # Use the lockfile to copy sources and install packages to the library
  restore(appDir)
  
  # Write the .Rprofile and .Renviron files
  packify(appDir)
}

#' @export
restore <- function(appDir = '.') {
  appDir <- normalizePath(appDir)
  packages <- lockInfo(appDir)
  
  # Generate the list of packages to install
  installList <- makeInstallList(packages)
  
  # Make sure the library directory exists 
  libDir <- libdir(appDir)
  if (!file.exists(libDir)) {
    dir.create(libDir, recursive=TRUE)
  }
  
  # Snapshot the sources for each package, then install them in turn from CRAN
  # or github, from binaries when available and then from sources.
  description <- getDescription(appDir)
  repos <- strsplit(as.character(description$Source), '\\s*,\\s*')[[1]]
  snapshotSources(appDir, repos, installList)
  installPkgs(appDir, repos, installList, libDir)    
}

#' @export
status <- function(appDir = '.', lib.loc = libdir(appDir), quiet = FALSE) {
  packages <- lockInfo(appDir)
  recsLock <- flattenPackageRecords(packages)
  namesLock <- pkgNames(recsLock)
  
  installedList <- as.vector(installed.packages(libdir(appDir))[,'Package'])
  recsLib <- flattenPackageRecords(getPackageRecords(installedList, NULL, lib.loc=lib.loc))
  namesLib <- pkgNames(recsLib)
  
  onlyLock <- recsLock[!recsLock %in% recsLib]
  onlyLib <- recsLib[!recsLib %in% recsLock]
  
  list(onlyLock = onlyLock, onlyLib = onlyLib)
  
  if (!isTRUE(quiet)) {
    prettyPrint(onlyLock,
                'Packrat thinks these packages are missing from your library:',
                'You can install them using "packrat::install()".')
  }
  
  dirDeps <- dirDependencies(appDir)
  recsDir <- flattenPackageRecords(getPackageRecords(dirDeps, NULL, lib.loc=lib.loc, fatal=FALSE))
  namesDir <- pkgNames(recsDir)
  
  # What packages are missing from packrat, but present in the source?
  libsInSourceIndex <- pkgNames(onlyLib) %in% pkgNames(recsDir)
  probablyInstall <- onlyLib[libsInSourceIndex]
  # What packages are missing from packrat and not present in the source?
  probablyRemove <- onlyLib[!libsInSourceIndex]
  
  if (!isTRUE(quiet)) {
    prettyPrint(
      probablyInstall,
      'These packages are installed and used in your R scripts, but\nmissing from packrat:',
      'You can add them with "packrat::add()".')
    
    prettyPrint(
      probablyRemove,
      'These packages are installed, but not used in your R scripts,\nand not present in packrat:',
      'You can remove them with "packrat::remove()".')
  }
  
  onlyInSource <- recsDir[!(namesDir %in% namesLib | namesDir %in% namesLock)]
  if (!isTRUE(quiet)) {
    prettyPrint(
      onlyInSource,
      'These packages are in neither packrat nor your library, but\nthey appear to be used in your R scripts.',
      'You can try to install the latest versions from CRAN using\n"packrat::bootstrap()". Or, install them manually, and then\nrun "packrat::add()".')
  }
  
  result <- list(
    PackratOnly = onlyLock,
    LibraryOnly = onlyLib,
    SourceOnly = onlyInSource,
    ProbablyInstall = probablyInstall,
    ProbablyRemove = probablyRemove
  )
  
  # If any of the lists have length > 0, return result; otherwise NULL
  if (any(sapply(result, length) > 0)) {
    return(invisible(result))
  } else {
    return(invisible())
  }
}

#' @keywords internal
prettyPrint <- function(packages, header, footer = NULL) {
  if (length(packages) > 0) {
    cat('\n')
    if (!is.null(header)) {
      cat(paste(header, collapse=''))
      cat('\n')
    }
    print.simple.list(lapply(packages, function(pkg) {
      result <- ifelse(is.na(pkg$version), '', pkg$version)
      names(result) <- paste("   ", pkg$name)
      result
    }))
    if (!is.null(footer)) {
      cat(paste(footer, collapse=''))
    }
    cat('\n')
  }
}

#' @export
add <- function(packages = NULL, appDir = '.', lib.loc = NULL,
                repos = getOption("repos"), allow.downgrade = FALSE,
                sourcePackagePaths = character(0)) {
  needsSnapshot <- any(sapply(packages, function(pkg) {
    addPackage(pkg, appDir, lib.loc)
  }))
  
  if (!needsSnapshot) {
    message('No changes were made.')
  } else {
    cat('Rebuilding snapshot\n')
    sourcePackages <- getSourcePackageInfo(sourcePackagePaths)
    snapshot(appDir=appDir, repos=repos, sourcePackages=sourcePackages)
  }
  return(invisible(needsSnapshot))
}

#' @keywords internal
addPackage <- function(package, appDir, lib.loc, allow.downgrade) {
  # TODO: Check if package is a base package and stop?
  
  packages <- lockInfo(appDir)
  pkgInfo <- searchPackages(packages, package)[[1]]
  
  # In library?
  if (isTRUE(nchar(find.package(package, lib.loc=lib.loc, quiet=TRUE)) > 0)) {
    # Check if installed copy of package has all its dependencies
    recLib <- getPackageRecords(package, NULL, recursive=TRUE, lib.loc=lib.loc,
                                fatal=TRUE)

    # In packrat?
    if (is.null(pkgInfo)) {
      # In library, not in packrat
      addDependencies(appDir, package)
      return(invisible(TRUE))
    } else {
      # In library, and also in packrat.
      # Compare versions to see what to do next.
      installedVer <- as.character(packageVersion(package, lib.loc=lib.loc))
      comp <- compareVersion(installedVer, pkgInfo$version)
      if (comp == 0) {
        # Already consistent. No work was required.
        return(invisible(FALSE))
      } else if (comp > 0) {
        # It's an upgrade; don't change the description, but update the lockfile
        return(invisible(TRUE))
      } else { # comp < 0
        if (!isTRUE(allow.downgrade)) {
          warning(
            'The installed version of package "', package, '" is earlier\n',
            'than the one in the lockfile. Run restore() to upgrade the\n',
            'library, or, if you really want to downgrade, re-run the ',
            'add() function with allow.downgrade = TRUE.')
          return(invisible(FALSE))
        } else {
          # It's a downgrade; update the lockfile
          return(invisible(TRUE))
        }
      }
    }
  } else {
    # Not in library
    if (is.null(pkgInfo)) {
      # In neither packrat nor library
      warning(
        'The package "', package, '" is not installed. Please install it\n',
        'and then run add() again.')
      return(invisible(FALSE))
    } else {
      # In packrat, but not library
      warning(
        'The package "', package, '" is not installed in your library,\n',
        'but it is present in packrat. If you meant to install it into\n',
        'your library using the information in packrat, then use the\n',
        'install() command instead.')
      return(invisible(FALSE))
    }
  }
}

clean <- function(appDir = getwd()) {
  # Clean up dependency information
  unlink(file.path(appDir, "DESCRIPTION"))
  unlink(file.path(appDir, "packrat.lock"))
  
  # Clean up downloaded sources and library directories
  unlink(file.path(appDir, "library"), recursive = TRUE)
  unlink(file.path(appDir, "library.old"), recursive = TRUE)
  unlink(file.path(appDir, "library.new"), recursive = TRUE)
  unlink(file.path(appDir, "packrat.sources"), recursive = TRUE)
}

#' Install .Rprofile and .Renviron files in the given directory to make it
#' use a private package library.
#' 
#' @export
packify <- function(dir = '.') {
  dir <- normalizePath(dir, TRUE)
  rprofile <- file.path(dir, '.Rprofile')
  renviron <- file.path(dir, '.Renviron')
  
  augmentFile(system.file('Rprofile', package='packrat'), rprofile, TRUE)
  augmentFile(system.file('Renviron', package='packrat'), renviron, FALSE)
  
  message('Packrat startup directives installed. Please quit and restart your R session.')
  
  invisible()
}

#' Add the contents of srcFile into targetFile, with "magic" comments bracketing
#' the contents. Should be safe to run multiple times--each subsequent call will
#' replace the contents between magic comments.
#' 
#' @keywords internal
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

#' @export
libdir <- function(appDir) {
  file.path(normalizePath(appDir), 'library', R.version$platform, 
            getRversion()[1,1:2])
}

lockInfo <- function(appDir) {
  # Get and parse the lockfile
  lockFilePath <- file.path(appDir, "packrat.lock")
  if (!file.exists(lockFilePath)) {
    stop(paste(lockFilePath, " is missing. Run packrat::bootstrap('",
               appDir, "') to generate it.", sep = ""))
  }
  return(readLockFile(lockFilePath))
}