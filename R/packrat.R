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
  
  sourcePackages <- getSourcePackageInfo(sourcePackagePaths)

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
  snapshotImpl(appDir, available.packages(contrib.url(repos)),
               sourcePackages=sourcePackages, lib.loc = NULL)
  
  # Use the lockfile to copy sources and install packages to the library
  restore(appDir)
  
  # Write the .Rprofile and .Renviron files
  packify(appDir)
  
  invisible()
}

#' @export
restore <- function(appDir = '.') {
  appDir <- normalizePath(appDir)
  
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

#' @keywords internal
prettyPrintPair <- function(packagesFrom, packagesTo, header, footer = NULL) {
  if (length(packagesFrom) > 0) {
    if (any(pkgNames(packagesFrom) != pkgNames(packagesTo)))
      stop('Invalid arguments--package records list mistmatch')
    
    cat('\n')
    if (!is.null(header)) {
      cat(paste(header, collapse=''))
      cat('\n')
    }
    
    df <- data.frame(from = paste(" ", sapply(packagesFrom, pick("version"))),
                     to = paste(" ", sapply(packagesTo, pick("version"))))
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
clean <- function(appDir = getwd(), lib.loc = libdir(appDir),
                  prompt = interactive()) {

  rootDeps <- appDependencies(appDir)
  packagesInUse <- getPackageRecords(rootDeps, available=NULL,
                                     sourcePackages=NULL,
                                     recursive=TRUE,
                                     lib.loc=lib.loc)
  
  installedPkgNames <- row.names(installed.packages(
    lib.loc=lib.loc, priority='NA', noCache=TRUE))
  
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
            getRversion())
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