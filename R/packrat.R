#' Given an application directory, computes the application's dependencies, 
#' and places the application's dependencies under packrat control. 
#' 
#' @export
bootstrap <- function(appDir = '.', sourcePackagePaths = character()) {
  
  descriptionFile <- file.path(appDir, 'DESCRIPTION')
  
  if (file.exists(descriptionFile)) {
    description <- as.data.frame(read.dcf(descriptionFile))
    type <- description$Type
    if (is.null(type) || identical(tolower(type), 'package')) {
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
  
  # Get the inferred set of dependencies and write the lockfile
  dependencies <- data.frame(Source = getOption("repos")[[1]],
                             Dependencies = paste(inferredDependencies,
                                                  collapse=", "))
  write.dcf(dependencies, file = descriptionFile)
  snapshot(appDir, getOption("repos"), sourcePackages)
  
  # Use the lockfile to copy sources and install packages to the library
  install(appDir)
  
  # Write the .Rprofile and .Renviron files
  packify(appDir)
}

install <- function(appDir = getwd()) {
  # Get and parse the lockfile
  lockFilePath <- file.path(appDir, "packrat.lock")
  if (!file.exists(lockFilePath)) {
    stop(paste(lockFilePath, " is missing. Run packrat::bootstrap('",
               appDir, "') to generate it.", sep = ""))
  }
  packages <- readLockFile(lockFilePath)
  
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
  snapshotSources(appDir, description$Source, installList)
  installPkgs(appDir, description$Source, installList, libDir)    
}

#' @export
status <- function(appDir = '.', lib.loc = NULL, quiet = FALSE) {
  lockFilePath <- file.path(appDir, "packrat.lock")
  if (!file.exists(lockFilePath)) {
    stop(lockFilePath, " is missing; this project doesn't seem to use packrat.")
  }
  packages <- readLockFile(lockFilePath)
  recsLock <- flattenPackageRecords(packages)
  
  installedList <- as.vector(installed.packages(libdir(appDir))[,'Package'])
  recsLib <- flattenPackageRecords(getPackageRecords(installedList, NULL, lib.loc=lib.loc))
  
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
  
  # What packages are missing from packrat, but present in the source?
  libsInSourceIndex <- onlyLib %in% recsDir
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
  
  onlyInSource <- recsDir[!(recsDir %in% recsLib | recsDir %in% recsLock)]
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

#' @keyword internal
flattenPackageRecords <- function(packageRecords) {
  visited <- new.env(parent=emptyenv())
  visit <- function(pkgRecs) {
    for (rec in pkgRecs) {
      visit(rec$depends)
      rec['depends'] <- NULL
      visited[[rec$name]] <- rec
    }
  }
  visit(packageRecords)
  lapply(sort(ls(visited)), function(name) {
    visited[[name]]
  })
}

pack <- function() {
}

clean <- function(appDir = getwd()) {
  # Clean up dependency information
  unlink(file.path(appDir, "DESCRIPTION"))
  unlink(file.path(appDir, "packrat.lock"))
  
  # Clean up downloaded sources and library directories
  unlink(libdir(appDir), recursive = TRUE)
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

libdir <- function(appDir) {
  file.path(normalizePath(appDir), 'library', R.version$platform, 
            getRversion()[1,1:2])
}
