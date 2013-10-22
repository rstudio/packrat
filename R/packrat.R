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
  
  # Get the inferred set of dependencies
  inferredDependencies <- appDependencies(appDir)
  
  sourcePackages <- getSourcePackageInfo(sourcePackagePaths)

  # Get the inferred set of dependencies and write the lockfile
  repos <- activeRepos()
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
  
  # Install each package from CRAN or github, from binaries when available and 
  # then from sources.
  description <- getDescription(appDir)
  repos <- strsplit(as.character(description$Source), '\\s*,\\s*')[[1]]
  installPkgs(appDir, repos, installList, libDir)    
}

#' @export
status <- function(appDir = '.', lib.loc = libdir(appDir), quiet = FALSE) {
  appDir <- normalizePath(appDir, winslash='/', mustWork=TRUE)

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
clean <- function(appDir = ".", lib.loc = libdir(appDir),
                  prompt = interactive()) {

  appDir <- normalizePath(appDir, winslash='/', mustWork=TRUE)

  rootDeps <- appDependencies(appDir)
  packagesInUse <- getPackageRecords(rootDeps, available=NULL,
                                     sourcePackages=NULL,
                                     recursive=TRUE,
                                     lib.loc=lib.loc)
  
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
  
  files <- c("DESCRIPTION", "packrat.lock")
  dirs <- c("library", "library.old", "library.new", "packrat.sources")
  
  # Clean up dependency information
  unlink(file.path(appDir, files))
  # Clean up downloaded sources and library directories
  unlink(file.path(appDir, dirs), recursive = TRUE)
  
  return(invisible())
}

#' Install .Rprofile and .Renviron files in the given directory to make it
#' use a private package library.
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
libdir <- function(appDir) {
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

