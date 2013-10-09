#' Given an application directory, computes the application's dependencies, 
#' and places the application's dependencies under packrat control. 
#' 
#' @export
bootstrap <- function(appDir = getwd()) {
  dependencies <- data.frame(Source = getOption("repos")[[1]],
                             Dependencies = paste(appDependencies(appDir),
                                                  collapse=", "))
  write.dcf(dependencies, file = paste(appDir, "/DESCRIPTION", sep = ""))
  snapshot(appDir)
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
  # TODO: remove when Rprofile/Renviron is set up properly in bootstrap, so we
  # can just rely on .libPaths
  libDir <- file.path(appDir, "library")
  if (!file.exists(libDir)) {
    dir.create(libDir)
  }
  
  # Install CRAN dependencies and then Github dependencies. 
  description <- getDescription(appDir)
  installCRAN(description$Source, installList, libDir)
  installGithub(installList, libDir)
}

pack <- function() {
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
