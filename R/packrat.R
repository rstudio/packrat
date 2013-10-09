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
  installCRANList <- lapply(installList, function(pkgRecord) {
    if (identical(pkgRecord$source, "CRAN")) pkgRecord else NULL    
  })
  
  # Make sure the library directory exists 
  # TODO: remove when Rprofile/Renviron is set up properly in bootstrap, so we
  # can just rely on .libPaths
  libDir <- file.path(appDir, "library")
  if (!file.exists(libDir)) {
    dir.create(libDir)
  }
  
  # Install CRAN dependencies
  description <- getDescription(appDir)
  installCRAN(description$Source, installCRANList, libDir)
}

pack <- function() {
}
