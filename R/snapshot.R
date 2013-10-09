snapshot <- function(appDir) {
  # Get the DESCRIPTION file from the directory
  descFilePath <- file.path(appDir, "DESCRIPTION")
  if (!file.exists(descFilePath)) {
    stop(paste(descFilePath, " not found. Run packrat::bootstrap('", appDir, 
               "') to initialize packrat for this directory.", sep = ""))
  }
  packageInfo <- data.frame(read.dcf(descFilePath))
  
  # Compute the package dependency information and write the lock file
  packageDependencies <- 
    strsplit(as.character(packageInfo$Dependencies), ", ")[[1]]
  writeLockFile(file.path(appDir, "packrat.lock"),
                          getPackageRecords(packageDependencies))
}