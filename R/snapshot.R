snapshot <- function(appDir) {  
  # Compute the package dependency information from the DESCRIPTION and write 
  # the lock file
  description <- getDescription(appDir)
  packageDependencies <- 
    strsplit(as.character(description$Dependencies), ", ")[[1]]
  writeLockFile(file.path(appDir, "packrat.lock"),
                getPackageRecords(packageDependencies))
}