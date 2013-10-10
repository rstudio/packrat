snapshot <- function(appDir) {  
  # Compute the package dependency information from the DESCRIPTION and write 
  # the lock file
  description <- getDescription(appDir)
  packageDependencies <- 
    strsplit(as.character(description$Dependencies), "\\s*,\\s*")[[1]]
  
  # Every package implicitly depends on packrat itself 
  packageDependencies <- c(packageDependencies, "packrat")
  
  writeLockFile(file.path(appDir, "packrat.lock"),
                getPackageRecords(packageDependencies, available.packages()))
}