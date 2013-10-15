snapshot <- function(appDir, repos, sourcePackages) {  
  # Compute the package dependency information from the DESCRIPTION and write 
  # the lock file
  description <- getDescription(appDir)
  packageDependencies <- 
    strsplit(as.character(description$Depends), "\\s*,\\s*")[[1]]
  
  # Every package implicitly depends on packrat itself 
  if (!("packrat" %in% packageDependencies))
    packageDependencies <- c(packageDependencies, "packrat")
  
  writeLockFile(file.path(appDir, "packrat.lock"),
                getPackageRecords(packageDependencies, 
                                  available.packages(contrib.url(repos)), 
                                  sourcePackages))
}