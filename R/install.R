installCRAN <- function(repos, pkgRecords, lib) {
  # Get the list of available packages and the latest version of those packages
  # from the repositories
  availablePkgs <- available.packages(contrib.url(repos))
  
  # Process and install each package
  for (pkgRecord in pkgRecords) {
    # See if the version requested is the current version in the repository
    currentVersion <- availablePkgs[pkgRecord$name,][["Version"]]
    if (identical(pkgRecord$version, currentVersion)) {
      # The version requested is the current one; let install.packages handle it
      install.packages(pkgRecord$name, repos = repos, lib = lib, 
                       dependencies = FALSE)
    } else {
      # The version requested is not current; try each given repository to find
      # the sources for the older version
      foundVersion <- FALSE
      for (repo in repos) {
        tryCatch({
          srcPackageName <- paste(pkgRecord$name, "_", pkgRecord$version, 
                                  ".tar.gz", sep = "")  
          archiveUrl <- file.path(repo, "src/contrib/Archive", pkgRecord$name, 
                                  srcPackageName)
          devtools::install_url(archiveUrl, reload = FALSE, 
                                dependencies = FALSE, args = paste("-l", lib))
          foundVersion <- TRUE
          break
        }, error = function(e) {
          # Ignore error and try the next repository
        })
      }
      if (!foundVersion) {
        stop(paste("Couldn't find version ", pkgRecord$version, " of ",
                   pkgRecord$name, " (", currentVersion, " is current)", 
                   sep = ""))
      }
    }
  }
}
