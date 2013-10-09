snapshotSources <- function(appDir, repos, pkgRecords) {
  # Get a list of source packages available on the repositories
  availablePkgs <- available.packages(contrib.url(repos, "source"), 
                                      type = "source")

  # Clean the source directory if it exists and recreate it
  sourceDir <- file.path(appDir, "packrat.sources")
  unlink(sourceDir, recursive = TRUE)
  dir.create(sourceDir)
  
  # Get the sources for each package
  for (pkgRecord in pkgRecords) {
    # Create the directory in which to place this package's sources
    pkgSrcDir <- file.path(sourceDir, pkgRecord$name)
    dir.create(pkgSrcDir)
    
    if (pkgRecord$source == "CRAN") {
      # Is the source for this version of the package on CRAN?
      if (identical(pkgRecord$version, 
          availablePkgs[pkgRecord$name,][["Version"]])) {
        # Get the source package from CRAN
        download.packages(pkgRecord$name, destdir = pkgSrcDir, 
                          available = availablePkgs, repos = repos, 
                          type = "source")
      } else {
        # The version requested is not the version on CRAN; it may be an 
        # older version. Look for the older version in the CRAN archive for
        # each named repository.
        foundVersion <- FALSE
        for (repo in repos) {
          tryCatch({
            srcPackageName <- paste(pkgRecord$name, "_", pkgRecord$version, 
                                    ".tar.gz", sep = "")
            archiveUrl <- file.path(repo, "src/contrib/Archive", pkgRecord$name, 
                                    srcPackageName)
            download.file(archiveUrl, file.path(pkgSrcDir, srcPackageName), 
                          mode = "wb")
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
    } else if (identical(pkgRecord$source, "github")) {
      srcPackageName <- paste(pkgRecord$gh_sha1, ".zip", sep = "")
      archiveUrl <- paste("http://github.com/", pkgRecord$gh_username, "/", 
                          pkgRecord$gh_repo, "/archive/", srcPackageName,
                          sep = "")
      request <- httr::GET(archiveUrl)
      httr::stop_for_status(request)
      writeBin(content(request), file.path(pkgSrcDir, srcPackageName))
    }
  }
}

installPkgs <- function(repos, pkgRecords, lib) {
  # Get the list of available packages and the latest version of those packages
  # from the repositories
  availablePkgs <- available.packages(contrib.url(repos))
  
  # Process and install each package
  for (pkgRecord in pkgRecords) {
    if (!identical(pkgRecord$source, "CRAN")) {
      # Package is not from CRAN, ignore it
      next
    }
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

installGithub <- function(pkgRecords, lib){
  # Process and install each package
  for (pkgRecord in pkgRecords) {
    if (!identical(pkgRecord$source, "github")) {
      # Package is not from Github, ignore it
      next
    }
    devtools::install_github(pkgRecord$gh_repo, 
                             username = pkgRecord$gh_username,
                             ref = pkgRecord$gh_sha1, args = paste("-l", lib))
  }
}
