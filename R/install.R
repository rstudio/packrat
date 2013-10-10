pkgSrcFilename <- function(pkgRecord) {
  if (identical(pkgRecord$source, "CRAN"))
    paste(pkgRecord$name, "_", pkgRecord$version, ".tar.gz", sep = "")
  else if (identical(pkgRecord$source, "github"))
    paste(pkgRecord$gh_sha1, ".zip", sep = "")
}

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
    
    if (identical(pkgRecord$source, "CRAN")) {
      currentVersion <- availablePkgs[pkgRecord$name,][["Version"]]
      # Is the source for this version of the package on CRAN?
      if (identical(pkgRecord$version, currentVersion)) {
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
            srcPackageName <- pkgSrcFilename(pkgRecord)
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
      srcPackageName <- pkgSrcFilename(pkgRecord)
      archiveUrl <- paste("http://github.com/", pkgRecord$gh_username, "/", 
                          pkgRecord$gh_repo, "/archive/", srcPackageName,
                          sep = "")
      request <- httr::GET(archiveUrl)
      httr::stop_for_status(request)
      writeBin(content(request), file.path(pkgSrcDir, srcPackageName))
    }
  }
}

installPkgs <- function(appDir, repos, pkgRecords, lib) {
  # Get the list of available packages and the latest version of those packages
  # from the repositories
  availablePkgs <- available.packages(contrib.url(repos))
  
  # Process and install each package
  for (pkgRecord in pkgRecords) {
    if (identical(pkgRecord$source, "CRAN") && 
        identical(pkgRecord$version, 
                  availablePkgs[pkgRecord$name,][["Version"]])) {
      # When installing the current version of a package from CRAN, just let
      # install.packages do its thing (it may install a pre-built binary)
      install.packages(pkgRecord$name, repos = repos, lib = lib, 
                       dependencies = FALSE)
    } else {
      # When installing from github or an older version, use the cached source
      # tarball or zip created in snapshotSources
      pkgSrc = file.path(appDir, "packrat.sources", pkgRecord$name, 
                         pkgSrcFilename(pkgRecord))
      install.packages(pkgSrc, lib = lib, repos = NULL, dependencies = FALSE,
                       type = "source")
    } 
  }
}

