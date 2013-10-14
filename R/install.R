# Given a package record, indicate the name we expect its source archive to have.
pkgSrcFilename <- function(pkgRecord) {
  if (identical(pkgRecord$source, "github"))
    paste(pkgRecord$gh_sha1, ".zip", sep = "")
  else 
    paste(pkgRecord$name, "_", pkgRecord$version, ".tar.gz", sep = "")
}

# Given a package record, fetch the sources for the package and place them in 
# the source directory root given by sourceDir.
getSourceForPkgRecord <- function(pkgRecord, sourceDir, availablePkgs, repos, 
                                  quiet = FALSE) {
  # Skip packages for which we can't find sources
  if (is.null(pkgRecord$source) ||
      is.na(pkgRecord$source)) {
    if (!quiet) {
      warning("Couldn't determine source for ", pkgRecord$name, " (", 
              pkgRecord$version, ")")
    }
    return(NULL)
  }
  
  # Create the directory in which to place this package's sources
  pkgSrcDir <- file.path(sourceDir, pkgRecord$name)
  if (!file.exists(pkgSrcDir))    
    dir.create(pkgSrcDir, recursive = TRUE)
  
  # If the file we want to download already exists, skip it
  if (file.exists(file.path(pkgSrcDir, pkgSrcFilename(pkgRecord)))) 
    return(NULL)

  if (!quiet) {
    message("Fetching sources for ", pkgRecord$name, " (", pkgRecord$version, 
            ") ... ", appendLF = FALSE)
  }
  type <- "current"

  if (identical(pkgRecord$source, "CRAN") &&
      pkgRecord$name %in% rownames(availablePkgs)) {
    currentVersion <- availablePkgs[pkgRecord$name,"Version"]
    # Is the source for this version of the package on CRAN?
    if (identical(pkgRecord$version, currentVersion)) {
        # Get the source package from CRAN
        download.packages(pkgRecord$name, destdir = pkgSrcDir, 
                          available = availablePkgs, repos = repos, 
                          type = "source", quiet = TRUE)
      } else {
        # The version requested is not the version on CRAN; it may be an 
        # older version. Look for the older version in the CRAN archive for
        # each named repository.
        foundVersion <- FALSE
        for (repo in repos) {
          tryCatch({
            srcPackageName <- pkgSrcFilename(pkgRecord)
            archiveUrl <- file.path(repo, "src/contrib/Archive", 
                                    pkgRecord$name, 
                                    srcPackageName)
            download.file(archiveUrl, file.path(pkgSrcDir, srcPackageName), 
                          mode = "wb", quiet = TRUE)
            foundVersion <- TRUE
            type <- "archived"
            break
          }, error = function(e) {
            # Ignore error and try the next repository
          })
        }
        if (!foundVersion) {
          stop("Couldn't find source for version ", pkgRecord$version, " of ",
               pkgRecord$name, " (", currentVersion, ") is current)")
        }
      }
    } else if (identical(pkgRecord$source, "github")) {
      srcPackageName <- pkgSrcFilename(pkgRecord)
      archiveUrl <- paste("http://github.com/", pkgRecord$gh_username, "/", 
                          pkgRecord$gh_repo, "/archive/", srcPackageName,
                          sep = "")
      request <- httr::GET(archiveUrl)
      httr::stop_for_status(request)
      writeBin(httr::content(request), file.path(pkgSrcDir, srcPackageName))
      type <- "Github"
    }
  if (!quiet) {
    message("OK (", type, ")")
  }
}

snapshotSources <- function(appDir, repos, pkgRecords) {
  # Get a list of source packages available on the repositories
  availablePkgs <- available.packages(contrib.url(repos, "source"), 
                                      type = "source")

  # Find the source directory (create it if necessary)
  sourceDir <- file.path(appDir, "packrat.sources")
  if (!file.exists(sourceDir))
    dir.create(sourceDir)
  
  # Get the sources for each package
  lapply(pkgRecords, function(pkgRecord) { 
    getSourceForPkgRecord(pkgRecord, sourceDir, availablePkgs, repos)
  })
}

installPkgs <- function(appDir, repos, pkgRecords, lib) {
  # Get the list of available packages and the latest version of those packages
  # from the repositories
  availablePkgs <- available.packages(contrib.url(repos))
  
  # Process and install each package
  for (pkgRecord in pkgRecords) {
    pkgSrc <- NULL
    type <- "source"
    
    message("Installing ", pkgRecord$name, " (", pkgRecord$version, ") ... ", 
            appendLF = FALSE)
    
    # Generally we want to install from sources, but we will download a pre-
    # built binary if (a) the package exists on CRAN, (b) the version on CRAN
    # is the version desired, and (c) R is set to download binaries.
    if (identical(pkgRecord$source, "CRAN") && 
        identical(pkgRecord$version, 
                  availablePkgs[pkgRecord$name,"Version"]) &&
        !identical(getOption("pkgType"), "source")) {
      tempdir <- tempdir()
      tryCatch ({
        downloaded <- download.packages(pkgRecord$name, destdir = tempdir, 
                                        repos = repos, 
                                        available = availablePkgs, quiet = TRUE)
        if (length(downloaded) > 1) {
          pkgSrc <- downloaded[2]
          type <- "binary"
        }
      }, error = function(e) {
        # Do nothing here, we'll try local sources if we fail to download from
        # the repo
      })  
    } 
    if (is.null(pkgSrc)) {
      # When installing from github or an older version, use the cached source
      # tarball or zip created in snapshotSources
      pkgSrc <- file.path(appDir, "packrat.sources", pkgRecord$name, 
                          pkgSrcFilename(pkgRecord))      
    } 
    if (!file.exists(pkgSrc)) {
      # If the source file is missing, try to download it. (Could happen in the
      # case where the packrat lockfile is present but cached sources are 
      # missing.)
      getSourceForPkgRecord(pkgRecord, quiet = TRUE)
      if (!file.exists(pkgSrc)) {
        stop("Failed to install ", pkgRecord$name, " (", pkgRecord$version, ")",
             ": sources missing at ", pkgSrc)
      }
    }
    devtools::install_local(path = pkgSrc, reload = FALSE, 
                            args = paste("-l", lib), dependencies = FALSE,
                            quick = TRUE, quiet = TRUE)
    message("OK (", type, ")")
  }
}

