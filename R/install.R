# Given a package record, indicate the name we expect its source archive to have.
pkgSrcFilename <- function(pkgRecord) {
  if (identical(pkgRecord$source, "github"))
    paste(pkgRecord$gh_sha1, ".tar.gz", sep = "")
  else 
    paste(pkgRecord$name, "_", pkgRecord$version, ".tar.gz", sep = "")
}

# Given a package record, fetch the sources for the package and place them in 
# the source directory root given by sourceDir.
getSourceForPkgRecord <- function(pkgRecord, sourceDir, availablePkgs, repos, 
                                  sourcePackages, quiet = FALSE) {
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
  pkgSrcFile <- pkgSrcFilename(pkgRecord)
  if (file.exists(file.path(pkgSrcDir, pkgSrcFile))) 
    return(NULL)

  if (!quiet) {
    message("Fetching sources for ", pkgRecord$name, " (", pkgRecord$version, 
            ") ... ", appendLF = FALSE)
  }
  type <- pkgRecord$source

  # If this is a local source path, compress the local sources rather than 
  # trying to download from an external source
  if (identical(pkgRecord$source, "source")) {
    # R's tar command preserves paths relative to the current directory in
    # the archive, so temporarily set the working directory there while
    # we create the tarball
    (function() { 
      wd <- getwd()
      on.exit(setwd(wd), add = TRUE)
      setwd(file.path(pkgRecord$source_path, ".."))
      tar(file.path(pkgSrcDir, pkgSrcFile), files = pkgRecord$name, 
          compression = "gzip", tar = "internal")
    })()
    type <- "local"
  } else if ((identical(pkgRecord$source, "CRAN") ||
              identical(pkgRecord$source, "Bioconductor")) &&
             pkgRecord$name %in% rownames(availablePkgs)) {
    currentVersion <- availablePkgs[pkgRecord$name,"Version"]
    # Is the source for this version of the package on CRAN and/or a 
    # Bioconductor repo? 
    if (identical(pkgRecord$version, currentVersion)) {
      # Get the source package
      download.packages(pkgRecord$name, destdir = pkgSrcDir, 
                        available = availablePkgs, repos = repos, 
                        type = "source", quiet = TRUE)
      type <- paste(type, "current")
    } else {
      # The version requested is not the version on CRAN; it may be an 
      # older version. Look for the older version in the CRAN archive for
      # each named repository.
      foundVersion <- FALSE
      for (repo in repos) {
        tryCatch({
          archiveUrl <- file.path(repo, "src/contrib/Archive", 
                                  pkgRecord$name, 
                                  pkgSrcFile)
          download.file(archiveUrl, file.path(pkgSrcDir, pkgSrcFile), 
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
             pkgRecord$name, " (", currentVersion, " is current)")
      }
      type <- paste(type, "archive")      
    }
  } else if (identical(pkgRecord$source, "github")) {
    archiveUrl <- paste("http://github.com/", pkgRecord$gh_username, "/", 
                        pkgRecord$gh_repo, "/archive/", pkgSrcFile,
                        sep = "")
    request <- httr::GET(archiveUrl)
    httr::stop_for_status(request)
    
    srczip <- tempfile(fileext='.zip')
    on.exit({
      if (file.exists(srczip))
        unlink(srczip, recursive=TRUE)
    })
    
    writeBin(httr::content(request), srczip)

    local({
      scratchDir <- tempfile()
      on.exit({
        if (file.exists(scratchDir))
          unlink(scratchDir, recursive=TRUE)
      })
      untar(srczip, exdir=scratchDir)
      # Find the base directory
      basedir <- if (length(dir(scratchDir)) == 1)
        file.path(scratchDir, dir(scratchDir))
      else
        scratchDir
      
      if (!file.exists(file.path(basedir, 'DESCRIPTION'))) {
        stop('No DESCRIPTION file was found in the archive for ', pkgRecord$name)
      }
      
      ghinfo <- data.frame(
        GithubRepo = pkgRecord$gh_repo,
        GithubUsername = pkgRecord$gh_username,
        GithubRef = pkgRecord$gh_ref,
        GithubSHA1 = pkgRecord$gh_sha1
      )
      if (!ends_with_newline(file.path(basedir, 'DESCRIPTION')))
        cat('\n', sep='', file=file.path(basedir, 'DESCRIPTION'), append=TRUE)
      write.dcf(ghinfo, file.path(basedir, 'DESCRIPTION'), append = TRUE)
      
      file.create(file.path(pkgSrcDir, pkgSrcFile))
      dest <- normalizePath(file.path(pkgSrcDir, pkgSrcFile))

      oldDir <- getwd()
      on.exit(setwd(oldDir))
      setwd(basedir)
      
      tar(tarfile=dest, files='.', compression='gzip', tar='internal')
    })
    
    type <- "Github"
  }
  if (!quiet) {
    message("OK (", type, ")")
  }
}

# check whether the specified file ends with newline
ends_with_newline <- function(path) {
  conn <- file(path, open = "rb", raw = TRUE)
  on.exit(close(conn))
  seek(conn, where = -1, origin = "end")
  lastByte <- readBin(conn, "raw", n = 1)
  lastByte == 0x0a
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
      getSourceForPkgRecord(pkgRecord, file.path(appDir, "packrat.sources"),
                            availblePkgs, repos, quiet = TRUE)
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

