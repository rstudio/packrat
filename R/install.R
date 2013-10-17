# Given a package record, indicate the name we expect its source archive to have.
pkgSrcFilename <- function(pkgRecord) {
  if (identical(pkgRecord$source, "github"))
    paste(pkgRecord$gh_sha1, ".tar.gz", sep = "")
  else 
    paste(pkgRecord$name, "_", pkgRecord$version, ".tar.gz", sep = "")
}

# Given a package record, indicate whether the package exists on a CRAN-like
# repository. 
isFromCranlikeRepo <- function(pkgRecord) {
  identical(pkgRecord$source, "CRAN") || 
  identical(pkgRecord$source, "Bioconductor")
}

# Given a package record and a database of packages, check to see if
# the package version is current. NB: Assumes package is present in db. 
versionMatchesDb <- function(pkgRecord, db) {
  versionMatch <- 
    identical(pkgRecord$version, db[pkgRecord$name,"Version"])
  if (versionMatch && identical(pkgRecord$source, "github")) {
    # For Github, we also need to check that the SHA1 is identical (the source
    # may be updated even if the version hasn't been bumped)
    pkgDescFile <- system.file('DESCRIPTION', package = pkgRecord$name)
    installedPkgRecord <- 
      inferPackageRecord(as.data.frame(read.dcf(pkgDescFile)))
    versionMatch <- identical(pkgRecord$gh_sha1, 
                              installedPkgRecord$gh_sha1)
  }
  versionMatch
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
  } else if (isFromCranlikeRepo(pkgRecord) &&
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
          type <- paste(type, "archived")
          break
        }, error = function(e) {
          # Ignore error and try the next repository
        })
      }
      if (!foundVersion) {
        stop("Couldn't find source for version ", pkgRecord$version, " of ",
             pkgRecord$name, " (", currentVersion, " is current)")
      }
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
      appendToDcf(file.path(basedir, 'DESCRIPTION'), ghinfo)
      
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

# Installs a single package from its record. Returns the method used to install
# the package (built source, downloaded binary, etc.)
installPkg <- function(pkgRecord, appDir, availablePkgs, lib = libdir(appDir)) {
  pkgSrc <- NULL
  type <- "built source"
  
  # Generally we want to install from sources, but we will download a pre-
  # built binary if (a) the package exists on CRAN, (b) the version on CRAN
  # is the version desired, and (c) R is set to download binaries.
  if (isFromCranlikeRepo(pkgRecord) && 
      pkgRecord$name %in% rownames(availablePkgs) &&
      versionMatchesDb(pkgRecord, availablePkgs) &&
      !identical(getOption("pkgType"), "source")) {
    tempdir <- tempdir()
    tryCatch ({
      downloaded <- download.packages(pkgRecord$name, destdir = tempdir, 
                                      repos = repos, 
                                      available = availablePkgs, quiet = TRUE)
      if (length(downloaded) > 1) {
        pkgSrc <- downloaded[2]
        type <- "downloaded binary"
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
  
  # Annotate DESCRIPTION file so we know we installed it
  descFile <- file.path(lib, pkgRecord$name, 'DESCRIPTION')
  appendToDcf(descFile, data.frame(
    InstallAgent=paste('packrat', packageVersion('packrat'))
  ))
  
  return(type)  
}

playInstallActions <- function(pkgRecords, actions, repos, appDir, lib) {
  # Get the list of available packages and the latest version of those packages
  # from the repositories, and the local install list for comparison 
  availablePkgs <- available.packages(contrib.url(repos))

  # If this is the initial snapshot, we can optimize the installation of
  # packages from the global library to the private Packrat library
  initialSnapshot <- !identical(.libPaths()[1], lib)
  installedPkgs <- installed.packages(priority = "NA")
  
  by(actions, 1:nrow(actions), function(action) {
    foundPkg <- FALSE
    pkgName <- row.names(action)
    for (pkgRecord in pkgRecords) {
      if (identical(pkgRecord$name, pkgName)) {
        if (initialSnapshot) {
          # If taking the initial snapshot and installing a version to the 
          # private library that matches the version in the global library,
          # short-circuit and do a copy here. 
          if (identical(action$action, "install") &&
              pkgName %in% rownames(installedPkgs) &&
              versionMatchesDb(pkgRecord, installedPkgs)) {
            message("Installing ", pkgRecord$name, " (", pkgRecord$version,
                    ") ...", appendLF = FALSE)
            file.copy(find.package(pkgRecord$name), lib, recursive = TRUE)
            message("OK (copied local binary)")
            foundPkg <- TRUE
            break
          }
        }
        if (identical(action$action, "replace")) {
          message("Replacing ", pkgRecord$name, " (", 
                  installedPkgs[pkgName,"Version"], " to ", pkgRecord$version, 
                  ") ...", appendLF = FALSE)
          remove.packages(action$package, lib = lib)
        } else if (identical(action$action, "install")) {
          message("Installing ", pkgRecord$name, " (", pkgRecord$version,
                  ") ...", appendLF = FALSE)
        }
        type <- installPkg(pkgRecord, appDir, availablePkgs) 
        message("OK (", type, ")")
        foundPkg <- TRUE
        break
      }
    }
    if (!foundPkg) {
      warning("Could not ", actions$action, " package ", as.character(action), 
              " because it is missing from the lockfile.")
    }
  })
  invisible()
}

installPkgs <- function(appDir, repos, pkgRecords, lib) {
  installedPkgs <- installed.packages(priority = "NA", lib.loc = lib)
  actions <- data.frame()
  
  for (pkgRecord in pkgRecords) {
    if (pkgRecord$name %in% rownames(installedPkgs)) {
      if (!versionMatchesDb(pkgRecord, installedPkgs)) { 
        actions <- rbind(actions, data.frame(row.names = pkgRecord$name,
                                             action = "replace", 
                                             stringsAsFactors = FALSE))
      }
    } else {
      actions <- rbind(actions, data.frame(row.names = pkgRecord$name, 
                                           action = "install", 
                                           stringsAsFactors = FALSE))
    }
  }
  
  # If any of the packages to be mutated are loaded, we have more work to do.
  if (any(rownames(actions) %in% loadedNamespaces())) {
    # TODO Write this to a file and replay it on startup. 
  }
  
  # Play the list
  if (nrow(actions) > 0)
    playInstallActions(pkgRecords, actions, repos, appDir, lib)
  else
    message("All packages are up to date.")
}

