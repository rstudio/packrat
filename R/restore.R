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
  
  # If we don't know where this package's source resides, give up
  if (identical(pkgRecord$source, "unknown") && !quiet) {
    stop("No sources available for package ", pkgRecord$name, ". Packrat can ", 
         "find sources for packages on CRAN-like repositories and packages ", 
         "installed using devtools::install_github. For other package types, ",
         "supply the path to the package's source using the argument ", 
         "sourcePackagePaths = c('~/path/to/package1', 'path/to/package2', ...)")
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
      fileLoc <- download.packages(pkgRecord$name, destdir = pkgSrcDir, 
                                   available = availablePkgs, repos = repos, 
                                   type = "source", quiet = TRUE)
      # If the file wasn't saved to the destination directory (which can happen
      # if the repo is local--see documentation in download.packages), copy it
      # there now
      if (!identical(fileLoc[1,2], file.path(pkgSrcDir, pkgSrcFile))) {
        file.copy(fileLoc[1,2], pkgSrcDir)
      }
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
                        pkgRecord$gh_repo, "/archive/", pkgRecord$gh_sha1,
                        ".tar.gz", sep = "")
    
    srczip <- tempfile(fileext='.tar.gz')
    on.exit({
      if (file.exists(srczip))
        unlink(srczip, recursive=TRUE)
    })
    
    download(archiveUrl, srczip, quiet = TRUE, mode = "wb")

    local({
      scratchDir <- tempfile()
      on.exit({
        if (file.exists(scratchDir))
          unlink(scratchDir, recursive=TRUE)
      })
      # untar can emit noisy warnings (e.g. "skipping pax global extended 
      # headers"); hide those
      suppressWarnings(untar(srczip, exdir=scratchDir, tar="internal"))
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
      dest <- normalizePath(file.path(pkgSrcDir, pkgSrcFile), winslash='/')

      # R's internal tar (which we use here for cross-platform consistency) 
      # emits warnings when there are > 100 characters in the path, due to the
      # resulting incompatibility with older implementations of tar. This isn't
      # relevant for our purposes, so suppress the warning. 
      in_dir(dirname(basedir),
             suppressWarnings(tar(tarfile=dest, files=basename(basedir),
                                  compression='gzip', tar='internal'))
      )
    })
    
    type <- "Github"
  }
  if (!quiet) {
    if (file.exists(file.path(pkgSrcDir, pkgSrcFile))) { 
      message("OK (", type, ")")
    } else {
      message("Failed")
      stop("Could not find sources for ", pkgRecord$name, " (", 
           pkgRecord$version, ").")
    }
  }
}

snapshotSources <- function(projDir, repos, pkgRecords) {
  # Get a list of source packages available on the repositories
  availablePkgs <- available.packages(contrib.url(repos, "source"), 
                                      type = "source")

  # Find the source directory (create it if necessary)
  sourceDir <- file.path(projDir, "packrat.sources")
  if (!file.exists(sourceDir))
    dir.create(sourceDir)
  
  # Get the sources for each package
  lapply(pkgRecords, function(pkgRecord) { 
    getSourceForPkgRecord(pkgRecord, sourceDir, availablePkgs, repos)
  })
}

annotatePkgDesc <- function(pkgRecord, projDir, lib = libdir(projDir)) {
  descFile <- file.path(lib, pkgRecord$name, 'DESCRIPTION')
  appendToDcf(descFile, data.frame(
    InstallAgent=paste('packrat', packageVersion('packrat')), 
    InstallSource=pkgRecord$source))
}

# Annotate a set of packages by name.
annotatePkgs <- function(pkgNames, projDir, lib = libdir(projDir)) {
  records <- searchPackages(lockInfo(projDir), pkgNames)
  lapply(records, function(record) {
    annotatePkgDesc(record, projDir, lib)
  })
}

# Takes a vector of package names, and returns a logical vector that indicates
# whether the package was not installed by packrat.
installedByPackrat <- function(pkgNames, lib.loc, default=NA) {
  # Can't use installed.packages(fields='InstallAgent') here because it uses
  # Meta/package.rds, not the DESCRIPTION file, and we only record this info in
  # the DESCRIPTION file.
  return(as.logical(sapply(pkgNames, function(pkg) {
    descFile <- file.path(lib.loc, pkg, 'DESCRIPTION')
    if (!file.exists(descFile))
      return(default)
    ia <- as.character(as.data.frame(read.dcf(descFile))$InstallAgent)
    if (length(ia) == 0)
      return(FALSE)
    return(grepl('^packrat\\b', ia)[1])
  })))
}

# Removes one or more packages from the app's private library and cached
# sources. 
removePkgs <- function(projDir, pkgNames, lib.loc = libdir(projDir)) {
  unlink(file.path(projDir, "packrat.sources", pkgNames), recursive = TRUE)
  remove.packages(pkgNames, lib.loc)
}

# Installs a single package from its record. Returns the method used to install
# the package (built source, downloaded binary, etc.)
installPkg <- function(pkgRecord, projDir, availablePkgs, repos, 
                       lib = libdir(projDir)) {
  pkgSrc <- NULL
  type <- "built source"
  needsInstall <- TRUE
  
  # Generally we want to install from sources, but we will download a pre-
  # built binary if (a) the package exists on CRAN, (b) the version on CRAN
  # is the version desired, and (c) R is set to download binaries.
  if (isFromCranlikeRepo(pkgRecord) && 
      pkgRecord$name %in% rownames(availablePkgs) &&
      versionMatchesDb(pkgRecord, availablePkgs) &&
      !identical(getOption("pkgType"), "source")) {
    tempdir <- tempdir()
    tryCatch ({
      # install.packages emits both messages and standard output; redirect these
      # streams to keep our own output clean. 
      suppressMessages(
        capture.output(
          install.packages(pkgRecord$name, lib = lib, repos = repos, 
                         available = availablePkgs, quiet = TRUE, 
                         dependencies = FALSE, verbose = FALSE)))
      type <- "downloaded binary"
      needsInstall <- FALSE
    }, error = function(e) {
      # Do nothing here, we'll try local sources if we fail to download from
      # the repo
    })  
  } 
  if (is.null(pkgSrc)) {
    # When installing from github or an older version, use the cached source
    # tarball or zip created in snapshotSources
    pkgSrc <- file.path(projDir, "packrat.sources", pkgRecord$name, 
                        pkgSrcFilename(pkgRecord))      
  } 
  if (!file.exists(pkgSrc)) {
    # If the source file is missing, try to download it. (Could happen in the
    # case where the packrat lockfile is present but cached sources are 
    # missing.)
    getSourceForPkgRecord(pkgRecord, file.path(projDir, "packrat.sources"),
                          availablePkgs, repos, quiet = TRUE)
    if (!file.exists(pkgSrc)) {
      stop("Failed to install ", pkgRecord$name, " (", pkgRecord$version, ")",
           ": sources missing at ", pkgSrc)
    }
  }
  
  if (needsInstall) {
    local({
      # devtools does not install to any libraries other than the default, so 
      # if the library we wish to install to is not the default, set as the
      # default while we do this operation. 
      if (!identical(.libPaths()[1], lib)) {
        oldLibPaths <- .libPaths() 
        on.exit(.libPaths(oldLibPaths), add = TRUE)
        .libPaths(lib)
      }
      install_local(path = pkgSrc, reload = FALSE, 
                    dependencies = FALSE, quick = TRUE, quiet = TRUE)
    })
  }
  
  # Annotate DESCRIPTION file so we know we installed it
  annotatePkgDesc(pkgRecord, projDir, lib)
  
  return(type)  
}

playActions <- function(pkgRecords, actions, repos, projDir, lib) {
  # Get the list of available packages and the latest version of those packages
  # from the repositories, and the local install list for comparison 
  availablePkgs <- available.packages(contrib.url(repos))

  # If this is the initial snapshot, we can optimize the installation of
  # packages from the global library to the private Packrat library
  initialSnapshot <- !identical(.libPaths()[1], lib)
  installedPkgs <- installed.packages(priority = c("NA", "recommended"))
  targetPkgs <- searchPackages(pkgRecords, names(actions))  
  
  for (i in seq_along(actions)) {
    action <- as.character(actions[i])
    pkgRecord <- targetPkgs[i][[1]]
    if (is.null(pkgRecord) && !identical(action, "remove")) {
      warning("Can't ", action, " ", names(actions[i]), 
              ": missing from lockfile")
      next
    }
    if (initialSnapshot) {
      # If taking the initial snapshot and installing a version to the 
      # private library that matches the version in the global library,
      # short-circuit and do a copy here. 
      if (identical(action, "add") &&
          pkgRecord$name %in% rownames(installedPkgs) &&
          versionMatchesDb(pkgRecord, installedPkgs)) {
        message("Installing ", pkgRecord$name, " (", pkgRecord$version,
                ") ... ", appendLF = FALSE)
        file.copy(find.package(pkgRecord$name), lib, recursive = TRUE)
        annotatePkgDesc(pkgRecord, projDir, lib)
        message("OK (copied local binary)")
        next
      }
    }
    if (identical(action, "upgrade") ||
        identical(action, "downgrade") ||
        identical(action, "crossgrade")) {
      # Changing package type or version: Remove the old one now (we'll write 
      # a new one in a moment)
      message("Replacing ", pkgRecord$name, " (", action, " ", 
              installedPkgs[pkgRecord$name,"Version"], " to ", 
              pkgRecord$version, ") ... ", appendLF = FALSE)
      removePkgs(projDir, pkgRecord$name, lib)
    } else if (identical(action, "add")) {
      message("Installing ", pkgRecord$name, " (", pkgRecord$version, ") ... ", 
              appendLF = FALSE)
    } else if (identical(action, "remove")) {
      if (is.null(pkgRecord)) {
        message("Removing ", names(actions[i]), " ... ", appendLF = FALSE)
        removePkgs(projDir, names(actions[i]), lib)
      } else {
        message("Removing ", pkgRecord$name, "( ", pkgRecord$version, ") ... ",
          appendLF = FALSE)
        removePkgs(projDir, pkgRecord$name, lib)
      }
      message("OK")
      next
    }
    type <- installPkg(pkgRecord, projDir, availablePkgs, repos, lib) 
    message("OK (", type, ")")
  }
  invisible()
}

restoreImpl <- function(projDir, repos, pkgRecords, lib,
                        pkgsToIgnore=character(0), prompt=interactive()) {
  installedPkgs <- 
    getPackageRecords(
      rownames(installed.packages(lib.loc = lib)), 
      recursive = FALSE, lib.loc = lib)
  actions <- diff(installedPkgs, pkgRecords)
  actions[names(actions) %in% pkgsToIgnore] <- NA
  restartNeeded <- FALSE
  
  mustConfirm <- any(c('downgrade', 'remove', 'crossgrade') %in% actions)
  
  if (all(is.na(actions))) {
    cat("Already up to date")
    return(invisible())
  }

  # Since we print actions as we do them, there's no need to do a summary
  # print first unless we need the user to confirm. 
  if (prompt && mustConfirm) {
    summarizeDiffs(actions, installedPkgs, pkgRecords, 
                   'Adding these packages to your library:', 
                   'Removing these packages from your library:', 
                   'Upgrading these packages in your library:',
                   'Downgrading these packages in your library:',
                   'Modifying these packages in your library:')
    
    answer <- readline('Do you want to continue? [Y/n] ')
    answer <- gsub('^\\s*(.*?)\\s*$', '\\1', answer)
    if (nzchar(answer) && tolower(answer) != 'y') {
      return(invisible())
    }
  }
  
  # The actions are sorted alphabetically; resort them in the order given by
  # pkgRecords (previously sorted topologically). Remove actions are special, 
  # since they don't exist in the lockfile-generated list; extract them and 
  # combine afterwards. 
  removeActions <- actions[actions == "remove"] 
  actions <- c(removeActions, 
               unlist(lapply(pkgRecords, function(p) { actions[p$name] })))

  # If any of the packages to be mutated are loaded, and the library we're
  # installing to is the default library, make a copy of the library and perform
  # the changes on the copy. 
  actions <- actions[!is.na(actions)]
  targetLib <- if (any(names(actions) %in% loadedNamespaces()) &&
                   identical(lib, .libPaths()[1])) {
    newlib <- file.path(projDir, 'library.new')
    dir.create(newlib)
    file.copy(file.path(projDir, 'library'), newlib, recursive = TRUE)
    restartNeeded <- TRUE
    libdir(newlib)
  } else {
    lib
  }
  
  # Play the list, if there's anything to play
  playActions(pkgRecords, actions, repos, projDir, targetLib)
  if (restartNeeded) {
    cat("You must restart R to finish applying these changes.")
  }
}

