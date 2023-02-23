# Given a package record, indicate the name we expect its source archive to have.
pkgSrcFilename <- function(pkgRecord) {
  if (identical(pkgRecord$source, "github"))
    paste(pkgRecord$gh_sha1, ".tar.gz", sep = "")
  else if (pkgRecord$source %in% c("bitbucket", "gitlab"))
    paste(pkgRecord$remote_sha, ".tar.gz", sep = "")
  else
    paste(pkgRecord$name, "_", pkgRecord$version, ".tar.gz", sep = "")
}

# Given a package record and a set of known repositories, indicate whether the
# package exists on a CRAN-like repository.
isFromCranlikeRepo <- function(pkgRecord, repos) {

  # for package records inferred from a DESCRIPTION file, we know
  # whether a package came from a CRAN-like repository
  if (inherits(pkgRecord, "CustomCRANLikeRepository"))
    return(TRUE)

  # TODO: this shouldn't happen, but if it does we'll assume the package
  # can be obtained from CRAN
  source <- pkgRecord$source
  if (!length(source))
    return(TRUE)

  # for records that do declare a source, ensure it's not 'source', 'github', 'bitbucket', or 'gitlab'.
  # in previous releases of packrat, we attempted to match the repository name
  # with one of the existing repositories; however, this caused issues in
  # certain environments (the names declared repositories in the lockfile, and
  # the the names of the active repositories in the R session, may not match)
  !tolower(source) %in% c("source", "github", "bitbucket", "gitlab")
}

# Given a package record and a database of packages, check to see if
# the package version is current. NB: Assumes package is present in db.
versionMatchesDb <- function(pkgRecord, db) {
  versionMatch <-
    identical(pkgRecord$version, db[pkgRecord$name, "Version"])

  # For GitHub, Bitbucket, and Gitlab, we also need to check that the SHA1 is identical
  # (the source may be updated even if the version hasn't been bumped)
  if (versionMatch && identical(pkgRecord$source, "github")) {
    pkgDescFile <- system.file('DESCRIPTION', package = pkgRecord$name)
    installedPkgRecord <-
      inferPackageRecord(as.data.frame(readDcf(pkgDescFile)))
    versionMatch <- identical(pkgRecord$gh_sha1,
                              installedPkgRecord$gh_sha1)
  } else if (versionMatch && pkgRecord$source %in% c("gitlab", "bitbucket")) {
    pkgDescFile <- system.file('DESCRIPTION', package = pkgRecord$name)
    installedPkgRecord <-
      inferPackageRecord(as.data.frame(readDcf(pkgDescFile)))
    versionMatch <- identical(pkgRecord$remote_sha,
                              installedPkgRecord$remote_sha)
  }
  versionMatch
}

# Given a package record, fetch the sources for the package and place them in
# the source directory root given by sourceDir.
# - Responsible for calling different download methods for different source
#   locations (e.g. git hosting service, CRAN).
# - Creates the path for the temporary destination file, named `srczip` at this
#   level. It doesn't create the file itself — download functions do that — but
#   handles its cleanup if it exists when the function exits.
getSourceForPkgRecord <- function(pkgRecord,
                                  sourceDir,
                                  availablePkgs,
                                  repos,
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

  # If we don't know where this package's source resides, give up
  if (identical(pkgRecord$source, "unknown") && !quiet) {
    stop("No sources available for package ", pkgRecord$name, ". Packrat can ",
         "find sources for packages on CRAN-like repositories and packages ",
         "installed using devtools::install_github, devtools::install_gitlab",
         "devtools::install_bitbucket. TODO: local repo")
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
  if (identical(type, "CustomCRANLikeRepository"))
    type <- "CRAN"

  # If this is a local source path, compress the local sources rather than
  # trying to download from an external source
  if (identical(pkgRecord$source, "source")) {

    local({

      if (file.exists(file.path(pkgRecord$source_path, "DESCRIPTION"))) {
        ## If the path supplied is the directory of a source package,
        ## build it

        build(
          pkg = pkgRecord$source_path,
          path = file.path(pkgSrcDir),
          binary = FALSE,
          vignettes = FALSE,
          quiet = TRUE
        )

      } else if (endswith(pkgRecord$source_path, ".tar.gz")) {

        ## We assume it's a package already generated by R CMD build -- just
        ## copy the tarball over
        file.copy(pkgRecord$source_path, file.path(pkgSrcDir, basename(pkgRecord$source_path)))

      } else {

        # R's tar command preserves paths relative to the current directory in
        # the archive, so temporarily set the working directory there while
        # we create the tarball

        wd <- getwd()
        on.exit(setwd(wd), add = TRUE)
        setwd(file.path(pkgRecord$source_path, ".."))

        tar(file.path(pkgSrcDir, pkgSrcFile), files = pkgRecord$name,
            compression = "gzip", tar = tar_binary())
      }
    })
    type <- "local"
  } else if (isFromCranlikeRepo(pkgRecord, repos)) {

    # Attempt to detect if this is the current version of a package
    # on a CRAN-like repository
    currentVersion <- if (pkgRecord$name %in% availablePkgs[, "Package"])
      availablePkgs[pkgRecord$name, "Version"]
    else
      NA

    # Is the reported package version from 'available.packages()'
    # newer than that reported from CRAN? If so, we may be attempting
    # to install a package version not compatible with this version of
    # R.
    if (!is.na(currentVersion) && is.character(pkgRecord$version)) {
      compared <- utils::compareVersion(currentVersion, pkgRecord$version)
      if (compared == -1) {
        fmt <- paste(
          "Package version '%s' is newer than the latest version reported",
          "by CRAN ('%s') -- packrat may be unable to retrieve package sources."
        )
        msg <- sprintf(fmt, pkgRecord$version, currentVersion)
        warning(msg)
      }
    }

    # Is the source for this version of the package on CRAN and/or a
    # Bioconductor repo?
    if (identical(pkgRecord$version, currentVersion)) {
      # Get the source package
      # NOTE: we cannot use 'availablePkgs' as it might have been used to
      # generate an available package listing for _binary_ packages,
      # rather than source packages. Leave it NULL and let R do the
      # right thing
      fileLoc <- downloadPackagesWithRetries(pkgRecord$name,
                                             destdir = pkgSrcDir,
                                             repos = repos,
                                             type = "source")
      if (!nrow(fileLoc)) {
        stop("Failed to download current version of ", pkgRecord$name,
             "(", pkgRecord$version, ")")
        }

      # If the file wasn't saved to the destination directory (which can happen
      # if the repo is local--see documentation in download.packages), copy it
      # there now
      if (!identical(fileLoc[1, 2], file.path(pkgSrcDir, pkgSrcFile))) {
        file.copy(fileLoc[1, 2], pkgSrcDir)
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
          downloadWithRetries(archiveUrl,
                              destfile = file.path(pkgSrcDir, pkgSrcFile),
                              mode = "wb", quiet = TRUE)
          foundVersion <- TRUE
          type <- paste(type, "archived")
          break
        }, error = function(e) {
          # Ignore error and try the next repository
        })
      }
      if (!foundVersion) {
        message("FAILED")
        stopMsg <- sprintf("Failed to retrieve package sources for %s %s from CRAN (internet connectivity issue?)",
                           pkgRecord$name,
                           pkgRecord$version)

        if (!is.na(currentVersion))
          stopMsg <- paste(stopMsg, sprintf("[%s is current]", currentVersion))

        stop(stopMsg)
      }
    }
  } else if (identical(pkgRecord$source, "github")) {
    archiveUrl <- githubArchiveUrl(pkgRecord)

    srczip <- tempfile(fileext = '.tar.gz')
    on.exit(
      unlink(srczip, recursive = TRUE),
      add = TRUE
    )

    tryCatch({
      githubDownload(archiveUrl, srczip)
    }, error = function(e) {
      message("FAILED")
      e$message <- sprintf("Failed to download package from GitHub URL: '%s'\n%s", archiveUrl, e$message)
      stop(e)
    })

    remote_info <- getRemoteInfo(pkgRecord)

    dest <- normalizePath(file.path(pkgSrcDir, pkgSrcFile), winslash = "/", mustWork = FALSE)

    tryCatch({
      success <- appendRemoteInfoToDescription(
        src = srczip,
        dest = dest,
        remote_info = remote_info
      )
    }, error = function(e) {
      e$message <- sprintf("Could not update 'DESCRIPTION' file for package %s:\n%s",
           pkgRecord$name, e)
      stop(e)
    })

    type <- "GitHub"

  } else if (identical(pkgRecord$source, "bitbucket")) {
    archiveUrl <- bitbucketArchiveUrl(pkgRecord)

    srczip <- tempfile(fileext = '.tar.gz')
    on.exit(
      unlink(srczip, recursive = TRUE),
      add = TRUE
    )

    tryCatch({
      bitbucketDownload(archiveUrl, srczip)
    }, error = function(e) {
      message("FAILED")
      e$message <- sprintf("Failed to download package from Bitbucket URL: '%s'\n%s", archiveUrl, e$message)
      stop(e)
    })

    remote_info <- getRemoteInfo(pkgRecord)

    dest <- normalizePath(file.path(pkgSrcDir, pkgSrcFile), winslash = "/", mustWork = FALSE)

    tryCatch({
      success <- appendRemoteInfoToDescription(
        src = srczip,
        dest = dest,
        remote_info = remote_info
      )
    }, error = function(e) {
      e$message <- sprintf("Could not update 'DESCRIPTION' file for package %s:\n%s",
           pkgRecord$name, e)
      stop(e)
    })

    type <- "Bitbucket"


  } else if (identical(pkgRecord$source, "gitlab")) {
    archiveUrl <- gitlabArchiveUrl(pkgRecord)

    srczip <- tempfile(fileext = '.tar.gz')
    on.exit(
      unlink(srczip, recursive = TRUE),
      add = TRUE
    )

    tryCatch({
      gitlabDownload(archiveUrl, srczip)
    }, error = function(e) {
      message("FAILED")
      e$message <- sprintf("Failed to download package from GitLab URL: '%s'\n%s", archiveUrl, e$message)
      stop(e)
    })

    remote_info <- getRemoteInfo(pkgRecord)

    dest <- normalizePath(file.path(pkgSrcDir, pkgSrcFile), winslash = "/", mustWork = FALSE)

    tryCatch({
      success <- appendRemoteInfoToDescription(
        src = srczip,
        dest = dest,
        remote_info = remote_info
      )
    }, error = function(e) {
      e$message <- sprintf("Could not update 'DESCRIPTION' file for package %s:\n%s",
           pkgRecord$name, e)
      stop(e)
    })

    type <- "GitLab"
  }
  if (!quiet) { # TODO: Does turning on (quiet) prevent it from failing on error here
    if (file.exists(file.path(pkgSrcDir, pkgSrcFile))) {
      message("OK (", type, ")")
    } else {
      message("FAILED")
      stop("Could not find sources for ", pkgRecord$name, " (",
           pkgRecord$version, ").")
    }
  }
}

snapshotSources <- function(project, repos, pkgRecords) {

  # Don't snapshot packages included in external.packages
  external.packages <- opts$external.packages()
  pkgRecords <- Filter(function(x) !(x$name %in% external.packages),
                       pkgRecords)

  # Get a list of source packages available on the repositories
  availablePkgs <- availablePackagesSource(repos = repos)

  # Find the source directory (create it if necessary)
  sourceDir <- srcDir(project)
  if (!file.exists(sourceDir))
    dir.create(sourceDir, recursive = TRUE)

  # Get the sources for each package
  results <- lapply(pkgRecords, function(pkgRecord) {
    try(getSourceForPkgRecord(pkgRecord, sourceDir, availablePkgs, repos),
        silent = TRUE)
  })

  errors <- results[sapply(results, function(x) inherits(x, "try-error"))]
  if (length(errors) > 0)
    stop("Errors occurred when fetching source files:\n", errors)

  invisible(NULL)
}

annotatePkgDesc <- function(pkgRecord, project, lib = libDir(project)) {
  descFile <- file.path(lib, pkgRecord$name, 'DESCRIPTION')

  # Get the records to write
  records <- list(
    InstallAgent = paste('packrat', packageVersion('packrat')),
    InstallSource = pkgRecord$source,
    InstallSourcePath = pkgRecord$source_path,
    Hash = hash(descFile)
  )

  # Read in the DCF file
  content <- as.data.frame(readDcf(descFile))
  stopifnot(nrow(content) == 1)

  # Replace the records
  for (i in seq_along(records)) {
    name <- names(records)[i]
    content[name] <- records[name]
  }

  # Write it out
  write_dcf(content, descFile)
}

# Annotate a set of packages by name.
annotatePkgs <- function(pkgNames, project, lib = libDir(project)) {
  records <- searchPackages(lockInfo(project), pkgNames)
  lapply(records, function(record) {
    annotatePkgDesc(record, project, lib)
  })
}

# Takes a vector of package names, and returns a logical vector that indicates
# whether the package was not installed by packrat.
installedByPackrat <- function(pkgNames, lib.loc, default = NA) {
  # Can't use installed.packages(fields='InstallAgent') here because it uses
  # Meta/package.rds, not the DESCRIPTION file, and we only record this info in
  # the DESCRIPTION file.
  return(as.logical(sapply(pkgNames, function(pkg) {
    descFile <- file.path(lib.loc, pkg, 'DESCRIPTION')
    if (!file.exists(descFile))
      return(default)
    ia <- as.character(as.data.frame(readDcf(descFile))$InstallAgent)
    if (length(ia) == 0)
      return(FALSE)
    return(grepl('^packrat\\b', ia)[1])
  })))
}

# Removes one or more packages from the app's private library.
removePkgs <- function(project, pkgNames, lib.loc = libDir(project)) {
  remove.packages(pkgNames, lib.loc)
  pkgNames
}

# Installs a single package from its record. Returns the method used to install
# the package (built source, downloaded binary, etc.)
installPkg <- function(pkgRecord,
                       project,
                       repos,
                       lib = libDir(project))
{
  pkgSrc <- NULL
  type <- "built source"
  needsInstall <- TRUE

  # If we're trying to install a package that overwrites a symlink, e.g. for a
  # cached package, we need to move that symlink out of the way (otherwise
  # `install.packages()` or `R CMD INSTALL` will fail with surprising errors,
  # like:
  #
  #     Error: 'zoo' is not a valid package name
  #
  # To avoid this, we explicitly move the symlink out of the way, and later
  # restore it if, for some reason, package installation failed.
  pkgInstallPath <- file.path(lib, pkgRecord$name)

  # NOTE: a symlink that points to a path that doesn't exist
  # will return FALSE when queried by `file.exists()`!
  if (file.exists(pkgInstallPath) || is.symlink(pkgInstallPath)) {

    temp <- tempfile(tmpdir = lib)
    file.rename(pkgInstallPath, temp)
    on.exit({
      if (file.exists(pkgInstallPath))
        unlink(temp, recursive = !is.symlink(temp))
      else
        file.rename(temp, pkgInstallPath)
    }, add = TRUE)

  }

  # Try restoring the package from the global cache.
  cacheCopyStatus <- new.env(parent = emptyenv())
  copiedFromCache <- restoreWithCopyFromCache(project, pkgRecord, cacheCopyStatus)
  if (copiedFromCache) {
    type <- cacheCopyStatus$type
    needsInstall <- FALSE
  }

  # Try restoring the package from the 'unsafe' cache, if applicable.
  copiedFromUntrustedCache <- restoreWithCopyFromUntrustedCache(project, pkgRecord, cacheCopyStatus)
  if (copiedFromUntrustedCache) {
    type <- cacheCopyStatus$type
    needsInstall <- FALSE
  }

  # if we still need to attempt an installation at this point,
  # remove a prior installation / file from library (if necessary).
  # we move the old directory out of the way temporarily, and then
  # delete if if all went well, or restore it if installation failed
  # for some reason
  if (needsInstall && file.exists(pkgInstallPath)) {
    pkgRenamePath <- tempfile(tmpdir = lib)
    file.rename(pkgInstallPath, pkgRenamePath)
    on.exit({
      if (file.exists(pkgInstallPath))
        unlink(pkgRenamePath, recursive = !is.symlink(pkgRenamePath))
      else
        file.rename(pkgRenamePath, pkgInstallPath)
    }, add = TRUE)
  }

  # Try downloading a binary (when appropriate).
  if (!(copiedFromCache || copiedFromUntrustedCache) &&
      hasBinaryRepositories() &&
      binaryRepositoriesEnabled() &&
      isFromCranlikeRepo(pkgRecord, repos) &&
      pkgRecord$name %in% availablePackagesBinary(repos = repos)[, "Package"] &&
      versionMatchesDb(pkgRecord, availablePackagesBinary(repos = repos)))
  {
    tempdir <- tempdir()
    tryCatch({
      # install.packages emits both messages and standard output; redirect these
      # streams to keep our own output clean.

      # on windows, we need to detach the package before installation
      detachPackageForInstallationIfNecessary(pkgRecord$name)

      suppressMessages(
        capture.output(
          utils::install.packages(pkgRecord$name,
                                  lib = lib,
                                  repos = repos,
                                  type = .Platform$pkgType,
                                  available = availablePackagesBinary(repos = repos),
                                  quiet = TRUE,
                                  dependencies = FALSE,
                                  verbose = FALSE)
        )
      )

      type <- "downloaded binary"
      needsInstall <- FALSE
    }, error = function(e) {
      # Do nothing here, we'll try local sources if we fail to download from
      # the repo
    })
  }

  if (is.null(pkgSrc)) {
    # When installing from github/bitbucket/gitlab or an older version, use the cached source
    # tarball or zip created in snapshotSources
    pkgSrc <- file.path(srcDir(project), pkgRecord$name,
                        pkgSrcFilename(pkgRecord))
  }

  if (needsInstall) {

    if (!file.exists(pkgSrc)) {
      # If the source file is missing, try to download it. (Could happen in the
      # case where the packrat lockfile is present but cached sources are
      # missing.)
      getSourceForPkgRecord(pkgRecord,
                            srcDir(project),
                            availablePackagesSource(repos = repos),
                            repos,
                            quiet = TRUE)
      if (!file.exists(pkgSrc)) {
        stop("Failed to install ", pkgRecord$name, " (", pkgRecord$version, ")",
             ": sources missing at ", pkgSrc)
      }
    }

    # Infer package type; note that RSPM may deliver binary packages
    # in archives with .tar.gz extension.
    pkgType <- tryCatch(
      archivePackageType(pkgSrc, quiet = TRUE),
      error = identity
    )

    if (identical(pkgType, "binary"))
      type <- "downloaded binary"

    local({
      # devtools does not install to any libraries other than the default, so
      # if the library we wish to install to is not the default, set as the
      # default while we do this operation.
      if (!isPathToSameFile(getLibPaths()[1], lib)) {
        oldLibPaths <- getLibPaths()
        on.exit(setLibPaths(oldLibPaths), add = TRUE)
        # Make sure the library actually exists, otherwise setLibPaths will silently
        # fail
        if (!file.exists(lib)) dir.create(lib, recursive = TRUE)
        setLibPaths(lib)
      }

      # on windows, we need to detach the package before installation
      detachPackageForInstallationIfNecessary(pkgRecord$name)

      quiet <- isTRUE(packrat::opts$quiet.package.installation())
      install_local_path(path = pkgSrc, reload = FALSE,
                         dependencies = FALSE, quick = TRUE, quiet = quiet)
    })
  }

  # Annotate DESCRIPTION file so we know we installed it
  annotatePkgDesc(pkgRecord, project, lib)

  # copy package into cache if enabled
  if (isUsingCache(project)) {
    pkgPath <- file.path(lib, pkgRecord$name)

    # copy into global cache if this is a trusted package
    if (isTrustedPackage(pkgRecord$name)) {
      descPath <- file.path(pkgPath, "DESCRIPTION")
      if (!file.exists(descPath)) {
        warning("cannot cache package: no DESCRIPTION file at path '", descPath, "'")
      } else {
        hash <- hash(descPath)
        moveInstalledPackageToCache(
          packagePath = pkgPath,
          hash = hash,
          cacheDir = cacheLibDir()
        )
      }
    } else {
      pkgPath <- file.path(lib, pkgRecord$name)
      tarballName <- pkgSrcFilename(pkgRecord)
      tarballPath <- file.path(srcDir(project), pkgRecord$name, tarballName)
      if (!file.exists(tarballPath)) {
        warning("cannot cache untrusted package: source tarball not available")
      } else {
        hash <- hashTarball(tarballPath)
        moveInstalledPackageToCache(
          packagePath = pkgPath,
          hash = hash,
          cacheDir = untrustedCacheLibDir()
        )
      }
    }
  }

  return(type)
}

playActions <- function(pkgRecords, actions, repos, project, lib) {

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
    if (action %in% c("upgrade", "downgrade", "crossgrade")) {
      # Changing package type or version: Remove the old one now (we'll write
      # a new one in a moment)
      message("Replacing ", pkgRecord$name, " (", action, " ",
              installedPkgs[pkgRecord$name, "Version"], " to ",
              pkgRecord$version, ") ... ", appendLF = FALSE)
      removePkgs(project, pkgRecord$name, lib)
    } else if (identical(action, "add")) {
      # Insert newline to show progress on consoles that buffer to newlines.
      message("Installing ", pkgRecord$name, " (", pkgRecord$version, ") ... ",
              appendLF = TRUE)
    } else if (identical(action, "remove")) {
      if (is.null(pkgRecord)) {
        message("Removing ", names(actions[i]), " ... ", appendLF = FALSE)
        removePkgs(project, names(actions[i]), lib)
      } else {
        message("Removing ", pkgRecord$name, "( ", pkgRecord$version, ") ... ",
                appendLF = FALSE)
        removePkgs(project, pkgRecord$name, lib)
      }
      message("OK")
      next
    }
    type <- installPkg(pkgRecord, project, repos, lib)
    message("\tOK (", type, ")")
  }
  invisible()
}

restoreImpl <- function(project,
                        repos,
                        pkgRecords,
                        lib,
                        pkgsToIgnore = character(),
                        prompt = interactive(),
                        dry.run = FALSE,
                        restart = TRUE)
{
  # optionally overlay the 'src' directory from a custom location
  overlaySourcePackages(srcDir(project))

  discoverUntrustedPackages(srcDir(project))

  # We also ignore restores for packages specified in external.packages
  pkgsToIgnore <- c(
    pkgsToIgnore,
    packrat::opts$external.packages(),
    packrat::opts$ignored.packages()
  )

  installedPkgs <- rownames(installed.packages(lib.loc = lib))
  installedPkgs <- setdiff(installedPkgs, c("manipulate", "rstudio"))
  installedPkgRecords <- getPackageRecords(
      installedPkgs,
      project = project,
      recursive = FALSE,
      lib.loc = lib
    )
  actions <- diff(installedPkgRecords, pkgRecords)
  actions[names(actions) %in% pkgsToIgnore] <- NA
  restartNeeded <- FALSE

  mustConfirm <- any(c('downgrade', 'remove', 'crossgrade') %in% actions)

  if (all(is.na(actions))) {
    message("Already up to date.")
    return(invisible())
  }

  # Since we print actions as we do them, there's no need to do a summary
  # print first unless we need the user to confirm.
  if (prompt && mustConfirm && !dry.run) {
    summarizeDiffs(actions, installedPkgRecords, pkgRecords,
                   'Adding these packages to your library:',
                   'Removing these packages from your library:',
                   'Upgrading these packages in your library:',
                   'Downgrading these packages in your library:',
                   'Modifying these packages in your library:')

    answer <- readline('Do you want to continue? [Y/n]: ')
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

  # Assign targetLib based on whether the namespace of a package within the
  # packrat directory is loaded -- packages that don't exist can just
  # return "" -- this will fail the equality checks later
  loadedNamespaces <- loadedNamespaces()
  packageLoadPaths <- sapply(names(actions), function(x) {
    if (x %in% loadedNamespaces)
      getNamespaceInfo(x, "path")
    else
      ""
  })

  loadedFromPrivateLibrary <- names(actions)[
    packageLoadPaths == libDir(project)
  ]

  if (length(loadedFromPrivateLibrary)) {
    newLibrary <- newLibraryDir(project)
    dir_copy(libraryRootDir(project), newLibrary)
    restartNeeded <- TRUE
    targetLib <- file.path(newLibrary, R.version$platform, getRversion())
  } else {
    targetLib <- lib
  }

  # Play the list, if there's anything to play
  if (!dry.run) {
    playActions(pkgRecords, actions, repos, project, targetLib)
    if (restartNeeded) {
      if (!restart || !attemptRestart())
        message("You must restart R to finish applying these changes.")
    }
  } else {
    list(pkgRecords = pkgRecords,
         actions = actions,
         repos = repos,
         project = project,
         targetLib = targetLib)
  }
}

detachPackageForInstallationIfNecessary <- function(pkg) {

  # no need to detach if not actually attached
  searchPathName <- paste("package", pkg, sep = ":")
  if (!searchPathName %in% search())
    return(FALSE)

  # get the library the package was actually loaded from
  location <- which(search() == searchPathName)
  pkgPath <- attr(as.environment(location), "path")
  if (!is.character(pkgPath))
    return(FALSE)

  # got the package path; detach and reload on exit of parent.
  # when running tests, we want to reload packrat from the same
  # directory it was run from rather than the private library, as
  # we install a dummy version of packrat that doesn't actually export
  # the functions we need
  libPaths <- if (pkg == "packrat" && isTestingPackrat()) {
    strsplit(Sys.getenv("R_PACKRAT_LIBPATHS"),
             .Platform$path.sep,
             fixed = TRUE)[[1]]
  } else {
    dirname(pkgPath)
  }

  detach(searchPathName, character.only = TRUE)

  # re-load the package when the calling function returns
  defer(library(pkg, lib.loc = libPaths, character.only = TRUE), parent.frame())

  TRUE
}

discoverUntrustedPackages <- function(srcDir) {
  if (is.na(Sys.getenv("POSIT_CONNECT", unset = NA)))
    return()

  # set the 'packrat.untrusted.packages' option if
  # it has not yet been specified
  if (is.null(getOption("packrat.untrusted.packages")))
    options("packrat.untrusted.packages" = list.files(srcDir))
}

overlaySourcePackages <- function(srcDir, overlayDir = NULL) {
  if (is.null(overlayDir))
    overlayDir <- Sys.getenv("R_PACKRAT_SRC_OVERLAY")

  if (!is.character(overlayDir) || !is.directory(overlayDir))
    return()

  overlayDir <- normalizePath(overlayDir, winslash = "/", mustWork = TRUE)
  sources <- list.files(
    overlayDir,
    recursive = TRUE,
    full.names = FALSE,
    no.. = TRUE,
    include.dirs = FALSE,
    pattern = "\\.tar\\.gz$"
  )

  lapply(sources, function(source) {
    target <- file.path(srcDir, source)
    source <- file.path(overlayDir, source)

    # skip if this tarball already exists in the target directory
    if (file.exists(target))
      return(NULL)

    # attempt to symlink source to target
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    if (!is.directory(dirname(target)))
      stop("failed to create directory '", dirname(target), "'")

    # generate symlink
    symlink(source, target)

    # report success
    file.exists(target)
  })
}

archivePackageType <- function(path, quiet = FALSE, default = "source") {

  info <- file.info(path, extra_cols = FALSE)
  if (is.na(info$isdir))
    stopf("no package at path %s", shQuote(path, type = "cmd"))

  # for directories, check for Meta
  if (info$isdir) {
    hasmeta <- file.exists(file.path(path, "Meta"))
    type <- if (hasmeta) "binary" else "source"
    return(type)
  }

  # otherwise, guess based on contents of package
  methods <- list(
    tar = function(path) untar(tarfile = path, list = TRUE),
    zip = function(path) unzip(zipfile = path, list = TRUE)$Name
  )

  # try zip first for files ending with '.zip'
  # (but attempt to be robust against mis-named files)
  if (endswith(path, ".zip"))
    methods <- methods[c("zip", "tar")]

  for (method in methods) {

    files <- tryCatch(method(path), error = identity)
    if (inherits(files, "error"))
      next

    hasmeta <- any(grepl("^[^/]+/Meta/?$", files))
    type <- if (hasmeta) "binary" else "source"
    return(type)

  }

  if (!quiet) {
    fmt <- "failed to determine type of package '%s'; assuming source"
    warningf(fmt, shQuote(path, type = "cmd"))
  }

  default

}


# Decompresses the archive passed to `src`. Appends `remote_info` to the
# DESCRIPTION file. Recompresses the file passed to `dest`, which must be
# a `.tar.gz`. Returns TRUE if successful.
appendRemoteInfoToDescription <- function(src, dest, remote_info) {
  # We expect `dest` to end with `".tar.gz"`.
  if (!grepl(".tar.gz$", dest)) {
    stop("Destination path for source archive must end in '.tar.gz'.")
  }

  # Extract the package to a temporary dir so that we can modify the
  # `DESCRIPTION` with the remote info.
  scratchDir <- tempfile()
  on.exit({
    if (file.exists(scratchDir))
      unlink(scratchDir, recursive = TRUE)
  })
  # untar can emit noisy warnings (e.g. "skipping pax global extended
  # headers"); hide those
  suppressWarnings(untar(src, exdir = scratchDir, tar = tar_binary()))

  # Determine the untarred base directory. We're looking to see if the untarred
  # directory contains only a single directory and if so, we treat that as our
  # base directory.
  if (length(dir(scratchDir)) == 1 &&
      is.directory(file.path(scratchDir, dir(scratchDir)))) {
    basedir <- file.path(scratchDir, dir(scratchDir))
  } else {
    basedir <- scratchDir
  }

  # Determine the true package root
  if (remote_info$RemoteType == "github") {
    remote_subdir <- remote_info$GithubSubdir
  } else {
    remote_subdir <- remote_info$RemoteSubdir
  }
  if (length(remote_subdir) > 0) {
    basedir <- file.path(basedir, remote_subdir)
  }

  if (!file.exists(file.path(basedir, "DESCRIPTION"))) {
    # This error may indicate a malformed package, or an unexpected directory
    # structure inside the tarball.
    stop("Could not locate DESCRIPTION file in package archive.")
  }

  # Do what we came here to do.
  appendToDcf(file.path(basedir, "DESCRIPTION"), remote_info)

  # Now we can recompress the file to wherever we've been told to do so.
  # R's internal tar (which we use here for cross-platform consistency)
  # emits warnings when there are > 100 characters in the path, due to the
  # resulting incompatibility with older implementations of tar. This isn't
  # relevant for our purposes, so suppress the warning.
  # tryCatch here so we can unlink the file if tar fails.
  tryCatch(
    in_dir(dirname(basedir),
            suppressWarnings(tar(tarfile = dest, files = basename(basedir),
                                compression = "gzip", tar = tar_binary()))
    ),
    error = function(e) {
      unlink(dest)
      stop(e)
    }
  )

  return(TRUE)
}
