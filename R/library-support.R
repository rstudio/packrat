## System packages == installed packages with a non-NA priority
## Returns TRUE/FALSE, indicating whether the symlinking was successful
symlinkSystemPackages <- function(project = NULL) {
  project <- getProjectDir(project)

  ## Get the system packages
  sysPkgs <- utils::installed.packages(.Library)
  sysPkgsBase <- sysPkgs[!is.na(sysPkgs[, "Priority"]), ]
  sysPkgNames <- rownames(sysPkgsBase)

  ## Make a directory where we can symlink these libraries
  libRdir <- libRdir(project = project)

  ## Only bash the old symlinks if the version of R running is not
  ## equal to the R version of 'base' in lib-R.
  if (file.exists(libRdir)) {
    ## Get the DESCRIPTION for 'base'
    baseDescriptionPath <- file.path(libRdir, "base", "DESCRIPTION")
    if (file.exists(baseDescriptionPath)) {
      tryCatch({
        DESCRIPTION <- readDcf(baseDescriptionPath, all = TRUE)
        libRVersion <- package_version(DESCRIPTION$Version)
        rVersion <- getRversion()
        if (libRVersion != rVersion) {
          message("Updating system packages ('", libRVersion, "' -> '", rVersion, "')")
          unlink(libRdir, recursive = TRUE)
        }
      }, error = function(e) {
        warning("Unable to read DESCRIPTION file associated with 'base' package")
      })
    }
  }
  dir.create(libRdir, recursive = TRUE, showWarnings = FALSE)



  ## Perform the symlinking -- we symlink individual packages because we don't
  ## want to capture any user libraries that may have been installed in the 'system'
  ## library directory
  ##
  ## NOTE: On Windows, we use junction points rather than symlinks to achieve the same
  ## effect
  results <- suppressWarnings(vapply(rownames(sysPkgsBase), function(pkg) {

    from <- file.path(.Library, pkg)
    to <- file.path(libRdir, pkg)

    # Bash old symlinks just to ensure this session will be non-stale.
    # TODO: What if multiple R sessions want to run with a single packrat project?
    if (file.exists(to) && is.symlink(to))
      unlink(to)

    # TODO: For some reason, empty directories rather than junction points can
    # get generated on Windows.
    if (file.exists(to))
      unlink(to, recursive = TRUE)

    symlink(from, to)
  }, logical(1)))

  # symlink returns FALSE if there was a failure
  if (!all(results)) {
    # clean up the libRdir if it's empty
    if (!length(list.files(libRdir)))
      unlink(libRdir, recursive = TRUE)
    return(FALSE)
  }

  ## Clean up recursive symlinks if necessary -- it is possible that, e.g.
  ## within a base package directory:
  ##
  ##     /Library/Frameworks/R.framework/Versions/3.2/library/MASS
  ##
  ## there will be a link to MASS within MASS; we try to be friendly and
  ## remove those.
  recursiveSymlinks <- file.path(.Library, sysPkgNames, sysPkgNames)
  invisible(lapply(recursiveSymlinks, function(file) {
    if (is.symlink(file)) {
      unlink(file)
    }
  }))

  return(TRUE)

}

symlinkExternalPackages <- function(project = NULL) {
  project <- getProjectDir(project)

  # Bash any old symlinks that might exist
  unlink(libExtDir(project), recursive = TRUE)
  dir.create(libExtDir(project), recursive = TRUE)

  # Find the user libraries -- if packrat mode is off, this is presumedly
  # just the .libPaths(); if we're in packrat mode we have to ask packrat
  # for those libraries
  lib.loc <- NULL
  if (isPackratModeOn())
    lib.loc <- .packrat_mutables$get("origLibPaths")

  ## Although this shouldn't occur in practice, there can be intermediate states
  ## where e.g. packrat mode is 'on' but this state has been lost -- .libPaths()
  ## is usually where we want to look for external packages, anyhow
  if (!length(lib.loc))
    lib.loc <- .libPaths()

  # Get the external packages as well as their dependencies (these need
  # to be symlinked in so that imports and so on can be correctly resolved)
  external.packages <- opts$external.packages()
  if (!length(external.packages)) return(invisible(NULL))
  pkgDeps <- recursivePackageDependencies(
    external.packages,
    lib.loc = lib.loc,
    available.packages = NULL
  )
  allPkgs <- union(external.packages, pkgDeps)

  # Get the locations of these packages within the supplied lib.loc
  loc <- lapply(allPkgs, function(x) {
    find.package(x, lib.loc = lib.loc, quiet = TRUE)
  })
  names(loc) <- allPkgs

  # Warn about missing packages
  notFound <- loc[sapply(loc, function(x) {
    !length(x)
  })]

  if (length(notFound)) {
    warning("The following external packages could not be located:\n- ",
            paste(shQuote(names(notFound)), collapse = ", "))
  }

  # Symlink the packages that were found
  loc <- loc[sapply(loc, function(x) length(x) > 0)]
  results <- lapply(loc, function(x) {
    symlink(
      x,
      file.path(libExtDir(project), basename(x))
    )
  })

  failedSymlinks <- results[sapply(results, Negate(isTRUE))]
  if (length(failedSymlinks)) {
    warning("The following external packages could not be linked into ",
            "the packrat private library:\n- ",
            paste(shQuote(names(failedSymlinks)), collapse = ", "))
  }
}

is.symlink <- function(path) {

  ## Strip trailing '/'
  path <- gsub("/*$", "", path)

  ## Sys.readlink returns NA for error, "" for 'not a symlink', and <path> for symlink
  ## return false for first two cases, true for second
  result <- Sys.readlink(path)
  if (is.na(result)) FALSE
  else nzchar(result)

}

useSymlinkedSystemLibrary <- function(project = NULL) {
  project <- getProjectDir(project)
  replaceLibrary(".Library", libRdir(project = project))
}
