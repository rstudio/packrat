## System packages == installed packages with a non-NA priority
## Returns TRUE/FALSE, indicating whether the symlinking was successful
symlinkSystemPackages <- function(project = NULL) {
  project <- getProjectDir(project)

  sysLibPath <- R.home("library")

  ## Get the system packages
  sysPkgs <- utils::installed.packages(sysLibPath)
  sysPkgsBase <- sysPkgs[!is.na(sysPkgs[, "Priority"]), ]
  sysPkgNames <- rownames(sysPkgsBase)

  ## Make a directory where we can symlink these libraries
  libRdir <- libRdir(project = project)
  if (!file.exists(libRdir))
    dir.create(libRdir, recursive = TRUE)

  for (pkg in sysPkgNames) {
    source <- file.path(sysLibPath, pkg)
    target <- file.path(libRdir, pkg)
    if (!ensurePackageSymlink(source, target))
      stop(sprintf("Failed to symlink package '%s' to '%s'", source, target))
  }

  TRUE
}

ensurePackageSymlink <- function(source, target) {

  # If we have a symlink already active in the
  # target location, check that it points to the
  # library corresponding to the current running
  # R session.
  if (file.exists(target)) {

    # If the resolved path is the same as the source path, accept it.
    resolved <- normalizePath(target, winslash = "/", mustWork = TRUE)
    if (resolved == normalizePath(source, winslash = "/", mustWork = FALSE))
      return(TRUE)

    # Remove the old symlink.
    if (is.symlink(target))
      unlink(target)
    else
      unlink(target, recursive = TRUE)
  }

  # Perform the symlink.
  symlink(source, target)

  file.exists(target)
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
