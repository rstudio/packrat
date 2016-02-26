## System packages == installed packages with a non-NA priority
## Returns TRUE/FALSE, indicating whether the symlinking was successful
symlinkSystemPackages <- function(project = NULL) {
  project <- getProjectDir(project)

  # Get the path to the base R library installation
  sysLibPath <- R.home("library")

  ## Get the system packages
  sysPkgs <- utils::installed.packages(sysLibPath)
  sysPkgsBase <- sysPkgs[!is.na(sysPkgs[, "Priority"]), ]
  sysPkgNames <- rownames(sysPkgsBase)

  ## Make a directory where we can symlink these libraries
  libRdir <- libRdir(project = project)
  if (!file.exists(libRdir))
    dir.create(libRdir, recursive = TRUE)

  ## Generate symlinks for each package
  for (pkg in sysPkgNames) {
    source <- file.path(sysLibPath, pkg)
    target <- file.path(libRdir, pkg)
    if (!ensurePackageSymlink(source, target))
      return(FALSE)
  }

  TRUE
}

isPathToSamePackage <- function(source, target) {

  # When not on Windows, we can just check that the normalized
  # paths resolve to the same location.
  if (!is.windows())
    return(normalizePath(source) == normalizePath(target))

  # On Windows, junction points are not resolved by 'normalizePath()',
  # so we need an alternate strategy for determining if the junction
  # point is up to date. We ensure that the 'DESCRIPTION' files at
  # both locations are equivalent.
  lhsPath <- file.path(source, "DESCRIPTION")
  rhsPath <- file.path(target, "DESCRIPTION")

  lhsContents <- readChar(lhsPath, file.info(lhsPath)$size, TRUE)
  rhsContents <- readChar(rhsPath, file.info(rhsPath)$size, TRUE)

  identical(lhsContents, rhsContents)
}

ensurePackageSymlink <- function(source, target) {

  # If we have a symlink already active in the
  # target location, check that it points to the
  # library corresponding to the current running
  # R session.
  if (file.exists(target)) {

    if (isPathToSamePackage(source, target))
      return(TRUE)

    # Remove the old symlink. Both junction points and symlinks
    # are safely removed with a simple, non-recursive unlink.
    unlink(target)
  }

  # If, for some reason, the target directory
  # still exists, bail as otherwise symlinking
  # will not work as desired.
  if (file.exists(target))
    stop("Target '", target, "' already exists and is not a symlink")

  # Perform the symlink.
  symlink(source, target)

  # Success if the file now exists
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
