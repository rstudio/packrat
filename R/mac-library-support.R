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

  ## We bash any old symlinks that were there already and regenerate
  ## them if necessary (this is an inexpensive process so we don't feel
  ## too badly)
  if (file.exists(libRdir)) {
    unlink(libRdir, recursive = TRUE)
  }
  dir.create(libRdir, recursive = TRUE, showWarnings = FALSE)

  ## Perform the symlinking -- we symlink individual packages because we don't
  ## want to capture any user libraries that may have been installed in the 'system'
  ## library directory
  ##
  ## NOTE: On Windows, we use junction points rather than symlinks to achieve the same
  ## effect
  results <- vapply(rownames(sysPkgsBase), function(pkg) {
    symlink(
      file.path(.Library, pkg),
      file.path(libRdir, pkg)
    )
  }, logical(1))

  if (!all(results)) {
    return(FALSE)
  }

  ## Clean up recursive symlinks if necessary -- it is possible that, e.g.
  ## within a base package directory:
  ##     /Library/Frameworks/R.framework/Versions/3.2/library/MASS
  ## there will be a link to MASS within MASS; we try to be friendly and
  ## remove those
  recursiveSymlinks <- file.path(.Library, sysPkgNames, sysPkgNames)
  invisible(lapply(recursiveSymlinks, function(file) {
    if (is.symlink(file)) {
      unlink(file)
    }
  }))

  return(TRUE)

}

symlinkExternalPackages <- function(project = NULL, lib.loc = .libPaths()) {
  project <- getProjectDir(project)

  external.packages <- opts$external.packages()
  if (!length(external.packages)) return(invisible(NULL))
  loc <- setNames(lapply(external.packages, function(x) {
    find.package(x, lib.loc = lib.loc, quiet = TRUE)
  }), external.packages)
  notFound <- loc[sapply(loc, function(x) {
    !length(x)
  })]
  if (length(notFound)) {
    warning("The following external packages could not be located:\n- ",
            paste(shQuote(names(notFound)), collapse = ", "))
  }
  loc <- loc[sapply(loc, function(x) length(x) > 0)]
  if (!file.exists(libExtDir(project)))
    dir.create(libExtDir(project), recursive = TRUE)
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
