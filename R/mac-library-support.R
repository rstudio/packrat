## System packages == installed packages with a non-NA priority
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
  for (pkg in rownames(sysPkgsBase)) {
    file.symlink(
      file.path(.Library, pkg),
      file.path(libRdir, pkg)
    )
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
