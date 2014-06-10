## System packages == installed packages with a non-NA priority
symlinkSystemPackages <- function(project = NULL) {
  project <- getProjectDir(project)

  ## Get the system packages
  sysPkgs <- installed.packages(.Library)
  sysPkgsBase <- sysPkgs[!is.na(sysPkgs[, "Priority"]), ]
  sysPkgNames <- rownames(sysPkgsBase)

  ## Make a directory where we can symlink these libraries
  libRdir <- libRdir(project = project)
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
}

useSymlinkedSystemLibrary <- function(project = NULL) {
  project <- getProjectDir(project)
  replaceLibrary(".Library", normalizePath(libRdir(project = project)))
}
