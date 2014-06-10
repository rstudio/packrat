symlinkSystemLibraries <- function(project = NULL) {
  project <- getProjectDir(project)

  ## Get the system packages
  sysPkgs <- installed.packages(.Library)
  sysPkgsBase <- sysPkgs[!is.na(sysPkgs[, "Priority"]), ]
  sysPkgNames <- rownames(sysPkgsBase)

  ## Make a directory where we can symlink these libraries
  libRdir <- file.path(
    .packrat$packratFolderName,
    relativeLibDir("lib-R")
  )
  dir.create(libRdir, recursive = TRUE, showWarnings = FALSE)

  ## Perform the symlinking
  for (pkg in rownames(sysPkgsBase)) {
    if (!file.exists(file.path(.Library, pkg))) {
      file.symlink(
        file.path(.Library, pkg),
        file.path(libRdir, pkg)
      )
    }
  }
}

useSymlinkedLibrary <- function(project = NULL) {
  project <- getProjectDir(project)

  replaceLibrary(".Library", normalizePath(file.path(
    .packrat$packratFolderName,
    relativeLibDir("lib-R")
  )))
}
