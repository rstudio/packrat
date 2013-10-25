# Clone a test project into a temporary directory for manipulation; returns the
# path to the test project
cloneTestProject <- function(projectName) {
  root <- file.path(system.file("tests", package = "packrat"), 
                    "testthat", "projects", projectName)
  target <- tempdir()
  if (file.exists(file.path(target, projectName))) {
    unlink(file.path(target, projectName), recursive = TRUE)
  }
  file.copy(root, target, recursive = TRUE)
  return(file.path(target, projectName))
}

# "Rebuilds" the test repo from its package "sources" (just DESCRIPTION files).
# Not run from tests.
rebuildTestRepo <- function(testroot) {
  wd <- getwd()
  on.exit(setwd(wd))
  src <- file.path(testroot, "packages")
  setwd(src)
  target <- file.path(testroot, "repo", "src", "contrib")
  unlink(target, recursive = TRUE)
  dir.create(target, recursive = TRUE)
  pkgs <- list.files(src)
  for (pkg in pkgs) {
    descfile <- as.data.frame(read.dcf(file.path(src, pkg, "DESCRIPTION")))
    tarball <- paste(pkg, "_", as.character(descfile$Version), ".tar.gz", 
                     sep = "")
    tar(tarball, pkg, compression = "gzip", tar = "internal")
    dir.create(file.path(target, pkg))
    file.rename(file.path(src, tarball), file.path(target, pkg, tarball))
  }
  tools::write_PACKAGES(target, subdirs = TRUE)
}

# Sets up the fake repo used for testing
setupTestRepo <- function() {
  repo <- paste("file://", 
                file.path(system.file("tests", package = "packrat"), 
                          "testthat", "repo"), 
                sep = "")
  names(repo) <- "CRAN"
  options("repos" = repo) 
  options("pkgType" = "source")
}
