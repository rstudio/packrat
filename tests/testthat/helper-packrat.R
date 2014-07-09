# Clone a test project into a temporary directory for manipulation; returns the
# path to the test project
cloneTestProject <- function(projectName) {
  root <- file.path("projects", projectName)
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
  repo <- paste("file:///", normalizePath("repo", winslash = "/"),
                sep = "")
  names(repo) <- "CRAN"
  options("repos" = repo)
  options("pkgType" = "source")
}

# Installs a test package from source. Necessary because install.packages
# fails under R CMD CHECK.
installTestPkg <- function(pkg, ver, lib) {
  pkgSrc <- file.path("repo", "src", "contrib", pkg,
                      paste(pkg, "_", ver, ".tar.gz", sep = ""))
  install_local_path(path = pkgSrc, reload = FALSE,
                     args = paste("-l", lib), dependencies = FALSE,
                     quick = TRUE, quiet = TRUE)
}

# Adds a dependency on a package to a test project
addTestDependency <- function(projRoot, pkg) {
  write(paste("library(", pkg, ")", sep = ""),
        file = file.path(projRoot, "deps.R"),
        append = TRUE)
}

# Removes a dependency from a test project (by deleting a file... fancy!)
removeTestDependencyFile <- function(projRoot, file) {
  unlink(file.path(projRoot, file))
}

verifyTopoSort <- function(graph, sorted) {
  if (length(graph) != length(sorted))
    return(FALSE)
  if (length(sorted) < 2)
    return(TRUE)
  if (!identical(unique(sorted), sorted))
    return(FALSE)
  if (any(is.na(sorted)) || any(is.na(names(graph))))
    return(FALSE)
  for (i in seq_along(sorted)) {
    deps <- graph[[sorted[[i]]]]
    if (length(setdiff(deps, head(sorted, i-1))) > 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

# Make the 'libraries' 'project' -- this is used to test whether files within the
# packrat controlled libraries are ignored
makeLibrariesProject <- function() {
  if (basename(getwd()) != "testthat") {
    warning("This function is only used to build a sample 'libraries' project in the testthat dir")
    return(NULL)
  }
  project <- file.path("projects", "libraries")
  unlink(project, recursive = TRUE)
  dir.create(project)
  cat("library(bread)", file = file.path(project, "library.R"), sep = "\n")

  ## Create the potential packrat libraries that might exist
  dir.create(newLibraryDir(project), recursive = TRUE)
  dir.create(oldLibraryDir(project), recursive = TRUE)
  dir.create(libraryRootDir(project), recursive = TRUE)

  # Some files within depending on oatmeal
  cat("library(oatmeal)", file = file.path(libraryRootDir(project), "lib-current.R"), sep = "\n")
  cat("library(oatmeal)", file = file.path(oldLibraryDir(project), "lib-old.R"), sep = "\n")
  cat("library(oatmeal)", file = file.path(newLibraryDir(project), "lib-new.R"), sep = "\n")
  project
}
