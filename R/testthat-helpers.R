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
rebuildTestRepo <- function(testroot = getwd()) {

  # Try to guess where the DESCRIPTION file lives (for R CMD check
  # and for interactive testing)
  candidates <- c(
    "DESCRIPTION",
    "../../DESCRIPTION",
    "../../00_pkg_src/packrat/DESCRIPTION",
    "../../packrat/DESCRIPTION"
  )

  for (candidate in candidates) {
    if (file.exists(candidate)) {
      DESCRIPTION <- normalizePath(candidate, winslash = "/")
      break
    }
  }

  owd <- getwd()
  on.exit(setwd(owd))

  # Move to the folder housing our dummy packages.
  source <- file.path(testroot, "packages")
  setwd(source)

  # Create a dummy folder for the current version of Packrat.
  dir.create("packrat", showWarnings = FALSE)
  file.copy(DESCRIPTION, "packrat/DESCRIPTION", overwrite = TRUE)

  # Force Packrat tests to believe the currently installed / tested
  # version of Packrat is on CRAN.
  cat("Repository: CRAN",
      file = "packrat/DESCRIPTION",
      sep = "\n",
      append = TRUE)

  # Copy in the dummy folders.
  target <- file.path(testroot, "repo", "src", "contrib")
  unlink(target, recursive = TRUE)
  dir.create(target, recursive = TRUE)
  pkgs <- list.files(source)
  for (pkg in pkgs) {
    descfile <- as.data.frame(read.dcf(file.path(source, pkg, "DESCRIPTION")))
    tarball <- paste(pkg, "_", as.character(descfile$Version), ".tar.gz",
                     sep = "")
    tar(tarball, pkg, compression = "gzip", tar = "internal")
    dir.create(file.path(target, pkg))
    file.rename(file.path(source, tarball), file.path(target, pkg, tarball))
  }

  tools::write_PACKAGES(target, subdirs = TRUE)
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
    if (length(setdiff(deps, head(sorted, i - 1))) > 0) {
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

# Sets up repositories etc. for a test context, and restores them when done.
beginTestContext <- function() {

  fields <- c("repos", "pkgType", "warn")
  options <- setNames(lapply(fields, getOption), fields)

  Sys.setenv(R_PACKRAT_TESTING = "yes")
  Sys.setenv(R_PACKRAT_LIBPATHS = paste(.libPaths(), collapse = .Platform$path.sep))

  CRAN <- paste(filePrefix(), normalizePath("repo", winslash = "/"), sep = "")

  options(repos = c(CRAN = CRAN), pkgType = "source", warn = 0)
  assign("test.options", options, envir = .packrat)
}

endTestContext <- function() {
  Sys.unsetenv("R_PACKRAT_TESTING")
  Sys.unsetenv("R_PACKRAT_LIBPATHS")
  options <- get("test.options", envir = .packrat)
  do.call(base::options, options)
}

withTestContext <- function(expr) {
  beginTestContext()
  force(expr)
  endTestContext()
}

scopeTestContext <- function() {
  beginTestContext()
  defer(endTestContext(), parent.frame())
}

bundle_test <- function(bundler, checker, ...) {

  # set and restore directory
  owd <- setwd(tempdir())
  on.exit(setwd(owd), add = TRUE)

  # create temporary directory
  dir <- file.path(tempdir(), "packrat-test-bundle")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  # enter, bundle and untar
  setwd("packrat-test-bundle")
  suppressWarnings(packrat::init(enter = FALSE))
  bundler(file = "test-bundle.tar.gz", ...)
  utils::untar("test-bundle.tar.gz", exdir = "untarred", tar = "internal")

  # run checker
  checker()

}
