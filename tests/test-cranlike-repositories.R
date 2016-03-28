library(packrat)

(function() {

  # Disable R_TESTS within this scope (we don't want the R
  # subprocess to attempt to call startup.Rs)
  R_TESTS <- Sys.getenv("R_TESTS", unset = NA)
  if (!is.na(R_TESTS)) {
    Sys.unsetenv("R_TESTS")
    on.exit(Sys.setenv(R_TESTS = R_TESTS), add = TRUE)
  }

  dir <- tempdir()
  owd <- setwd(dir)
  on.exit(setwd(owd), add = TRUE)

  # Save repos
  repos <- getOption("repos")
  on.exit(options(repos = repos), add = TRUE)

  # Create the local repo
  localCRAN <- file.path(dir, "sushi")
  packrat::repos_create(localCRAN)
  on.exit(unlink(localCRAN, recursive = TRUE), add = TRUE)

  # Use only the 'sushi' repository
  options(repos = getOption("repos")["sushi"])

  # Create an example package.
  env <- new.env(parent = emptyenv())
  env$sashimi <- function() {}
  suppressMessages(
    utils::package.skeleton("sashimi", path = dir, environment = env)
  )
  on.exit(unlink(file.path(dir, "sashimi"), recursive = TRUE), add = TRUE)

  # tidy up the broken package
  unlink(file.path(dir, "sashimi/man"), recursive = TRUE)

  # Try uploading the package from the directory itself (requires building)
  message("\nBuilding sashimi:\n")
  packrat::repos_upload(file.path(dir, "sashimi"), "sushi")

  # Try building and uploading a tarball
  system(paste("R --vanilla CMD build", file.path(dir, "sashimi")))
  tarball <- list.files(dir, pattern = "\\.tar\\.gz$")[[1]]
  packrat::repos_upload(file.path(dir, tarball), "sushi")

  # Try installing the package as normal
  tempLib <- file.path(dir, "library")
  if (!file.exists(tempLib)) {
    dir.create(tempLib)
    on.exit(unlink(tempLib, recursive = TRUE), add = TRUE)
  }
  install.packages("sashimi", lib = tempLib, type = "source")

  # avoid bogus warning from R CMD check
  eval(call("library", "sashimi", lib.loc = tempLib))
  detach("package:sashimi", unload = TRUE)

})()
