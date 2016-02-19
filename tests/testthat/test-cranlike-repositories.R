context("CRAN")

test_that("We can create and 'upload' an example package", {
  skip_on_cran() && skip_on_travis()

  dir <- tempdir()
  owd <- getwd()
  setwd(dir)
  on.exit(setwd(owd), add = TRUE)

  # Create the local repo
  localCRAN <- file.path(dir, "sushi")
  packrat::repos_create(localCRAN)

  repos <- getOption("repos")
  options(repos = repos["sushi"])
  on.exit(options(repos = repos), add = TRUE)

  # Create an example package.
  env <- new.env(parent = emptyenv())
  env$sashimi <- function() {}
  suppressMessages(
    utils::package.skeleton("sashimi", path = dir, environment = env)
  )

  # tidy up the broken package
  unlink("sashimi/man", recursive = TRUE)

  # Try uploading the package from the directory itself (requires building)
  message("\nBuilding sashimi:\n")
  packrat::repos_upload(
    file.path(dir, "sashimi"),
    "sushi"
  )

  # Try building and uploading a tarball
  system(paste("R --vanilla CMD build", file.path(dir, "sashimi")))
  tarball <- list.files(dir, pattern = "\\.tar\\.gz$")[[1]]
  packrat::repos_upload(
    file.path(dir, tarball),
    "sushi"
  )

  # Try installing the package as normal
  tempLib <- file.path(dir, "library")
  dir.create(tempLib)
  install.packages("sashimi", lib = tempLib)
  library("sashimi", lib.loc = tempLib)
  detach("package:sashimi", unload = TRUE)

})
