context("Custom CRAN-like Repositories")

test_that("We can create and 'upload' an example package", {

  if (!requireNamespace("devtools", quietly = TRUE))
    return()

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
  devtools::create(file.path(dir, "sashimi"))

  # Try uploading the package
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
