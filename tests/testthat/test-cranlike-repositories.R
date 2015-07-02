context("Custom CRAN-like Repositories")

test_that("We can create and 'upload' an example package", {

  testthat:::skip_on_cran()

  # Bail if we don't have 'pkgKitten' (needed to generate
  # a buildable package skeleton)
  if (!requireNamespace("pkgKitten", quietly = TRUE))
    return()

  dir <- tempdir()

  # Create the local repo
  localCRAN <- file.path(dir, "sushi")
  packrat::repos_create(localCRAN)

  # Create an example package
  pkgKitten::kitten("sashimi", path = dir)

  # Try uploading the package
  packrat::repos_upload(
    file.path(dir, "sashimi"),
    "sushi"
  )

  # Try uploading a tarball
  packrat::repos_upload(
    file.path(dir, "sashimi_1.0.tar.gz"),
    "sushi"
  )

  # Try installing the package as normal
  tempLib <- file.path(dir, "library")
  dir.create(tempLib)
  install.packages("sashimi", lib = tempLib)
  library("sashimi", lib.loc = tempLib)

  sashimi::hello("World!")
  detach("package:sashimi", unload = TRUE)

})
