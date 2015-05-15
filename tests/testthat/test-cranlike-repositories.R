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
  packrat::create_repo(localCRAN)

  # Create an example package
  pkgKitten::kitten("sashimi", path = dir)
  descPath <- file.path(dir, "sashimi", "DESCRIPTION")
  cat("Repository: sushi", file = descPath, append = TRUE, sep = "\n")

  # Try uploading the package
  packrat::upload_package(
    file.path(dir, "sashimi"),
    "sushi"
  )

  # Try installing the package as normal
  tempLib <- file.path(dir, "library")
  dir.create(tempLib)
  install.packages("sashimi", lib = tempLib)
  library("sashimi", lib.loc = tempLib)

  sashimi::hello("World!")

})
