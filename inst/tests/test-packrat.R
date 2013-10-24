library(testthat)

context("Packrat tests")

# For the context of these tests, we need to use a private repo and library. 
# Set the repo and library and restore the original settings when we're done.
repos <- getOption("repos")
pkgType <- getOption("pkgType")
on.exit({ 
  options("repos"= repos) 
  options("pkgType" = pkgType) 
}, add = TRUE)
setupTestRepo()

test_that("bootstrap creates project structure", {
  # Use the test repo for this test, and restore the existing repos when the
  # test is finished
  projRoot <- cloneTestProject("sated")
  bootstrap(projRoot, 
            sourcePackagePaths = 
              file.path(system.file("tests", package = "packrat"), 
                        "packages", "packrat"))
  expect_true(file.exists(file.path(projRoot, "packrat.lock")))
})
