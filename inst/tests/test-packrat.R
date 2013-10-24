library(testthat)

test_that("bootstrap creates project structure", {
  # Use the test repo for this test, and restore the existing repos when the
  # test is finished
  repos <- getOption("repos")
  pkgType <- getOption("pkgType")
  on.exit({ options("repos"= repos); options("pkgType" = pkgType) }, add = TRUE)
  setupTestRepo()

  projRoot <- cloneTestProject("sated")
  bootstrap(projRoot)
  expect_true(file.exists(file.path(projRoot, "packrat.lock")))
})
