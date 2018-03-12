context("Initialization")

test_that("we can re-initialize the test repository", {
  skip_on_cran()
  rebuildTestRepo()
})
