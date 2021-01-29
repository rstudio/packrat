test_that("we can re-initialize the test repositories", {
  skip_on_cran()
  rebuildTestRepo()
  rebuildEmptyTestRepo()
})
