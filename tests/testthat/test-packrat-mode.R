context("packrat mode")

test_that("packrat_mode successfully sets the library paths when turned on and off", {

  orig_libs <- .libPaths()
  ## Make sure packrat mode is off
  if (packrat:::packratModeOn())
    packrat_mode()
  packrat_mode(auto.snapshot = FALSE, bootstrap = FALSE)
  packrat_mode()
  expect_identical(orig_libs, .libPaths())

})
