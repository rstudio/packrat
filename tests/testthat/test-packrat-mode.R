context("packrat mode")

test_that("packrat_mode successfully sets the library paths when turned on and off", {

  orig_libs <- .libPaths()
  packrat_mode()
  packrat_mode()
  expect_identical(orig_libs, .libPaths())

})
