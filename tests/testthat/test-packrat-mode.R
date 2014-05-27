context("packrat mode")

test_that("packratMode successfully sets the library paths when turned on and off", {

  cat("Current directory:\n")
  cat(getwd())
  cat("\n\n")

  cat("Original .libPaths() on entry:\n")
  cat(.libPaths(), sep = "\n")
  cat("\n\n")

  ## Make sure packrat mode is off
  if (packrat:::isPackratModeOn()) {
    packratModeOff()
    cat("Turning packratMode off\n")
  }

  cat(".libPaths() after checking packratMode:\n")
  cat(.libPaths(), sep = "\n")
  cat("\n\n")

  orig_libs <- .libPaths()

  packratModeOn(autoSnapshot = FALSE, bootstrap = FALSE)

  cat(".libPaths() after entering packratMode:\n")
  cat(.libPaths(), sep = "\n")
  cat("\n\n")

  packratModeOff()

  cat(".libPaths() after exiting packratMode:\n")
  cat(.libPaths(), sep = "\n")
  cat("\n\n")

  expect_identical(orig_libs, .libPaths())

})
