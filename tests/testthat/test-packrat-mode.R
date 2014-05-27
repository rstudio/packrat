context("packrat mode")

test_that("packrat_mode successfully sets the library paths when turned on and off", {

  cat("Current directory:\n")
  cat(getwd())
  cat("\n\n")

  cat("Original .libPaths() on entry:\n")
  cat(.libPaths(), sep = "\n")
  cat("\n\n")

  ## Make sure packrat mode is off
  if (packrat:::isPackratModeOn()) {
    packrat_off()
    cat("Turning packrat_mode off\n")
  }

  cat(".libPaths() after checking packrat_mode:\n")
  cat(.libPaths(), sep = "\n")
  cat("\n\n")

  orig_libs <- .libPaths()

  packrat_on(auto.snapshot = FALSE, bootstrap = FALSE)

  cat(".libPaths() after entering packrat_mode:\n")
  cat(.libPaths(), sep = "\n")
  cat("\n\n")

  packrat_off()

  cat(".libPaths() after exiting packrat_mode:\n")
  cat(.libPaths(), sep = "\n")
  cat("\n\n")

  expect_identical(orig_libs, .libPaths())

})
