context("packrat mode")

test_that("packrat_mode successfully sets the library paths when turned on and off", {

  cat("Current directory:\n")
  cat(getwd())
  cat("\n\n")

  cat("Original getLibPaths() on entry:\n")
  cat(getLibPaths(), sep = "\n")
  cat("\n\n")

  ## Make sure packrat mode is off
  if (packrat:::isPackratModeOn()) {
    packrat::off()
    cat("Turning packratMode off\n")
  }

  cat("getLibPaths() after checking packrat mode:\n")
  cat(getLibPaths(), sep = "\n")
  cat("\n\n")

  orig_libs <- getLibPaths()

  # don't use packrat::on so we can avoid the initialization step
  packrat:::setPackratModeOn(auto.snapshot = FALSE,
                             clean.search.path = FALSE)

  cat("getLibPaths() after entering packrat mode:\n")
  cat(getLibPaths(), sep = "\n")
  cat("\n\n")

  packrat::off()

  cat("getLibPaths() after exiting packrat mode:\n")
  cat(getLibPaths(), sep = "\n")
  cat("\n\n")

  expect_identical(orig_libs, getLibPaths())

})
