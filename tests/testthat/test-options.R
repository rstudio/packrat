# Confirms that we can use set_opts and the single-option setter and that the
# single-option setter does not overwrite previously configured state (#655)
test_that("an option can be set and retrieved", {
  initial <- get_opts()

  expected <- initial
  expected[["ignored.packages"]] <- c("emo")
  expected[["snapshot.recommended.packages"]] <- TRUE

  packrat::set_opts(ignored.packages = c("emo"), persist = FALSE)
  packrat::opts$snapshot.recommended.packages(TRUE, persist = FALSE)

  actual <- get_opts()
  expect_equal(actual, expected)
})
