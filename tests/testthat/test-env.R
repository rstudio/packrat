context("Environment Variables")

name <- "R_PACKRAT_TEST_ENV"

test_that("variables can be set, retrieved", {
  setenv(name, letters)
  expect_identical(getenv(name), letters)
})

test_that("variables with quotes handled", {
  text <- c("Hello, 'World'", "How 'are' you?")
  setenv(name, text)
  expect_identical(getenv(name), text)
})
