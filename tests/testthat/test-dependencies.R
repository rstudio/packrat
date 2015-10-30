context("Dependencies")

test_that("dependencies are properly resolved in expressions", {

  expr <- quote({
    library(dplyr)
    library("plyr")
    requireNamespace(quietly = TRUE, package = "knitr")

    # Don't trip up on 'library(x, character.only = TRUE)'
    for (x in 1:10)
      library(x, character.only = TRUE)

    "stats"::rnorm(1)
    setRefClass("foo", "bar")
  })

  dependencies <- expressionDependencies(expr)
  expect_true(setequal(
    c("plyr", "dplyr", "stats", "knitr", "methods"),
    dependencies
  ))

})
