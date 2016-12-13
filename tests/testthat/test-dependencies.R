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


test_that("dependencies are discovered in R Markdown documents using alternate engines", {
  altEngineRmd <- file.path("resources", "alternate-engines.Rmd")
  expect_true("testthat" %in% packrat:::fileDependencies(altEngineRmd))
})

test_that("dependencies are discovered in R Markdown documents with R chunks", {
  ordinaryRmd <- file.path("resources", "params-example.Rmd")
  expect_true("rmarkdown" %in% packrat:::fileDependencies(ordinaryRmd))
})

test_that("dependencies are discovered in R Markdown documents with no chunks", {
  chunklessRmd <- file.path("resources", "no-chunks.Rmd")
  expect_true("rmarkdown" %in% packrat:::fileDependencies(chunklessRmd))
})

