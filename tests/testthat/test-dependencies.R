context("Dependencies")

test_that("dependencies are properly resolved in expressions", {
  skip_on_cran()

  expr <- quote({
    library(tools)
    library("utils")
    requireNamespace(quietly = TRUE, package = "knitr")

    # Don't trip up on 'library(x, character.only = TRUE)'
    for (x in 1:10)
      library(x, character.only = TRUE)

    "stats"::rnorm(1)
    setRefClass("foo", "bar")
  })

  dependencies <- expressionDependencies(expr)
  expect_true(setequal(
    c("tools", "utils", "stats", "knitr", "methods"),
    dependencies
  ))

})


test_that("dependencies are discovered in R Markdown documents using alternate engines", {
  skip_on_cran()
  altEngineRmd <- file.path("resources", "alternate-engines.Rmd")
  expect_true("testthat" %in% packrat:::fileDependencies(altEngineRmd))
})

test_that("dependencies are discovered in R Markdown documents with R chunks", {
  skip_on_cran()
  ordinaryRmd <- file.path("resources", "params-example.Rmd")
  expect_true("rmarkdown" %in% packrat:::fileDependencies(ordinaryRmd))
})

test_that("dependencies are discovered in R Markdown documents with no chunks", {
  skip_on_cran()
  chunklessRmd <- file.path("resources", "no-chunks.Rmd")
  expect_true("rmarkdown" %in% packrat:::fileDependencies(chunklessRmd))
})

test_that("dependencies are discovered in inline R code", {
  skip_on_cran()

  # ensure that we've restored 'inline_exec' properly at the end
  inline_exec <- yoink("knitr", "inline_exec")
  on.exit(
    expect_identical(inline_exec, yoink("knitr", "inline_exec")),
    add = TRUE
  )

  # run the regular test
  emojiRmd <- file.path("resources", "emoji.Rmd")
  expect_true("emo" %in% packrat:::fileDependencies(emojiRmd))
})
