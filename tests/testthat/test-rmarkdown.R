context("R Markdown")

test_that("Rmd documents with parameters are analyzed", {

  # we need to skip this test if we don't have an up-to-date version of knitr available
  if (packageVersion("knitr") < "1.11")
    skip("requires knitr 1.11 or greater")

  parameterDocPath <- file.path("resources", "params-example.Rmd")
  deps <- packrat:::fileDependencies(parameterDocPath)
  expect_true("rmarkdown" %in% deps, "all Rmd docs have an rmarkdown dependency")
  expect_true("shiny" %in% deps, "Rmd docs with parameters have a shiny dependency for the customization app")
  expect_true("stringr" %in% deps, "dependencies in parameter expressions are extracted")
})
