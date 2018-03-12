context("Shiny")

test_that("projects which use shiny implicitly are detected", {
  skip_on_cran()

  # Try checking to see if packrat believes all example shiny apps
  # are, in fact, shiny apps
  options(repos = c(CRAN = "http://cran.rstudio.org"))
  if ("shiny" %in% rownames(installed.packages())) {
    examplesPath <- system.file("examples", package = "shiny")
    apps <- list.files(examplesPath, full.names = TRUE)
    for (app in apps) {
      expect_true("shiny" %in% packrat:::appDependencies(app))
    }
  }

  # Check that 'shiny' is listed as a dependency for an
  # R Markdown document with 'runtime: shiny'
  interactiveDocPath <- file.path("resources", "interactive-doc-example.Rmd")
  expect_true("shiny" %in% packrat:::fileDependencies(interactiveDocPath))

})
