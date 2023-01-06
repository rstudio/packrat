test_that("Shiny examples have a shiny dependency", {
  skip_on_cran()
  skip_if_not_installed("shiny")

  # Confirm packrat believes all example shiny apps are, in fact, shiny apps
  examplesPath <- system.file("examples", package = "shiny")
  apps <- list.files(examplesPath, full.names = TRUE)
  for (app in apps) {
    expect_true("shiny" %in% packrat:::appDependencies(app), app)
  }
})

test_that("projects which use shiny implicitly are detected", {
  skip_on_cran()

  # Check that 'shiny' is listed as a dependency for an
  # R Markdown document with 'runtime: shiny'
  interactiveDocPath <- file.path("resources", "interactive-doc-example.Rmd")
  expect_true("shiny" %in% packrat:::fileDependencies(interactiveDocPath))
})
