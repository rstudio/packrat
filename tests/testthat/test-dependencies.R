
# Dependency analysis needs rmarkdown+knitr and rmarkdown needs pandoc.
test_that("we have pandoc", {
  skip_on_cran()

  expect_true(rmarkdown::pandoc_available())
})

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

test_that("package dependencies are not discovered in a Quarto document without R chunks", {
  skip_on_cran()
  ordinaryQmd <- file.path("resources", "simple.qmd")
  expect_equal(length(packrat:::fileDependencies(ordinaryQmd)), 0)
})

test_that("package dependencies are discovered in a Quarto document with R chunks", {
  skip_on_cran()
  ordinaryQmd <- file.path("resources", "dependencies.qmd")
  expect_equal(packrat:::fileDependencies(ordinaryQmd), c("bread"))
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

test_that("dependencies are discovered in R Markdown documents in independent R chunks", {
  skip_on_cran()

  someBrokenChunksRmd <- file.path("resources", "broken-chunks.Rmd")
  brokenDeps <- packrat:::fileDependencies(someBrokenChunksRmd)

  # shiny_prerendered file
  expect_true("shiny" %in% brokenDeps)

  # check for working chunks
  expect_true(all(
    c("pkgA", "pkgB", "pkgD", "pkgF", "pkgG") %in% brokenDeps
  ))
})

test_that("dependencies are discovered in the presence of variables", {
  skip_on_cran()

  loadingPackages <- file.path("resources", "loading-packages.R")
  deps <- packrat:::fileDependencies(loadingPackages)
  expect_true(all(deps %in% c("bread", "oatmeal")))
})

test_that("dependencies in function default values are discovered", {
  skip_on_cran()
  emojiR <- file.path("resources", "emoji.R")
  expect_equal(packrat:::fileDependencies(emojiR), "emo")
})

test_that("knitr doesn't warn about unknown engines in dependency discovery", {
  skip_on_cran()

  file <- "resources/unknown-engines.Rmd"

  caughtWarning <- NULL
  deps <- withCallingHandlers(
    packrat:::fileDependencies(file),
    warning = function(w) caughtWarning <<- w
  )

  expect_null(caughtWarning)
  expect_equal(deps, "rmarkdown")

})

withTestContext({
  test_that("project dependencies are detected", {
    skip_on_cran()

    packrat:::set_opts(local.repos = "packages", persist = FALSE)
    on.exit(packrat:::set_opts(local.repos = NULL, persist = FALSE), add = TRUE)

    projRoot <- cloneTestProject("sated")
    deps <- packrat:::appDependencies(projRoot)
    expect_equal(deps, c("bread", "breakfast", "oatmeal", "packrat", "toast"))
  })

  test_that("project dependencies can ignore top-level dependencies", {
    skip_on_cran()

    packrat:::set_opts(ignored.packages = c("bread"), local.repos = "packages", persist = FALSE)
    on.exit(packrat:::set_opts(ignored.packages = NULL, local.repos = NULL, persist = FALSE), add = TRUE)

    projRoot <- cloneTestProject("smallbreakfast")
    deps <- packrat:::appDependencies(projRoot)
    expect_equal(deps, c("oatmeal", "packrat")) # bread is ignored.
  })

  test_that("project dependencies can ignore lower-level dependencies", {
    skip_on_cran()

    packrat:::set_opts(ignored.packages = c("toast"), local.repos = "packages", persist = FALSE)
    on.exit(packrat:::set_opts(ignored.packages = NULL, local.repos = NULL, persist = FALSE), add = TRUE)

    projRoot <- cloneTestProject("sated")
    deps <- packrat:::appDependencies(projRoot)
    expect_equal(deps, c("breakfast", "oatmeal", "packrat")) # toast and bread are ignored
  })
})
