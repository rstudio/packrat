context("ignores")

test_that("updateRBuildIgnore adds the packrat directory to ignore", {
  path <- file.path(tempdir(), ".Rbuildignore")
  unlink(path)
  updateRBuildIgnore(project = tempdir())
  content <- readLines(path)
  expect_identical(content, c("^packrat/", "^\\.Rprofile$"))
  unlink(path)
})

test_that("updateRBuildIgnore preserves content in ignore file", {
  path <- file.path(tempdir(), ".Rbuildignore")
  unlink(path)
  cat(c("foo", "bar", "baz"), file = path, sep = "\n")
  updateRBuildIgnore(project = tempdir())
  content <- readLines(path)
  expect_identical(content, c("foo", "bar", "baz", "^packrat/", "^\\.Rprofile$"))
  unlink(path)
})

test_that("updateGitIgnore works", {

  options <- list(
    vcs.ignore.lib = TRUE,
    vcs.ignore.src = FALSE
  )
  dir <- file.path(tempdir(), "packrat-test")
  .gitignore <- file.path(dir, ".gitignore")
  dir.create(dir)
  updateGitIgnore(project = dir, options = options)
  content <- readLines(.gitignore)
  expect_identical(content, "packrat/lib*/")

  ## idempotency
  updateGitIgnore(project = dir, options = options)
  content <- readLines(.gitignore)
  expect_identical(content, "packrat/lib*/")

  ## preserve content of a .gitignore
  unlink(.gitignore)
  cat(c("foo", "bar", "baz"), file = .gitignore, sep = "\n")
  updateGitIgnore(project = dir, options = options)
  content <- readLines(.gitignore)
  expect_true(all(c("foo", "bar", "baz", "packrat/lib*/") %in% content))

  ## change options
  options$vcs.ignore.src <- TRUE
  updateGitIgnore(project = dir, options = options)
  content <- readLines(.gitignore)
  expect_true(all(c("foo", "bar", "baz", "packrat/lib*/", "packrat/src/") %in% content))

  ## remove all options
  options[] <- FALSE
  updateGitIgnore(project = dir, options = options)
  content <- readLines(.gitignore)
  expect_true(all(c("foo", "bar", "baz") %in% content))
  expect_false("packrat/lib*/" %in% content)
  expect_false("packrat/src/" %in% content)

  ## when all options FALSE and .gitignore does not already
  ## exist, .gitignore is not created.
  unlink(.gitignore)
  updateGitIgnore(project = dir, options = options)
  expect_false(file.exists(.gitignore))

  unlink(dir, recursive = TRUE)

})

test_that("updateIgnoreFile preserves old structure of file", {

  contents <- c(
    "# This is a comment.",
    "ignore/path",
    "",
    "# This is a comment.",
    "ignore/path"
  )

  file <- tempfile()
  on.exit(unlink(file))
  cat(contents, file = file, sep = "\n")

  updateIgnoreFile(project = tempdir(), file = basename(file), add = c("packrat/lib*/"))
  updated <- readLines(file)
  expect_identical(updated, c(contents, "packrat/lib*/"))

})
