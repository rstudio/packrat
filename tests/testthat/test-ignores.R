context("ignores")

test_that("updateRBuildIgnore adds the packrat directory to ignore", {
  path <- file.path(tempdir(), ".Rbuildignore")
  unlink(path)
  updateRBuildIgnore(project = tempdir())
  content <- readLines(path)
  expect_identical(content, "^packrat/")
  unlink(path)
})

test_that("updateRBuildIgnore preserves content in ignore file", {
  path <- file.path(tempdir(), ".Rbuildignore")
  unlink(path)
  cat(c("foo", "bar", "baz"), file = path, sep = "\n")
  updateRBuildIgnore(project = tempdir())
  content <- readLines(path)
  expect_identical(content, c("foo", "bar", "baz", "^packrat/"))
})
