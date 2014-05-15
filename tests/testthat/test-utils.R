context("utils")

test_that("dir_copy copies directories", {
  dir.create("foo")
  file.create("foo/foo1.R")
  file.create("foo/foo2.R")
  file.create("foo/foo3.R")
  file.create("foo/.dotFile")
  dir_copy("foo", "bar", overwrite = TRUE)
  expect_identical(
    list.files("foo"),
    list.files("bar")
  )
  expect_error(dir_copy("foo", "bar"))

})
