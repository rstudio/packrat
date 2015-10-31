context("utils")

emit <- function(x) cat(x, sep = "\n")

test_that("dir_copy copies directories", {

  # Work in temporary directory
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(tempdir())

  # Create a directory and try to copy it
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

test_that("defer evaluates in appropriate environment", {

  foo <- function() {
    emit("+ foo")
    defer(emit("> foo"),              environment())
    defer(emit("> foo.parent"),        parent.frame(1))
    defer(emit("> foo.parent.parent"), parent.frame(2))
    emit("- foo")
  }

  bar <- function() {
    emit("+ bar")
    foo()
    emit("- bar")
  }

  baz <- function() {
    emit("+ baz")
    bar()
    emit("- baz")
  }

  output <- capture.output(baz())
  expected <- c(
    "+ baz",
    "+ bar",
    "+ foo",
    "- foo",
    "> foo",
    "- bar",
    "> foo.parent",
    "- baz",
    "> foo.parent.parent"
  )

  expect_identical(output, expected)

})

test_that("defer captures arguments properly", {

  foo <- function(x) {
    defer(emit(x), envir = parent.frame())
  }

  bar <- function(y) {
    emit("+ bar")
    foo(y)
    emit("- bar")
  }

  output <- capture.output(bar("> foo"))
  expected <- c("+ bar", "- bar", "> foo")
  expect_identical(output, expected)

})

test_that("defer works with arbitrary expressions", {

  foo <- function(x) {
    defer({
      x + 1
      emit("> foo")
    }, envir = parent.frame())
  }

  bar <- function() {
    emit("+ bar")
    foo(1)
    emit("- bar")
  }

  output <- capture.output(bar())
  expected <- c("+ bar", "- bar", "> foo")
  expect_identical(output, expected)

})
