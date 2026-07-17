test_that("new_defaults get returns values", {
  d <- new_defaults(list(a = 1, b = 2))

  # entire list when name is missing
  expect_identical(d$get(), list(a = 1, b = 2))

  # single value, dropped by default
  expect_identical(d$get("a"), 1)

  # named subset when drop = FALSE
  expect_identical(d$get("a", drop = FALSE), list(a = 1))
  expect_identical(d$get(c("a", "b")), list(a = 1, b = 2))

  # default = TRUE ignores anything set later
  d$set(a = 99)
  expect_identical(d$get("a"), 99)
  expect_identical(d$get("a", default = TRUE), 1)
})

test_that("new_defaults set merges named arguments", {
  d <- new_defaults(list(a = 1, b = 2))

  d$set(a = 10, c = 3)
  expect_identical(d$get(), list(a = 10, b = 2, c = 3))
})

test_that("new_defaults set merges a single list argument", {
  d <- new_defaults(list(a = 1, b = 2))

  d$set(list(a = 10, c = 3))
  expect_identical(d$get(), list(a = 10, b = 2, c = 3))
})

test_that("new_defaults set with no arguments leaves defaults untouched", {
  d <- new_defaults(list(a = 1))

  expect_null(d$set())
  expect_null(d$set(list()))
  expect_identical(d$get(), list(a = 1))
})

test_that("new_defaults restore replaces all defaults", {
  d <- new_defaults(list(a = 1, b = 2))
  d$set(a = 10)

  d$restore(list(z = 99))
  expect_identical(d$get(), list(z = 99))

  # restoring with no target reverts to the original value
  d$restore()
  expect_identical(d$get(), list(a = 1, b = 2))
})

test_that("new_defaults merge does not mutate stored defaults", {
  d <- new_defaults(list(a = 1, b = 2))

  expect_identical(d$merge(list(a = 10, c = 3)), list(a = 10, b = 2, c = 3))
  # merge is non-destructive
  expect_identical(d$get(), list(a = 1, b = 2))
})
