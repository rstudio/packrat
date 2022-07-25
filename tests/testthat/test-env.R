library(mockery)

test_that("TAR environment variable is respected", {
  TAR <- Sys.getenv("TAR")
  if (is.na(TAR)) {
    on.exit(Sys.unsetenv("TAR"))
  } else {
    on.exit(Sys.setenv("TAR" = TAR))
  }

  Sys.setenv(TAR = "foo/bar/tar")

  expect_equal(tar_binary(), "foo/bar/tar")
})

test_that("With no TAR variable set, on Unix, we look for tar with Sys.which", {
  TAR <- Sys.getenv("TAR")
  if (is.na(TAR)) {
    on.exit(Sys.unsetenv("TAR"))
  } else {
    on.exit(Sys.setenv("TAR" = TAR))
  }

  Sys.unsetenv("TAR")

  stub(is.unix, "tar_binary", TRUE)
  stub(base::Sys.which, "tar_binary", "foo/bar/tar")

  expect_equal(tar_binary(), "foo/bar/tar")
})
