library(mockery)

test_that("TAR environment variable is respected", {
  TAR <- Sys.getenv("TAR")
  if (is.na(TAR)) {
    on.exit(Sys.unsetenv("TAR"))
  } else {
    on.exit(Sys.setenv("TAR" = TAR))
  }

  Sys.setenv(TAR = "/foo/bar/tar")

  expect_equal(tar_binary(), "/foo/bar/tar")
})

test_that("On Unix, use tar on the path if it exists, otherwise internal", {
  skip_on_os("windows")
  TAR <- Sys.getenv("TAR")
  if (is.na(TAR)) {
    on.exit(Sys.unsetenv("TAR"))
  } else {
    on.exit(Sys.setenv("TAR" = TAR))
  }

  Sys.unsetenv("TAR")

  which_tar <- file.path(Sys.which("tar"))

  if (file.exists(which_tar)) {
    expect_equal(tar_binary(), which_tar)
  } else {
    expect_equal(tar_binary(), "internal")
  }
})

test_that("On Windows, use the system tar if it exists, otherwise internal", {
  skip_on_os(c("mac", "linux", "solaris"))
  TAR <- Sys.getenv("TAR")
  if (is.na(TAR)) {
    on.exit(Sys.unsetenv("TAR"))
  } else {
    on.exit(Sys.setenv("TAR" = TAR))
  }

  Sys.unsetenv("TAR")

  root <- Sys.getenv("SystemRoot", unset = NA)
  if (is.na(root)) {
    root <- "C:/Windows"
  }
  tar <- file.path(root, "System32/tar.exe")

  if (file.exists(tar)) {
    expect_equal(tar_binary(), tar)
  } else {
    expect_equal(tar_binary(), "internal")
  }
})
