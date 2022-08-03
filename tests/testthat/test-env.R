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

test_that("On Unix, use tar on the path if it exists", {
  TAR <- Sys.getenv("TAR")
  if (is.na(TAR)) {
    on.exit(Sys.unsetenv("TAR"))
  } else {
    on.exit(Sys.setenv("TAR" = TAR))
  }

  Sys.unsetenv("TAR")

  mockery::stub(tar_binary, "is.unix", TRUE)
  mockery::stub(tar_binary, "is.windows", FALSE)
  mockery::stub(tar_binary, "Sys.which", "/foo/bar/tar")
  mockery::stub(tar_binary, "file.exists", TRUE)

  expect_equal(tar_binary(), "/foo/bar/tar")
})

test_that("On Unix, use 'internal' as a fallback if no tar is found on the PATH", {
  TAR <- Sys.getenv("TAR")
  if (is.na(TAR)) {
    on.exit(Sys.unsetenv("TAR"))
  } else {
    on.exit(Sys.setenv("TAR" = TAR))
  }

  Sys.unsetenv("TAR")

  mockery::stub(tar_binary, "is.unix", TRUE)
  mockery::stub(tar_binary, "is.windows", FALSE)
  mockery::stub(tar_binary, "Sys.which", "")

  expect_warning(expect_equal(tar_binary(), "internal"))
})

test_that("On Windows, use the system tar if it exists", {
  TAR <- Sys.getenv("TAR")
  if (is.na(TAR)) {
    on.exit(Sys.unsetenv("TAR"))
  } else {
    on.exit(Sys.setenv("TAR" = TAR))
  }

  Sys.unsetenv("TAR")

  fake_sys_getenv <- function(x, ...) {
    if (x == "TAR") {
      return(NA)
    } else if (x == "SystemRoot") {
      return("C:/foo")
    }
  }

  mockery::stub(tar_binary, "is.unix", FALSE)
  mockery::stub(tar_binary, "is.windows", TRUE)
  mockery::stub(tar_binary, "Sys.getenv", fake_sys_getenv)
  mockery::stub(tar_binary, "file.path", "C:/foo/tar.exe")
  mockery::stub(tar_binary, "file.exists", TRUE)

  expect_equal(tar_binary(), "C:/foo/tar.exe")
})

test_that("On Windows, use 'internal' as a fallback if system tar doesn't exist", {
  TAR <- Sys.getenv("TAR")
  if (is.na(TAR)) {
    on.exit(Sys.unsetenv("TAR"))
  } else {
    on.exit(Sys.setenv("TAR" = TAR))
  }

  Sys.unsetenv("TAR")

  fake_sys_getenv <- function(x, ...) {
    if (x == "TAR") {
      return(NA)
    } else if (x == "SystemRoot") {
      return("C:/foo")
    }
  }

  mockery::stub(tar_binary, "is.unix", FALSE)
  mockery::stub(tar_binary, "is.windows", TRUE)
  mockery::stub(tar_binary, "Sys.getenv", fake_sys_getenv)
  mockery::stub(tar_binary, "file.path", "C:/foo/tar.exe")
  mockery::stub(tar_binary, "file.exists", FALSE)

  expect_warning(expect_equal(tar_binary(), "internal"))
})
