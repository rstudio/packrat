context("Bundle")

# For the context of these tests, we need to use a private repo.
repos <- getOption("repos")
pkgType <- getOption("pkgType")
on.exit({
  options("repos"= repos)
  options("pkgType" = pkgType)
}, add = TRUE)
setupTestRepo()

test_that("Bundle works when using R's internal tar", {

  TAR <- Sys.getenv("TAR")
  Sys.setenv(TAR = "")

  owd <- getwd()
  setwd(tempdir())

  dir.create("packrat-test-bundle")
  setwd("packrat-test-bundle")
  packrat::init(enter = FALSE)
  packrat::bundle(file = "test-bundle.tar.gz")
  untar("test-bundle.tar.gz", exdir = "untarred")
  expect_identical(
    grep("lib*", list.files("packrat"), value = TRUE, invert = TRUE),
    list.files("untarred/packrat-test-bundle/packrat/")
  )

  unlink(file.path(tempdir(), "packrat-test-bundle"), recursive = TRUE)
  setwd(owd)
  Sys.setenv(TAR = TAR)

})

test_that("Bundle works when using an external tar", {

  if (Sys.getenv("TAR") != "") {

    owd <- getwd()
    setwd(tempdir())

    dir.create("packrat-test-bundle")
    setwd("packrat-test-bundle")
    packrat::init(enter = FALSE)
    packrat::bundle(file = file.path("test-bundle.tar.gz"))
    untar("test-bundle.tar.gz", exdir = "untarred")
    expect_identical(
      grep("lib*", list.files("packrat"), value = TRUE, invert = TRUE),
      list.files("untarred/packrat-test-bundle/packrat/")
    )

    unlink(file.path(tempdir(), "packrat-test-bundle"), recursive = TRUE)
    setwd(owd)

  }

})
