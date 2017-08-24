context("Bundle")

test_that("Bundle works when using R's internal tar", {

  skip_on_cran()
  skip_on_travis()
  scopeTestContext()

  # force packrat to use the internal R tar
  TAR <- Sys.getenv("TAR")
  Sys.unsetenv("TAR")
  on.exit(Sys.setenv(TAR = TAR), add = TRUE)

  # bundle with the regular bundle and verify
  bundle_test(packrat::bundle, function() {
    expect_identical(
      grep("lib*", list.files("packrat"), value = TRUE, invert = TRUE),
      list.files("untarred/packrat-test-bundle/packrat/")
    )
  })

})

test_that("Bundle works when omitting CRAN packages", {

  skip_on_cran()
  skip_on_travis()
  scopeTestContext()

  checker <- function() {
    # we shouldn't see any CRAN packages in the unbundled sources
    srcDir <- "untarred/packrat-test-bundle/packrat/src"
    srcFiles <- list.files(srcDir, pattern = "tar.gz$", recursive = TRUE)
    cat(paste(srcFiles, collapse = "\n"))
    expect_true(length(srcFiles) == 0, "src dir should be empty")
  }

  bundle_test(packrat:::bundle, checker, omit.cran.src = TRUE)

})
