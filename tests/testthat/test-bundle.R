test_that("Bundle works when using R's internal tar", {

  skip_on_cran()
  skip_on_travis()
  skip_on_ci()
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
  skip_on_ci()
  scopeTestContext()

  checker <- function() {
    # we shouldn't see any CRAN packages in the unbundled sources other than Packrat
    srcDir <- "untarred/packrat-test-bundle/packrat/src"
    srcFiles <- list.files(srcDir, pattern = "tar.gz$", recursive = TRUE)
    expect_true(length(srcFiles) == 1, "src dir should be empty (other than Packrat)")
  }

  bundle_test(packrat:::bundle, checker, omit.cran.src = TRUE)

})
