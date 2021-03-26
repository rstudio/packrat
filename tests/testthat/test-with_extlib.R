test_that("with_extlib successfully works with no packages provided", {
  skip_on_cran()

  ## Make sure packrat mode is off
  if (packrat:::isPackratModeOn())
    packrat::off()

  orig_libs <- packrat:::getLibPaths()
  .libPaths(c(file.path(getwd(), "packages"), orig_libs))
  on.exit(.libPaths(orig_libs), add = TRUE)

  expect_identical(packageVersion("bread"), package_version("1.0.0"))

  # don't use packrat::on so we can avoid the initialization step
  packrat:::setPackratModeOn(auto.snapshot = FALSE, clean.search.path = FALSE)

  expect_identical(packrat::with_extlib(expr = packageVersion("bread")), package_version("1.0.0"))

  packrat::off()

})
