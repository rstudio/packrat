test_that("we can hash packages containing multiple packages in LinkingTo", {
  skip_on_cran()

  path <- system.file("DESCRIPTION", package = "Rclusterpp")
  if (file.exists(path)) {
    hash(path)
  }
})

test_that("hash function is available and has expected arguments", {
  skip_on_cran()

  # This test is a canary that you may be breaking compatibility with Connect,
  # which expects this non-exported function to be present. That's not to say
  # the function signature can't be changed, only that the relevant call in
  # Connect's packrat_restore.R should be studied to avoid any breakage.
  expect_identical(
    formals(hash),
    pairlist(path = quote(expr =), descLookup = as.name("installedDescLookup"))
  )

})

test_that("hash treats some RemoteType values as CRAN", {
  # DESCRIPTION as created by install.packages
  simpleDescription <- tempfile()
  on.exit(unlink(simpleDescription), add = TRUE)
  writeLines(c(
    "Package: dummy",
    "Version: 1.0.0"
  ), simpleDescription)
  simpleHash <- hash(simpleDescription)

  cranDescription <- tempfile()
  on.exit(unlink(cranDescription), add = TRUE)
  writeLines(c(
    "Package: dummy",
    "Version: 1.0.0",
    "RemoteType: cran"
  ), cranDescription)
  cranHash <- hash(cranDescription)
  expect_equal(cranHash, simpleHash)

  standardDescription <- tempfile()
  on.exit(unlink(standardDescription), add = TRUE)
  writeLines(c(
    "Package: dummy",
    "Version: 1.0.0",
    "RemoteType: standard"
  ), standardDescription)
  standardHash <- hash(standardDescription)
  expect_equal(standardHash, simpleHash)

  urlDescription <- tempfile()
  on.exit(unlink(urlDescription), add = TRUE)
  writeLines(c(
    "Package: dummy",
    "Version: 1.0.0",
    "RemoteType: url",
    "RemoteUrl: https://cran.rstudio.com//src/contrib/Archive/dummy/dummy_1.1.0.tar.gz"
  ), urlDescription)
  urlHash <- hash(urlDescription)
  expect_equal(urlHash, simpleHash)
})
