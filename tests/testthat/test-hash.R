context("hash")

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
