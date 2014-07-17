context("hash")

test_that("we can hash packages containing multiple packages in LinkingTo", {
  path <- system.file("DESCRIPTION", package = "Rclusterpp")
  if (file.exists(path)) {
    hash(path)
  }
})
