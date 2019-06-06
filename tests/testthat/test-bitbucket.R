context("Bitbucket")

test_that("we can use devtools:::download to retrieve Bitbucket archives", {
  skip("run manually for now")

  if (!canUseBitbucketDownloader())
    skip("requires devtools")

  url <- "https://bitbucket.org/mariamedp/packrat-test-pkg/get/5a6b90280c5fec133efb88aec95014d4f9aef80f.tar.gz"
  destination <- tempfile("packrat-test-bitbucket-", fileext = ".tar.gz")
  result <- bitbucketDownload(url, destination)
  expect_true(result == 0)
  expect_true(file.exists(destination))
  unlink(destination)

})
