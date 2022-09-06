bitbucket_pkg_record <- list(
  name = "museli",
  source = "bitbucket",
  version = "0.1.0",
  hash = "abc123",
  remote_repo = "museli",
  remote_username = "breakfaster",
  remote_ref = "HEAD",
  remote_sha = "abcde12345",
  remote_host = "api.bitbucket.org/2.0",
  depends = list()
)

test_that("bitbucketArchiveUrl returns the correct URL", {
  mockery::stub(bitbucketArchiveUrl, "secureDownloadMethod", "curl")
  expect_equal(
    bitbucketArchiveUrl(bitbucket_pkg_record),
    "https://bitbucket.org/breakfaster/museli/get/abcde12345.tar.gz"
  )
})
