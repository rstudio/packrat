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

test_that("bitbucketDownload calls renvDownload with the correct values", {
  url <- bitbucketArchiveUrl(bitbucket_pkg_record)
  destfile <- "/dev/null"

  auth_download_option <- options(packrat.authenticated.downloads.use.renv = TRUE)
  on.exit(options(auth_download_option), add = TRUE)

  mockery::stub(bitbucketDownload, "bitbucketAuthenticated", TRUE)
  renv_download_mock <- mockery::mock(destfile)
  mockery::stub(bitbucketDownload, "renvDownload", renv_download_mock, depth = 3)

  bitbucketDownload(url, destfile)

  mockery::expect_called(renv_download_mock, 1)
  mockery::expect_args(renv_download_mock, 1, url, destfile, type = "bitbucket")
})
