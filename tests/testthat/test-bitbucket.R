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

test_that("bitbucketDownload calls renvDownload in the expected context", {
  url <- bitbucketArchiveUrl(bitbucket_pkg_record)
  destfile <- nullfile()

  # Testing the effect of the option, rather than just mocking canUseRenvDownload
  mockery::stub(bitbucketDownload, "canUseRenvDownload", TRUE)
  mockery::stub(bitbucketDownload, "bitbucketAuthenticated", TRUE)
  renv_download_mock <- mockery::mock(destfile)
  mockery::stub(bitbucketDownload, "renvDownload", renv_download_mock, depth = 5)

  bitbucketDownload(url, destfile)

  mockery::expect_called(renv_download_mock, 1)
  mockery::expect_args(renv_download_mock, 1, url, destfile, type = "bitbucket")
})

test_that("bitbucketDownload calls bitbucketDownloadHttr in the expected context", {
  url <- bitbucketArchiveUrl(bitbucket_pkg_record)
  destfile <- nullfile()

  mockery::stub(bitbucketDownload, "bitbucketAuthenticated", TRUE)
  mockery::stub(bitbucketDownload, "canUseRenvDownload", FALSE)
  mockery::stub(bitbucketDownload, "canUseHttr", TRUE)
  httr_download_mock <- mockery::mock(TRUE)
  mockery::stub(bitbucketDownload, "bitbucketDownloadHttr", httr_download_mock, depth = 5)

  bitbucketDownload(url, destfile)

  mockery::expect_called(httr_download_mock, 1)
  mockery::expect_args(httr_download_mock, 1, url, destfile)
})

test_that("bitbucketDownload calls downloadWithRetries in the expected contexts", {
  url <- bitbucketArchiveUrl(bitbucket_pkg_record)
  destfile <- nullfile()

  # With auth data but no configured auth-capable method configured

  mockery::stub(bitbucketDownload, "bitbucketAuthenticated", TRUE)
  mockery::stub(bitbucketDownload, "canUseRenvDownload", FALSE)
  mockery::stub(bitbucketDownload, "canUseHttr", FALSE)
  download_with_retries_mock <- mockery::mock(TRUE)
  mockery::stub(bitbucketDownload, "downloadWithRetries", download_with_retries_mock, depth = 5)

  bitbucketDownload(url, destfile)

  mockery::expect_called(download_with_retries_mock, 1)
  mockery::expect_args(download_with_retries_mock, 1, url, destfile)

  # With auth-capable methods configured but no auth data

  mockery::stub(bitbucketDownload, "bitbucketAuthenticated", FALSE)
  mockery::stub(bitbucketDownload, "canUseRenvDownload", TRUE)
  mockery::stub(bitbucketDownload, "canUseHttr", TRUE)
  download_with_retries_mock <- mockery::mock(TRUE)
  mockery::stub(bitbucketDownload, "downloadWithRetries", download_with_retries_mock, depth = 5)

  bitbucketDownload(url, destfile)

  mockery::expect_called(download_with_retries_mock, 1)
  mockery::expect_args(download_with_retries_mock, 1, url, destfile)
})
