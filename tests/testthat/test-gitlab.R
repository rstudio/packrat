gitlab_pkg_record <- list(
  name = "museli",
  source = "gitlab",
  version = "0.1.0",
  hash = "abc123",
  remote_repo = "museli",
  remote_username = "breakfaster",
  remote_ref = "HEAD",
  remote_sha = "abcde12345",
  remote_host = "gitlab.com",
  depends = list()
)

gitlab_subgroup_pkg_record <- list(
  name = "museli",
  source = "gitlab",
  version = "0.1.0",
  hash = "abc123",
  remote_repo = "museli/strawberries",
  remote_username = "breakfaster",
  remote_ref = "HEAD",
  remote_sha = "abcde12345",
  remote_host = "gitlab.com",
  depends = list()
)

test_that("gitlabArchiveUrl returns the correct URL", {
  mockery::stub(gitlabArchiveUrl, "secureDownloadMethod", "curl")
  expect_equal(
    gitlabArchiveUrl(gitlab_pkg_record),
    "https://gitlab.com/api/v4/projects/breakfaster%2Fmuseli/repository/archive?sha=abcde12345"
  )
  expect_equal(
    gitlabArchiveUrl(gitlab_subgroup_pkg_record),
    "https://gitlab.com/api/v4/projects/breakfaster%2Fmuseli%2Fstrawberries/repository/archive?sha=abcde12345"
  )
})

test_that("gitlabDownload calls renvDownload in the expected context", {
  url <- gitlabArchiveUrl(gitlab_pkg_record)
  destfile <- nullfile()

  # Testing the effect of the option, rather than just mocking canUseRenvDownload
  mockery::stub(gitlabDownload, "canUseRenvDownload", TRUE)
  mockery::stub(gitlabDownload, "gitlabAuthenticated", TRUE)
  renv_download_mock <- mockery::mock(destfile)
  mockery::stub(gitlabDownload, "renvDownload", renv_download_mock, depth = 5)

  gitlabDownload(url, destfile)

  mockery::expect_called(renv_download_mock, 1)
  mockery::expect_args(renv_download_mock, 1, url, destfile, type = "gitlab")
})

test_that("gitlabDownload calls gitlabDownloadHttr in the expected context", {
  url <- gitlabArchiveUrl(gitlab_pkg_record)
  destfile <- nullfile()

  mockery::stub(gitlabDownload, "gitlabAuthenticated", TRUE)
  mockery::stub(gitlabDownload, "canUseRenvDownload", FALSE)
  mockery::stub(gitlabDownload, "canUseHttr", TRUE)
  httr_download_mock <- mockery::mock(TRUE)
  mockery::stub(gitlabDownload, "gitlabDownloadHttr", httr_download_mock, depth = 5)

  gitlabDownload(url, destfile)

  mockery::expect_called(httr_download_mock, 1)
  mockery::expect_args(httr_download_mock, 1, url, destfile)
})

test_that("gitlabDownload calls downloadWithRetries in the expected contexts", {
  url <- gitlabArchiveUrl(gitlab_pkg_record)
  destfile <- nullfile()

  # With auth data but no configured auth-capable method configured

  mockery::stub(gitlabDownload, "gitlabAuthenticated", TRUE)
  mockery::stub(gitlabDownload, "canUseRenvDownload", FALSE)
  mockery::stub(gitlabDownload, "canUseHttr", FALSE)
  download_with_retries_mock <- mockery::mock(TRUE)
  mockery::stub(gitlabDownload, "downloadWithRetries", download_with_retries_mock, depth = 5)

  gitlabDownload(url, destfile)

  mockery::expect_called(download_with_retries_mock, 1)
  mockery::expect_args(download_with_retries_mock, 1, url, destfile)

  # With auth-capable methods configured but no auth data

  mockery::stub(gitlabDownload, "gitlabAuthenticated", FALSE)
  mockery::stub(gitlabDownload, "canUseRenvDownload", TRUE)
  mockery::stub(gitlabDownload, "canUseHttr", TRUE)
  download_with_retries_mock <- mockery::mock(TRUE)
  mockery::stub(gitlabDownload, "downloadWithRetries", download_with_retries_mock, depth = 5)

  gitlabDownload(url, destfile)

  mockery::expect_called(download_with_retries_mock, 1)
  mockery::expect_args(download_with_retries_mock, 1, url, destfile)
})
