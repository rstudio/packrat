github_pkg_record <- list(
  name = "muesli",
  source = "github",
  version = "0.1.0",
  hash = "abc123",
  gh_repo = "muesli",
  gh_username = "breakfaster",
  gh_ref = "HEAD",
  gh_sha1 = "abcde12345",
  remote_host = "api.github.com",
  remote_repo = "muesli",
  remote_username = "breakfaster",
  remote_ref = "HEAD",
  remote_sha = "abcde12345",
  depends = list()
)

old_github_pkg_record <- list(
  name = "muesli",
  source = "github",
  version = "0.1.0",
  hash = "abc123",
  gh_repo = "muesli",
  gh_username = "breakfaster",
  gh_ref = "HEAD",
  gh_sha1 = "abcde12345",
  depends = list()
)


test_that("githubArchiveUrl returns the correct URL", {
  mockery::stub(githubArchiveUrl, "secureDownloadMethod", "curl")
  expect_equal(
    githubArchiveUrl(github_pkg_record),
    "https://api.github.com/repos/breakfaster/muesli/tarball/abcde12345"
  )

  expect_equal(
    githubArchiveUrl(old_github_pkg_record),
    "https://api.github.com/repos/breakfaster/muesli/tarball/abcde12345"
  )
})

test_that("githubDownload calls renvDownload in the expected context", {
  url <- githubArchiveUrl(github_pkg_record)
  destfile <- nullfile()

  # Testing the effect of the option, rather than just mocking canUseRenvDownload
  mockery::stub(githubDownload, "canUseRenvDownload", TRUE)
  mockery::stub(githubDownload, "githubAuthenticated", TRUE)
  renv_download_mock <- mockery::mock(destfile)
  mockery::stub(githubDownload, "renvDownload", renv_download_mock, depth = 5)

  githubDownload(url, destfile)

  mockery::expect_called(renv_download_mock, 1)
  mockery::expect_args(renv_download_mock, 1, url, destfile, type = "github")
})

test_that("githubDownload calls githubDownloadHttr in the expected context", {
  url <- githubArchiveUrl(github_pkg_record)
  destfile <- nullfile()

  mockery::stub(githubDownload, "githubAuthenticated", TRUE)
  mockery::stub(githubDownload, "canUseRenvDownload", FALSE)
  mockery::stub(githubDownload, "canUseHttr", TRUE)
  httr_download_mock <- mockery::mock(TRUE)
  mockery::stub(githubDownload, "githubDownloadHttr", httr_download_mock, depth = 5)

  githubDownload(url, destfile)

  mockery::expect_called(httr_download_mock, 1)
  mockery::expect_args(httr_download_mock, 1, url, destfile)
})

test_that("githubDownload calls downloadWithRetries in the expected contexts", {
  url <- githubArchiveUrl(github_pkg_record)
  destfile <- nullfile()

  # With auth data but no configured auth-capable method configured

  mockery::stub(githubDownload, "githubAuthenticated", TRUE)
  mockery::stub(githubDownload, "canUseRenvDownload", FALSE)
  mockery::stub(githubDownload, "canUseHttr", FALSE)
  download_with_retries_mock <- mockery::mock(TRUE)
  mockery::stub(githubDownload, "downloadWithRetries", download_with_retries_mock, depth = 5)

  githubDownload(url, destfile)

  mockery::expect_called(download_with_retries_mock, 1)
  mockery::expect_args(download_with_retries_mock, 1, url, destfile)

  # With auth-capable methods configured but no auth data

  mockery::stub(githubDownload, "githubAuthenticated", FALSE)
  mockery::stub(githubDownload, "canUseRenvDownload", TRUE)
  mockery::stub(githubDownload, "canUseHttr", TRUE)
  download_with_retries_mock <- mockery::mock(TRUE)
  mockery::stub(githubDownload, "downloadWithRetries", download_with_retries_mock, depth = 5)

  githubDownload(url, destfile)

  mockery::expect_called(download_with_retries_mock, 1)
  mockery::expect_args(download_with_retries_mock, 1, url, destfile)
})
