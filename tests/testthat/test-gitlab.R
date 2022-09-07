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

test_that("gitlabArchiveUrl returns the correct URL", {
  mockery::stub(gitlabArchiveUrl, "secureDownloadMethod", "curl")
  expect_equal(
    gitlabArchiveUrl(gitlab_pkg_record),
    "https://gitlab.com/api/v4/projects/breakfaster%2Fmuseli/repository/archive?sha=abcde12345"
  )
})

test_that("gitlabDownload calls renv$download with the correct values", {
  url <- gitlabArchiveUrl(gitlab_pkg_record)
  destfile <- "/dev/null"

  auth_download_option <- options(packrat.authenticated.downloads.use.renv = TRUE)
  on.exit(options(auth_download_option), add = TRUE)

  mockery::stub(gitlabDownload, "gitlabAuthenticated", TRUE)
  renv_download_mock <- mockery::mock(destfile)
  mockery::stub(gitlabDownload, "renvDownload", renv_download_mock, depth = 3)

  gitlabDownload(url, destfile)

  mockery::expect_called(renv_download_mock, 1)
  mockery::expect_args(renv_download_mock, 1, url, destfile, type = "gitlab")
})
