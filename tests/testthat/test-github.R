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

test_that("githubDownload calls renvDownload with the correct values", {
  url <- githubArchiveUrl(github_pkg_record)
  destfile <- "/dev/null"

  auth_download_option <- options(packrat.authenticated.downloads.use.renv = TRUE)
  on.exit(options(auth_download_option), add = TRUE)

  mockery::stub(githubDownload, "githubAuthenticated", TRUE)
  renv_download_mock <- mockery::mock(destfile)
  mockery::stub(githubDownload, "renvDownload", renv_download_mock, depth = 3)

  githubDownload(url, destfile)

  mockery::expect_called(renv_download_mock, 1)
  mockery::expect_args(renv_download_mock, 1, url, destfile, type = "github")
})
