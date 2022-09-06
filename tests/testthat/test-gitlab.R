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
