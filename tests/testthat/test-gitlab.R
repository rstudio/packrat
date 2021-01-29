test_that("we can use devtools:::download to retrieve Gitlab archives", {
  skip("run manually for now")

  if (!canUseGitlabDownloader())
    skip("requires devtools")

  url <- "https://gitlab.com/api/v4/projects/alexkgold%2Fpackrat-test-pkg/repository/archive?sha=e4c14e817e802ce20ba05acc9657f306094d5d23"
  destination <- tempfile("packrat-test-gitlab-", fileext = ".tar.gz")
  result <- gitlabDownload(url, destination)
  expect_true(result == 0)
  expect_true(file.exists(destination))
  unlink(destination)
})
