context("Gitlab")

test_that("we can use devtools:::download to retrieve Gitlab archives", {
  skip("run manually for now")

  if (!canUseGitlabDownloader())
    skip("requires devtools")

  url <- "https://gitlab.com/alexkgold/packrat-test-pkg"
  destination <- tempfile("packrat-test-gitlab-", fileext = ".tar.gz")
  result <- gitlabDownload(url, destination)
  expect_true(result == 0)
  expect_true(file.exists(destination))
  unlink(destination)
})
