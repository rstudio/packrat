context("GitHub")

test_that("we can use devtools:::download to retrieve GitHub archives", {
  skip("run manually for now")

  if (!canUseGitHubDownloader())
    skip("requires devtools")

  url <- "https://api.github.com/repos/rstudio/packrat/tarball/cd0f9a4dae7ea0c79966b6784b44d7e4e4edadad"
  destination <- tempfile("packrat-test-gh-", fileext = ".tar.gz")
  result <- githubDownload(url, destination)
  expect_true(result == 0)
  expect_true(file.exists(destination))
  unlink(destination)

})
