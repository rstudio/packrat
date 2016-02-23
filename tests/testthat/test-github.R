context("GitHub")

test_that("we can use devtools:::download to retrieve GitHub archives", {

  skip("run manually for now")

  if (!canUseGitHubDownloader())
    skip("requires devtools")

  url <- "https://github.com/rstudio/packrat/archive/cd0f9a4dae7ea0c79966b6784b44d7e4e4edadad.zip"
  destination <- tempfile("packrat-test-gh-", fileext = ".zip")
  result <- githubDownload(url, destination)
  expect_true(result == 0)
  expect_true(file.exists(destination))
  unlink(destination)

})
