context("Local Repositories")

# For the context of these tests, we need to use a private repo.
repos <- getOption("repos")
pkgType <- getOption("pkgType")
on.exit({
  options("repos" = repos)
  options("pkgType" = pkgType)
}, add = TRUE)
setupTestRepo()

test_that("init fails when package not found in any repo", {
  projRoot <- cloneTestProject("sated")
  expect_error(init(enter = FALSE, projRoot))
})

test_that("init warns if a package is found in multiple local repos", {
  projRoot <- cloneTestProject("sated")
  expect_warning(init(enter = FALSE, projRoot, options = list(local.repos = c("packages", "other-packages"))))
})

test_that("install_local fails if no repository has been defined", {
  expect_error(install_local("foo"))
})
