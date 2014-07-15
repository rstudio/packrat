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
  repos <- getOption("repos")
  options(repos = c(CRAN = paste0("file:///", normalizePath("repo-empty"))))
  ## we expect a warning signalling that the package 'breakfast' is not found
  ## in a repo or locally
  expect_error(suppressWarnings(init(enter = FALSE, projRoot)))
  options(repos = repos)
})

test_that("init warns if a package is found in multiple local repos", {
  projRoot <- cloneTestProject("sated")
  expect_warning(init(enter = FALSE, projRoot, options = list(local.repos = c("packages", "other-packages"))))
})

test_that("install_local fails if no repository has been defined", {
  expect_error(install_local("foo"))
})

test_that("packrat::get_opts can read / write atrocious paths", {
  path <- list.files(pattern = "^Ugly")
  opts$local.repos(path)
  readPath <- opts$local.repos()
  expect_identical(path, readPath)
})
