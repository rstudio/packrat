context("Cache")

# https://github.com/rstudio/packrat/issues/345
test_that("package installation when configured with a a cache uses the cache", {
  skip_on_cran()
  skip_on_os("windows")

  scopeTestContext()

  projRoot <- cloneTestProject("healthy")
  libRoot <- file.path(projRoot, "packrat", "lib")
  srcRoot <- file.path(projRoot, "packrat", "src")
  theCache <- tempdir()

  init(projRoot, options = list(local.repos = "packages"), enter = FALSE)

  rv <- R.Version()
  package_dir <- file.path(libDir(projRoot), "oatmeal")

  expect_true(file.exists(package_dir), package_dir)
  expect_false(is.symlink(package_dir), package_dir)

  Sys.setenv(R_PACKRAT_CACHE_DIR = theCache)
  on.exit(Sys.unsetenv("R_PACKRAT_CACHE_DIR"), add = TRUE)

  set_opts(project = projRoot, use.cache = TRUE)
  on.exit(set_opts(use.cache = FALSE, project = projRoot), add = TRUE)

  options(packrat.verbose.cache = TRUE)
  on.exit(options(packrat.verbose.cache = FALSE), add = TRUE)

  # Initial restore. Populates the cache and creates a symlink into it.
  unlink(libRoot, recursive = TRUE)
  unlink(srcRoot, recursive = TRUE)
  restore(projRoot,
          overwrite.dirty = TRUE,
          prompt = FALSE,
          restart = FALSE)

  expect_true(file.exists(package_dir), package_dir)
  expect_true(is.symlink(package_dir), package_dir)

  # Subsequent restore. Uses the cache.
  unlink(libRoot, recursive = TRUE)
  unlink(srcRoot, recursive = TRUE)
  restore(projRoot,
          overwrite.dirty = TRUE,
          prompt = FALSE,
          restart = FALSE)

  expect_true(file.exists(package_dir), package_dir)
  expect_true(is.symlink(package_dir), package_dir)
})
