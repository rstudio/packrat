context("Cache")

# https://github.com/rstudio/packrat/issues/345
test_that("package installation when configured with a a cache uses the cache", {
  skip_on_cran()
  skip_on_os("windows")

  scopeTestContext()

  projRoot <- cloneTestProject("healthy")
  libRoot <- file.path(projRoot, "packrat", "lib")
  srcRoot <- file.path(projRoot, "packrat", "src")

  theCache <- tempfile("packrat-cache-")
  ensureDirectory(theCache)
  Sys.setenv(R_PACKRAT_CACHE_DIR = theCache)
  on.exit(Sys.unsetenv("R_PACKRAT_CACHE_DIR"), add = TRUE)

  init(projRoot, options = list(local.repos = "packages"), enter = FALSE)

  rv <- R.Version()
  packageDir <- file.path(libDir(projRoot), "oatmeal")

  expect_true(file.exists(packageDir), packageDir)
  expect_false(is.symlink(packageDir), packageDir)

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

  expect_true(file.exists(packageDir), packageDir)
  expect_true(is.symlink(packageDir), packageDir)

  # Subsequent restore. Uses the cache.
  unlink(libRoot, recursive = TRUE)
  unlink(srcRoot, recursive = TRUE)
  restore(projRoot,
          overwrite.dirty = TRUE,
          prompt = FALSE,
          restart = FALSE)

  # Daisy-chain a test where we attempt to recover when
  # a cache entry is corrupt. This test models some real-life
  # situations we've seen where a cache's package entries seem
  # to lose all files except for an empty DESCRIPTION.
  pkgDir <- file.path(libDir(projRoot), "oatmeal")
  cacheEntry <- system(paste("readlink", pkgDir), intern = TRUE)
  unlink(cacheEntry, recursive = TRUE)
  ensureDirectory(cacheEntry)
  file.create(file.path(cacheEntry, "DESCRIPTION"))

  unlink(libRoot, recursive = TRUE)
  unlink(srcRoot, recursive = TRUE)

  restore(projRoot,
          overwrite.dirty = TRUE,
          prompt = FALSE,
          restart = FALSE)

  expect_true(file.exists(packageDir), packageDir)
  expect_true(is.symlink(packageDir), packageDir)
})

test_that("packrat uses the untrusted cache when instructed", {
  skip_on_cran()
  skip_on_os("windows")

  scopeTestContext()

  # pretend that we're RStudio Connect
  Sys.setenv(RSTUDIO_CONNECT = 1)
  on.exit(Sys.unsetenv("RSTUDIO_CONNECT"), add = TRUE)

  projRoot <- cloneTestProject("healthy")
  libRoot <- file.path(projRoot, "packrat", "lib")
  srcRoot <- file.path(projRoot, "packrat", "src")

  theCache <- tempfile("packrat-cache-")
  ensureDirectory(theCache)
  Sys.setenv(R_PACKRAT_CACHE_DIR = theCache)
  on.exit(Sys.unsetenv("R_PACKRAT_CACHE_DIR"), add = TRUE)

  init(projRoot, options = list(local.repos = "packages"), enter = FALSE)

  rv <- R.Version()
  packageDir <- file.path(libDir(projRoot), "oatmeal")

  expect_true(file.exists(packageDir), packageDir)
  expect_false(is.symlink(packageDir), packageDir)

  set_opts(project = projRoot, use.cache = TRUE)
  on.exit(set_opts(use.cache = FALSE, project = projRoot), add = TRUE)

  options(packrat.verbose.cache = TRUE)
  on.exit(options(packrat.verbose.cache = FALSE), add = TRUE)

  # Initial restore. Populates the cache and creates a symlink into it.
  unlink(libRoot, recursive = TRUE)
  restore(projRoot,
          overwrite.dirty = TRUE,
          prompt = FALSE,
          restart = FALSE)

  expect_true(file.exists(packageDir), packageDir)
  expect_true(is.symlink(packageDir), packageDir)

  # Subsequent restore. Uses the cache.
  unlink(libRoot, recursive = TRUE)
  restore(projRoot,
          overwrite.dirty = TRUE,
          prompt = FALSE,
          restart = FALSE)

  expect_true(file.exists(packageDir), packageDir)
  expect_true(is.symlink(packageDir), packageDir)
})
