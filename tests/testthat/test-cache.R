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

test_that("packrat can recover from a bad cache", {
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

  # leave only an empty DESCRIPTION file in the oatmeal cache
  allFiles <- dir(theCache, recursive = TRUE, include.dirs = TRUE)
  whichDescFile <- grep("/oatmeal/.*/oatmeal/DESCRIPTION", allFiles)
  whichNonDescFiles <- grep("/oatmeal/.*/oatmeal/.*", allFiles)
  whichNonDescFiles <- whichNonDescFiles[!whichNonDescFiles %in% whichDescFile]

  descFile <- paste(theCache, allFiles[whichDescFile][[1]], sep = "/")

  unlink(
    paste(theCache
          , allFiles[whichNonDescFiles]
          , sep = "/"
          ),
    recursive = TRUE
  )
  write(NULL, file = descFile)

  remaining_files <- dir(theCache, recursive = TRUE, include.dirs = TRUE)
  # only 2 files remaining: folder, and DESCRIPTION
  expect_equal(sum(grepl("/oatmeal/.*/oatmeal", remaining_files)), 2)
  expect_equal(readLines(descFile), "")

  # Subsequent restore. Rebuilds the bad cache
  unlink(libRoot, recursive = TRUE)
  restore(projRoot,
          overwrite.dirty = TRUE,
          prompt = FALSE,
          restart = FALSE)

  expect_true(file.exists(packageDir), packageDir)
  expect_true(is.symlink(packageDir), packageDir)

  # Final restore. Uses the cache.
  unlink(libRoot, recursive = TRUE)
  restore(projRoot,
          overwrite.dirty = TRUE,
          prompt = FALSE,
          restart = FALSE)

  expect_true(file.exists(packageDir), packageDir)
  expect_true(is.symlink(packageDir), packageDir)
})
