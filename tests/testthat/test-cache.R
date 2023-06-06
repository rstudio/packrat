library(testthat)

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

  suppressWarnings(
    restore(projRoot,
            overwrite.dirty = TRUE,
            prompt = FALSE,
            restart = FALSE)
  )

  expect_true(file.exists(packageDir), packageDir)
  expect_true(is.symlink(packageDir), packageDir)
})

test_that("packrat uses the untrusted cache when instructed", {
  skip_on_cran()
  skip_on_os("windows")

  scopeTestContext()

  # pretend that we're Posit Connect
  Sys.setenv(POSIT_CONNECT = 1)
  on.exit(Sys.unsetenv("POSIT_CONNECT"), add = TRUE)

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

# cacheFileBackup ------------------------------------------

test_that("moves the original file aside and returns a function to restore the original", {
  dir <- withr::local_tempdir()

  target <- file.path(dir, "target")
  writeLines("original contents", target)

  restore <- cacheFileBackup(target)
  expect_false(file.exists(target))
  # moved to a temporary name in the same directory.
  expect_length(list.files(dir), 1)

  restore()
  expect_true(file.exists(target))
  expect_length(list.files(dir), 1)
  expect_equal(readLines(target), "original contents")
})

test_that("moves the original file aside and returns a function to remove the original when replaced", {
  dir <- withr::local_tempdir()

  target <- file.path(dir, "target")
  writeLines("original contents", target)

  restore <- cacheFileBackup(target)
  expect_false(file.exists(target))
  # moved to a temporary name in the same directory.
  expect_length(list.files(dir), 1)

  writeLines("replacement contents", target)

  restore()
  expect_true(file.exists(target))
  expect_length(list.files(dir), 1)
  expect_equal(readLines(target), "replacement contents")
})

# moveInstalledPackageToCache ------------------------------

test_that("errs when target exists and not overwriting", {
  dir <- withr::local_tempdir()
  libDir <- file.path(dir, "lib")
  cacheDir <- file.path(dir, "cache")
  dir.create(libDir)
  dir.create(cacheDir)

  packageName <- "pkgName"
  hash <- "decafbad"

  libPackageDir <- file.path(libDir, packageName)
  libDescription <- file.path(libPackageDir, "DESCRIPTION")
  cachePackageDir <- file.path(cacheDir, packageName, hash, packageName)
  cacheDescription <- file.path(cachePackageDir, "DESCRIPTION")

  dir.create(libPackageDir)
  writeLines("Package: pkgName", libDescription)
  dir.create(cachePackageDir, recursive = TRUE)
  writeLines("Package: pkgName-in-cache", cacheDescription)

  expect_error(
    moveInstalledPackageToCache(libPackageDir, hash, cacheDir, FALSE, TRUE, TRUE),
    "cached package already exists at path"
  )

  # library and cache are untouched
  expect_equal(readLines(libDescription), "Package: pkgName")
  expect_equal(readLines(cacheDescription), "Package: pkgName-in-cache")
})

test_that("package is moved into cache", {
  dir <- withr::local_tempdir()
  libDir <- file.path(dir, "lib")
  cacheDir <- file.path(dir, "cache")
  dir.create(libDir)
  dir.create(cacheDir)

  packageName <- "pkgName"
  hash <- "decafbad"

  libPackageDir <- file.path(libDir, packageName)
  libDescription <- file.path(libPackageDir, "DESCRIPTION")
  cachePackageDir <- file.path(cacheDir, packageName, hash, packageName)
  cacheDescription <- file.path(cachePackageDir, "DESCRIPTION")

  dir.create(libPackageDir)
  writeLines("Package: pkgName", libDescription)

  moveInstalledPackageToCache(libPackageDir, hash, cacheDir, FALSE, TRUE, TRUE)

  expect_true(file.exists(cachePackageDir))
  expect_true(file.exists(libPackageDir))

  expect_equal(readLines(libDescription), "Package: pkgName")
  expect_equal(readLines(cacheDescription), "Package: pkgName")

  if (is.windows()) {
  } else {
    expect_equal(Sys.readlink(libPackageDir), cachePackageDir)
  }
})

test_that("package is moved into cache without renaming", {
  dir <- withr::local_tempdir()
  libDir <- file.path(dir, "lib")
  cacheDir <- file.path(dir, "cache")
  dir.create(libDir)
  dir.create(cacheDir)

  packageName <- "pkgName"
  hash <- "decafbad"

  libPackageDir <- file.path(libDir, packageName)
  libDescription <- file.path(libPackageDir, "DESCRIPTION")
  cachePackageDir <- file.path(cacheDir, packageName, hash, packageName)
  cacheDescription <- file.path(cachePackageDir, "DESCRIPTION")

  dir.create(libPackageDir)
  writeLines("Package: pkgName", libDescription)

  moveInstalledPackageToCache(libPackageDir, hash, cacheDir, FALSE, TRUE, FALSE)

  expect_true(file.exists(cachePackageDir))
  expect_true(file.exists(libPackageDir))

  expect_equal(readLines(libDescription), "Package: pkgName")
  expect_equal(readLines(cacheDescription), "Package: pkgName")

  if (is.windows()) {
  } else {
    expect_equal(Sys.readlink(libPackageDir), cachePackageDir)
  }
})


test_that("package is moved into cache when target exists and overwriting", {
  dir <- withr::local_tempdir()
  libDir <- file.path(dir, "lib")
  cacheDir <- file.path(dir, "cache")
  dir.create(libDir)
  dir.create(cacheDir)

  packageName <- "pkgName"
  hash <- "decafbad"

  libPackageDir <- file.path(libDir, packageName)
  libDescription <- file.path(libPackageDir, "DESCRIPTION")
  cachePackageDir <- file.path(cacheDir, packageName, hash, packageName)
  cacheDescription <- file.path(cachePackageDir, "DESCRIPTION")

  dir.create(libPackageDir)
  writeLines("Package: pkgName", libDescription)
  dir.create(cachePackageDir, recursive = TRUE)
  writeLines("Package: pkgName-in-cache", cacheDescription)

  moveInstalledPackageToCache(libPackageDir, hash, cacheDir, TRUE, TRUE)

  expect_true(file.exists(cachePackageDir))
  expect_true(file.exists(libPackageDir))

  expect_equal(readLines(libDescription), "Package: pkgName")
  expect_equal(readLines(cacheDescription), "Package: pkgName")

  if (is.windows()) {
  } else {
    expect_equal(Sys.readlink(libPackageDir), cachePackageDir)
  }
})
