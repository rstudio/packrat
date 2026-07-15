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
  restore(projRoot, overwrite.dirty = TRUE, prompt = FALSE, restart = FALSE)

  expect_true(file.exists(packageDir), packageDir)
  expect_true(is.symlink(packageDir), packageDir)

  # Subsequent restore. Uses the cache.
  unlink(libRoot, recursive = TRUE)
  unlink(srcRoot, recursive = TRUE)
  restore(projRoot, overwrite.dirty = TRUE, prompt = FALSE, restart = FALSE)

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
    restore(projRoot, overwrite.dirty = TRUE, prompt = FALSE, restart = FALSE)
  )

  expect_true(file.exists(packageDir), packageDir)
  expect_true(is.symlink(packageDir), packageDir)
})

test_that("packrat warns when lockfile hash does not match installed hash", {
  skip_on_cran()
  skip_on_os("windows")

  scopeTestContext()

  projRoot <- cloneTestProject("healthy")
  libRoot <- file.path(projRoot, "packrat", "lib")
  srcRoot <- file.path(projRoot, "packrat", "src")

  theCache <- tempfile("packrat-cache-")
  ensureDirectory(theCache)
  Sys.setenv(R_PACKRAT_CACHE_DIR = theCache)
  on.exit(
    {
      Sys.unsetenv("R_PACKRAT_CACHE_DIR")
      unlink(theCache, recursive = TRUE)
    },
    add = TRUE
  )
  init(projRoot, options = list(local.repos = "packages"), enter = FALSE)

  set_opts(project = projRoot, use.cache = TRUE)
  on.exit(set_opts(use.cache = FALSE, project = projRoot), add = TRUE)

  # Replace one package's hash in the lockfile with a bogus value
  lockfilePath <- file.path(projRoot, "packrat", "packrat.lock")
  lockfileContent <- readLines(lockfilePath)
  hashLines <- grep("^Hash:", lockfileContent)
  lockfileContent[hashLines[1]] <- "Hash: 00000000000000000000000000000000"
  writeLines(lockfileContent, lockfilePath)

  # Remove lib and src so restore must reinstall
  unlink(libRoot, recursive = TRUE)
  unlink(srcRoot, recursive = TRUE)

  # Restore should warn about the hash mismatch
  expect_warning(
    restore(projRoot, overwrite.dirty = TRUE, prompt = FALSE, restart = FALSE),
    "expected hash.*after installation"
  )
})

test_that("moveInstalledPackageToCache caches a fresh package", {
  skip_on_os("windows")

  packagePath <- makeTestPackage("oatmeal")
  cacheDir <- tempfile("packrat-cache-")
  on.exit(
    unlink(c(dirname(packagePath), cacheDir), recursive = TRUE),
    add = TRUE
  )

  hash <- strrep("a", 32)
  cachedPackagePath <- file.path(cacheDir, "oatmeal", hash, "oatmeal")

  cacheCopyStatus <- new.env(parent = emptyenv())
  result <- moveInstalledPackageToCache(
    packagePath,
    hash,
    cacheDir = cacheDir,
    cacheCopyStatus = cacheCopyStatus
  )

  expect_identical(result, cachedPackagePath)
  expect_true(is.symlink(packagePath))
  expect_true(file.exists(file.path(cachedPackagePath, "DESCRIPTION")))

  # a fresh insert isn't a discard, so no status is reported
  expect_null(cacheCopyStatus$type)
})

test_that("moveInstalledPackageToCache adopts a pre-existing cache entry", {
  skip_on_os("windows")

  packagePath <- makeTestPackage("oatmeal", version = "2.0")
  cacheDir <- tempfile("packrat-cache-")
  on.exit(
    unlink(c(dirname(packagePath), cacheDir), recursive = TRUE),
    add = TRUE
  )

  hash <- strrep("a", 32)
  cachedPackagePath <- file.path(cacheDir, "oatmeal", hash, "oatmeal")

  dir.create(cachedPackagePath, recursive = TRUE)
  writeLines(
    c("Package: oatmeal", "Version: 1.0"),
    file.path(cachedPackagePath, "DESCRIPTION")
  )

  cacheCopyStatus <- new.env(parent = emptyenv())
  result <- moveInstalledPackageToCache(
    packagePath,
    hash,
    cacheDir = cacheDir,
    cacheCopyStatus = cacheCopyStatus
  )

  expect_identical(result, cachedPackagePath)
  expect_true(is.symlink(packagePath))

  # the pre-existing cache entry is untouched; the freshly-built package
  # (version 2.0) was discarded in favor of it
  desc <- readLines(file.path(cachedPackagePath, "DESCRIPTION"))
  expect_true("Version: 1.0" %in% desc)
  expect_identical(cacheCopyStatus$type, "symlinked cache")
})

test_that("moveInstalledPackageToCache adopts a competing process's copy", {
  skip_on_os("windows")

  packagePath <- makeTestPackage("oatmeal")
  cacheDir <- tempfile("packrat-cache-")
  on.exit(
    unlink(c(dirname(packagePath), cacheDir), recursive = TRUE),
    add = TRUE
  )

  hash <- strrep("a", 32)
  cachedPackagePath <- file.path(cacheDir, "oatmeal", hash, "oatmeal")

  # Simulate losing the race to populate the cache: renames into the final
  # cache location fail (as when a competing process created the destination
  # first), and the competitor's byte-identical copy appears between the
  # file.exists() check and the second rename.
  realRename <- base::file.rename
  renamesToCache <- 0
  local_mocked_bindings(
    file.rename = function(from, to) {
      if (!identical(to, cachedPackagePath)) {
        return(realRename(from, to))
      }
      renamesToCache <<- renamesToCache + 1
      if (renamesToCache == 2) {
        dir.create(cachedPackagePath, recursive = TRUE)
        writeLines(
          c("Package: oatmeal", "Version: 1.0"),
          file.path(cachedPackagePath, "DESCRIPTION")
        )
      }
      FALSE
    },
    .package = "base"
  )

  cacheCopyStatus <- new.env(parent = emptyenv())
  result <- moveInstalledPackageToCache(
    packagePath,
    hash,
    cacheDir = cacheDir,
    cacheCopyStatus = cacheCopyStatus
  )

  expect_identical(result, cachedPackagePath)
  expect_true(is.symlink(packagePath))
  expect_true(file.exists(file.path(packagePath, "DESCRIPTION")))
  expect_identical(cacheCopyStatus$type, "symlinked cache")
})

test_that("moveInstalledPackageToCache reports a fresh-package cache failure", {
  skip_on_os("windows")

  packagePath <- makeTestPackage("oatmeal")
  cacheDir <- tempfile("packrat-cache-")
  on.exit(
    unlink(c(dirname(packagePath), cacheDir), recursive = TRUE),
    add = TRUE
  )

  hash <- strrep("a", 32)
  cachedPackagePath <- file.path(cacheDir, "oatmeal", hash, "oatmeal")

  # All renames into the final cache location fail and no competing process
  # populates the cache. With no pre-existing cache entry there is nothing to
  # roll back, so the error should report the copy failure rather than a
  # bogus "package may be lost from cache".
  realRename <- base::file.rename
  local_mocked_bindings(
    file.rename = function(from, to) {
      if (!identical(to, cachedPackagePath)) {
        return(realRename(from, to))
      }
      FALSE
    },
    .package = "base"
  )

  expect_error(
    moveInstalledPackageToCache(packagePath, hash, cacheDir = cacheDir),
    "failed to copy package 'oatmeal' to cache"
  )

  # the original installation is untouched
  expect_false(is.symlink(packagePath))
  expect_true(file.exists(file.path(packagePath, "DESCRIPTION")))
})

test_that("packrat uses the untrusted cache when instructed", {
  skip_on_cran()
  skip_on_os("windows")

  scopeTestContext()

  # pretend that we're Posit Connect
  Sys.setenv(POSIT_CONNECT = 1)
  on.exit(
    {
      Sys.unsetenv("POSIT_CONNECT")
      options(packrat.untrusted.packages = NULL)
    },
    add = TRUE
  )

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
  restore(projRoot, overwrite.dirty = TRUE, prompt = FALSE, restart = FALSE)

  expect_true(file.exists(packageDir), packageDir)
  expect_true(is.symlink(packageDir), packageDir)

  # Subsequent restore. Uses the cache.
  unlink(libRoot, recursive = TRUE)
  restore(projRoot, overwrite.dirty = TRUE, prompt = FALSE, restart = FALSE)

  expect_true(file.exists(packageDir), packageDir)
  expect_true(is.symlink(packageDir), packageDir)
})
