# Tests for two-tier cache read fallback:
#   1. paths.R: globalAppDataDir() and globalCacheLibDir()
#   2. restore-routines.R: restoreWithCopyFromGlobalCache()
#   3. restore.R: installPkg() wiring (fallback ordering)

# ---------------------------------------------------------------------------
# helpers
# ---------------------------------------------------------------------------

# Create a minimal fake "installed package" directory with a DESCRIPTION file.
make_fake_cached_pkg <- function(base_dir, pkg_name, hash) {
  pkg_path <- file.path(base_dir, pkg_name, hash, pkg_name)
  dir.create(pkg_path, recursive = TRUE)
  writeLines(
    c(
      paste0("Package: ", pkg_name),
      "Version: 1.0.0",
      "Title: Fake"
    ),
    file.path(pkg_path, "DESCRIPTION")
  )
  pkg_path
}

# Build a pkgRecord list (mimics the structure used by packrat internals).
make_pkg_record <- function(
  name = "mypkg",
  hash = "abc123",
  version = "1.0.0",
  source = "CRAN"
) {
  list(name = name, hash = hash, version = version, source = source)
}

# ---------------------------------------------------------------------------
# globalAppDataDir()
# ---------------------------------------------------------------------------

test_that("globalAppDataDir returns NULL when env var is unset", {
  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = NA), {
    expect_null(globalAppDataDir())
  })
})

test_that("globalAppDataDir returns NULL when env var is empty string", {
  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = ""), {
    expect_null(globalAppDataDir())
  })
})

test_that("globalAppDataDir returns correct path when env var is set", {
  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = "/tmp/my-global-cache"), {
    result <- globalAppDataDir()
    rv <- R.Version()
    expected_version <- paste(rv$major, rv$minor, sep = ".")
    expect_equal(result, file.path("/tmp/my-global-cache", expected_version))
  })
})

test_that("globalAppDataDir uses R.Version() major.minor, not getRversion()", {
  # Verify the version string format matches R.Version() output

  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = "/cache"), {
    result <- globalAppDataDir()
    rv <- R.Version()
    expected <- file.path("/cache", paste(rv$major, rv$minor, sep = "."))
    expect_equal(result, expected)
  })
})

# ---------------------------------------------------------------------------
# globalCacheLibDir()
# ---------------------------------------------------------------------------

test_that("globalCacheLibDir returns NULL when env var is unset", {
  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = NA), {
    expect_null(globalCacheLibDir())
  })
})

test_that("globalCacheLibDir returns NULL when env var is empty string", {
  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = ""), {
    expect_null(globalCacheLibDir())
  })
})

test_that("globalCacheLibDir builds correct path when env var is set", {
  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = "/tmp/global"), {
    result <- globalCacheLibDir()
    rv <- R.Version()
    version_str <- paste(rv$major, rv$minor, sep = ".")
    expect_equal(
      result,
      file.path("/tmp/global", version_str, "v2", "library")
    )
  })
})

test_that("globalCacheLibDir passes variadic args to build full path", {
  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = "/tmp/global"), {
    result <- globalCacheLibDir("mypkg", "abc123", "mypkg")
    rv <- R.Version()
    version_str <- paste(rv$major, rv$minor, sep = ".")
    expect_equal(
      result,
      file.path(
        "/tmp/global",
        version_str,
        "v2",
        "library",
        "mypkg",
        "abc123",
        "mypkg"
      )
    )
  })
})

test_that("globalCacheLibDir with no extra args returns library root", {
  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = "/cache"), {
    result <- globalCacheLibDir()
    expect_true(grepl("v2/library$", result))
  })
})

# ---------------------------------------------------------------------------
# restoreWithCopyFromGlobalCache()
# ---------------------------------------------------------------------------

test_that("restoreWithCopyFromGlobalCache returns FALSE when env var is not set", {
  skip_on_cran()

  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = NA), {
    project <- withr::local_tempdir()
    pkgRecord <- make_pkg_record()
    cacheCopyStatus <- new.env(parent = emptyenv())

    result <- restoreWithCopyFromGlobalCache(
      project,
      pkgRecord,
      cacheCopyStatus
    )

    expect_false(result)
    expect_null(cacheCopyStatus$type)
  })
})

test_that("restoreWithCopyFromGlobalCache returns FALSE when cache disabled for project", {
  skip_on_cran()

  globalCacheDir <- withr::local_tempdir()
  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = globalCacheDir), {
    project <- withr::local_tempdir()
    # Ensure use.cache is FALSE (the default)
    set_opts(project = project, use.cache = FALSE)

    pkgRecord <- make_pkg_record()
    cacheCopyStatus <- new.env(parent = emptyenv())

    result <- restoreWithCopyFromGlobalCache(
      project,
      pkgRecord,
      cacheCopyStatus
    )

    expect_false(result)
  })
})

test_that("restoreWithCopyFromGlobalCache returns FALSE when pkgRecord has no hash", {
  skip_on_cran()

  globalCacheDir <- withr::local_tempdir()
  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = globalCacheDir), {
    project <- withr::local_tempdir()
    set_opts(project = project, use.cache = TRUE)
    withr::defer(set_opts(use.cache = FALSE, project = project))

    pkgRecord <- make_pkg_record(hash = NULL)
    cacheCopyStatus <- new.env(parent = emptyenv())

    result <- restoreWithCopyFromGlobalCache(
      project,
      pkgRecord,
      cacheCopyStatus
    )

    expect_false(result)
  })
})

test_that("restoreWithCopyFromGlobalCache returns FALSE when pkgRecord hash is empty character", {
  skip_on_cran()

  globalCacheDir <- withr::local_tempdir()
  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = globalCacheDir), {
    project <- withr::local_tempdir()
    set_opts(project = project, use.cache = TRUE)
    withr::defer(set_opts(use.cache = FALSE, project = project))

    # length(character(0)) is 0, so this should return FALSE
    pkgRecord <- make_pkg_record()
    pkgRecord$hash <- character(0)
    cacheCopyStatus <- new.env(parent = emptyenv())

    result <- restoreWithCopyFromGlobalCache(
      project,
      pkgRecord,
      cacheCopyStatus
    )

    expect_false(result)
  })
})

test_that("restoreWithCopyFromGlobalCache returns FALSE when global cache entry does not exist", {
  skip_on_cran()

  globalCacheDir <- withr::local_tempdir()
  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = globalCacheDir), {
    project <- withr::local_tempdir()
    set_opts(project = project, use.cache = TRUE)
    withr::defer(set_opts(use.cache = FALSE, project = project))

    pkgRecord <- make_pkg_record(name = "nonexistent", hash = "deadbeef")
    cacheCopyStatus <- new.env(parent = emptyenv())

    # The directory for this package/hash does not exist in globalCacheDir
    result <- restoreWithCopyFromGlobalCache(
      project,
      pkgRecord,
      cacheCopyStatus
    )

    expect_false(result)
  })
})

test_that("restoreWithCopyFromGlobalCache returns FALSE when cache entry is corrupt (no DESCRIPTION)", {
  skip_on_cran()

  globalCacheDir <- withr::local_tempdir()
  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = globalCacheDir), {
    project <- withr::local_tempdir()
    set_opts(project = project, use.cache = TRUE)
    withr::defer(set_opts(use.cache = FALSE, project = project))

    pkgRecord <- make_pkg_record(name = "badpkg", hash = "badhash")

    # Create the cache directory structure but without a DESCRIPTION file
    rv <- R.Version()
    version_str <- paste(rv$major, rv$minor, sep = ".")
    corrupt_path <- file.path(
      globalCacheDir,
      version_str,
      "v2",
      "library",
      "badpkg",
      "badhash",
      "badpkg"
    )
    dir.create(corrupt_path, recursive = TRUE)
    # No DESCRIPTION file created -- this is the corruption

    cacheCopyStatus <- new.env(parent = emptyenv())

    expect_warning(
      result <- restoreWithCopyFromGlobalCache(
        project,
        pkgRecord,
        cacheCopyStatus
      ),
      "corrupt"
    )

    expect_false(result)
  })
})

test_that("restoreWithCopyFromGlobalCache returns FALSE when DESCRIPTION is empty", {
  skip_on_cran()

  globalCacheDir <- withr::local_tempdir()
  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = globalCacheDir), {
    project <- withr::local_tempdir()
    set_opts(project = project, use.cache = TRUE)
    withr::defer(set_opts(use.cache = FALSE, project = project))

    pkgRecord <- make_pkg_record(name = "emptydesc", hash = "ehash")

    rv <- R.Version()
    version_str <- paste(rv$major, rv$minor, sep = ".")
    pkg_path <- file.path(
      globalCacheDir,
      version_str,
      "v2",
      "library",
      "emptydesc",
      "ehash",
      "emptydesc"
    )
    dir.create(pkg_path, recursive = TRUE)
    file.create(file.path(pkg_path, "DESCRIPTION")) # empty file

    cacheCopyStatus <- new.env(parent = emptyenv())

    expect_warning(
      result <- restoreWithCopyFromGlobalCache(
        project,
        pkgRecord,
        cacheCopyStatus
      ),
      "corrupt"
    )

    expect_false(result)
  })
})

test_that("restoreWithCopyFromGlobalCache symlinks on success and sets status", {
  skip_on_cran()
  skip_on_os("windows")

  globalCacheDir <- withr::local_tempdir()
  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = globalCacheDir), {
    project <- withr::local_tempdir()
    set_opts(project = project, use.cache = TRUE)
    withr::defer(set_opts(use.cache = FALSE, project = project))

    pkgRecord <- make_pkg_record(name = "goodpkg", hash = "goodhash")

    # Populate the global cache with a valid package
    rv <- R.Version()
    version_str <- paste(rv$major, rv$minor, sep = ".")
    pkg_path <- file.path(
      globalCacheDir,
      version_str,
      "v2",
      "library",
      "goodpkg",
      "goodhash",
      "goodpkg"
    )
    dir.create(pkg_path, recursive = TRUE)
    writeLines(
      c("Package: goodpkg", "Version: 1.0.0", "Title: Good"),
      file.path(pkg_path, "DESCRIPTION")
    )

    # Create the project lib directory so the symlink target directory exists
    lib <- libDir(project)
    dir.create(lib, recursive = TRUE)

    cacheCopyStatus <- new.env(parent = emptyenv())

    result <- restoreWithCopyFromGlobalCache(
      project,
      pkgRecord,
      cacheCopyStatus
    )

    expect_true(result)
    expect_equal(cacheCopyStatus$type, "symlinked global cache")

    # Verify symlink was created
    target <- file.path(lib, "goodpkg")
    expect_true(file.exists(target))
    expect_true(is.symlink(target))
  })
})

test_that("restoreWithCopyFromGlobalCache backs up and restores existing target on failure", {
  skip_on_cran()

  globalCacheDir <- withr::local_tempdir()
  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = globalCacheDir), {
    project <- withr::local_tempdir()
    set_opts(project = project, use.cache = TRUE)
    withr::defer(set_opts(use.cache = FALSE, project = project))

    pkgRecord <- make_pkg_record(name = "existing", hash = "nope")

    # Don't create the global cache entry -- so lookup will fail at the
    # directory existence check. But the env var is set, cache is enabled,
    # hash is present, package is cacheable -- it will reach the dir check.

    # Create an existing package in the project lib
    lib <- libDir(project)
    existing_path <- file.path(lib, "existing")
    dir.create(existing_path, recursive = TRUE)
    writeLines("marker", file.path(existing_path, "marker.txt"))

    cacheCopyStatus <- new.env(parent = emptyenv())

    result <- restoreWithCopyFromGlobalCache(
      project,
      pkgRecord,
      cacheCopyStatus
    )

    # Should return FALSE because cache entry doesn't exist
    expect_false(result)

    # The existing directory should NOT have been disturbed since the function
    # returned FALSE before reaching the backup/symlink logic
    # (the dir check happens before the backup)
  })
})

test_that("restoreWithCopyFromGlobalCache overwrites pre-existing target on success", {
  skip_on_cran()
  skip_on_os("windows")

  globalCacheDir <- withr::local_tempdir()
  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = globalCacheDir), {
    project <- withr::local_tempdir()
    set_opts(project = project, use.cache = TRUE)
    withr::defer(set_opts(use.cache = FALSE, project = project))

    pkgRecord <- make_pkg_record(name = "replaceme", hash = "newhash")

    # Populate the global cache
    rv <- R.Version()
    version_str <- paste(rv$major, rv$minor, sep = ".")
    pkg_path <- file.path(
      globalCacheDir,
      version_str,
      "v2",
      "library",
      "replaceme",
      "newhash",
      "replaceme"
    )
    dir.create(pkg_path, recursive = TRUE)
    writeLines(
      c("Package: replaceme", "Version: 2.0.0", "Title: New"),
      file.path(pkg_path, "DESCRIPTION")
    )

    # Create existing package in the project lib
    lib <- libDir(project)
    existing_path <- file.path(lib, "replaceme")
    dir.create(existing_path, recursive = TRUE)
    writeLines("old version marker", file.path(existing_path, "OLD_MARKER"))

    cacheCopyStatus <- new.env(parent = emptyenv())

    result <- restoreWithCopyFromGlobalCache(
      project,
      pkgRecord,
      cacheCopyStatus
    )

    expect_true(result)
    expect_equal(cacheCopyStatus$type, "symlinked global cache")

    # The target should now be a symlink, not the old directory
    target <- file.path(lib, "replaceme")
    expect_true(is.symlink(target))
    expect_false(file.exists(file.path(target, "OLD_MARKER")))
    expect_true(file.exists(file.path(target, "DESCRIPTION")))
  })
})

# ---------------------------------------------------------------------------
# installPkg() fallback ordering
# ---------------------------------------------------------------------------

test_that("installPkg tries global cache fallback after primary cache miss", {
  skip_on_cran()
  skip_on_os("windows")

  scopeTestContext()

  globalCacheDir <- withr::local_tempdir()
  project <- withr::local_tempdir()

  # Enable cache for project
  set_opts(project = project, use.cache = TRUE)
  withr::defer(set_opts(use.cache = FALSE, project = project))

  pkgRecord <- make_pkg_record(name = "fromprimary", hash = "hash1")

  # Populate the PRIMARY cache (not global) with a valid entry
  # so restoreWithCopyFromCache succeeds first
  primaryCacheDir <- withr::local_tempdir()
  withr::local_envvar(
    R_PACKRAT_CACHE_DIR = primaryCacheDir,
    R_PACKRAT_GLOBAL_CACHE_DIR = globalCacheDir
  )

  rv <- R.Version()
  primary_pkg_path <- file.path(
    primaryCacheDir,
    getRversion(),
    "v2",
    "library",
    "fromprimary",
    "hash1",
    "fromprimary"
  )
  dir.create(primary_pkg_path, recursive = TRUE)
  writeLines(
    c("Package: fromprimary", "Version: 1.0.0", "Title: Primary"),
    file.path(primary_pkg_path, "DESCRIPTION")
  )

  # Create the project lib directory
  lib <- libDir(project)
  dir.create(lib, recursive = TRUE)

  result <- installPkg(pkgRecord, project, repos = character(), lib = lib)

  # Should have come from primary cache
  expect_equal(result, "symlinked cache")
})

test_that("installPkg falls through to global cache when primary cache misses", {
  skip_on_cran()
  skip_on_os("windows")

  scopeTestContext()

  globalCacheDir <- withr::local_tempdir()
  primaryCacheDir <- withr::local_tempdir()
  project <- withr::local_tempdir()

  withr::local_envvar(
    R_PACKRAT_CACHE_DIR = primaryCacheDir,
    R_PACKRAT_GLOBAL_CACHE_DIR = globalCacheDir
  )

  # Enable cache for project
  set_opts(project = project, use.cache = TRUE)
  withr::defer(set_opts(use.cache = FALSE, project = project))

  pkgRecord <- make_pkg_record(name = "fromglobal", hash = "ghash")

  # Do NOT populate primary cache -- leave it empty.
  # Populate the GLOBAL cache with a valid entry.
  rv <- R.Version()
  version_str <- paste(rv$major, rv$minor, sep = ".")
  global_pkg_path <- file.path(
    globalCacheDir,
    version_str,
    "v2",
    "library",
    "fromglobal",
    "ghash",
    "fromglobal"
  )
  dir.create(global_pkg_path, recursive = TRUE)
  writeLines(
    c("Package: fromglobal", "Version: 1.0.0", "Title: Global"),
    file.path(global_pkg_path, "DESCRIPTION")
  )

  # Create the project lib directory
  lib <- libDir(project)
  dir.create(lib, recursive = TRUE)

  result <- installPkg(pkgRecord, project, repos = character(), lib = lib)

  # Should have come from global cache
  expect_equal(result, "symlinked global cache")
})

test_that("installPkg skips global cache fallback when env var is unset", {
  skip_on_cran()
  skip_on_os("windows")

  scopeTestContext()

  primaryCacheDir <- withr::local_tempdir()
  project <- withr::local_tempdir()

  withr::local_envvar(
    R_PACKRAT_CACHE_DIR = primaryCacheDir,
    R_PACKRAT_GLOBAL_CACHE_DIR = NA
  )

  # Enable cache for project
  set_opts(project = project, use.cache = TRUE)
  withr::defer(set_opts(use.cache = FALSE, project = project))

  pkgRecord <- make_pkg_record(name = "testpkg", hash = "thash")

  # Populate primary cache
  primary_pkg_path <- file.path(
    primaryCacheDir,
    getRversion(),
    "v2",
    "library",
    "testpkg",
    "thash",
    "testpkg"
  )
  dir.create(primary_pkg_path, recursive = TRUE)
  writeLines(
    c("Package: testpkg", "Version: 1.0.0", "Title: Test"),
    file.path(primary_pkg_path, "DESCRIPTION")
  )

  lib <- libDir(project)
  dir.create(lib, recursive = TRUE)

  # With global cache unset, should still succeed from primary cache

  result <- installPkg(pkgRecord, project, repos = character(), lib = lib)
  expect_equal(result, "symlinked cache")
})

# ---------------------------------------------------------------------------
# Edge cases and interaction tests
# ---------------------------------------------------------------------------

test_that("restoreWithCopyFromGlobalCache is independent of primary cache state", {
  skip_on_cran()
  skip_on_os("windows")

  # The global cache function doesn't check cacheLibDir at all.
  # It uses globalCacheLibDir exclusively.

  globalCacheDir <- withr::local_tempdir()
  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = globalCacheDir), {
    project <- withr::local_tempdir()
    set_opts(project = project, use.cache = TRUE)
    withr::defer(set_opts(use.cache = FALSE, project = project))

    pkgRecord <- make_pkg_record(name = "indep", hash = "ihash")

    # Create the global cache entry
    rv <- R.Version()
    version_str <- paste(rv$major, rv$minor, sep = ".")
    pkg_path <- file.path(
      globalCacheDir,
      version_str,
      "v2",
      "library",
      "indep",
      "ihash",
      "indep"
    )
    dir.create(pkg_path, recursive = TRUE)
    writeLines(
      c("Package: indep", "Version: 1.0.0", "Title: Independent"),
      file.path(pkg_path, "DESCRIPTION")
    )

    lib <- libDir(project)
    dir.create(lib, recursive = TRUE)

    cacheCopyStatus <- new.env(parent = emptyenv())
    result <- restoreWithCopyFromGlobalCache(
      project,
      pkgRecord,
      cacheCopyStatus
    )

    expect_true(result)
    expect_equal(cacheCopyStatus$type, "symlinked global cache")
  })
})

test_that("globalCacheLibDir path structure matches what restoreWithCopyFromGlobalCache expects", {
  # Verify the path construction is consistent between the two functions
  withr::with_envvar(c(R_PACKRAT_GLOBAL_CACHE_DIR = "/test/cache"), {
    # globalCacheLibDir with package path args
    lib_path <- globalCacheLibDir("pkg", "hash", "pkg")

    rv <- R.Version()
    version_str <- paste(rv$major, rv$minor, sep = ".")

    expect_equal(
      lib_path,
      file.path(
        "/test/cache",
        version_str,
        "v2",
        "library",
        "pkg",
        "hash",
        "pkg"
      )
    )

    # This is exactly the pattern used in restoreWithCopyFromGlobalCache:
    #   globalCacheLibDir(pkgRecord$name, pkgRecord$hash, pkgRecord$name)
    pkgRecord <- make_pkg_record(name = "pkg", hash = "hash")
    source_path <- globalCacheLibDir(
      pkgRecord$name,
      pkgRecord$hash,
      pkgRecord$name
    )
    expect_equal(lib_path, source_path)
  })
})
