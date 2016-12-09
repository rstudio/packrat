library(testthat)

# Set up test context.
context("Cache")

withTestContext({
  # https://github.com/rstudio/packrat/issues/345
  test_that("package installation when configured with a a cache uses the cache", {
    skip_on_cran()

    projRoot <- cloneTestProject("healthy")
    libRoot <- file.path(projRoot,"packrat","lib")
    srcRoot <- file.path(projRoot,"packtar","src")
    theCache <- tempdir()

    packrat::init(projRoot, options = list(local.repos = "packages"))

    rv <- R.Version()
    package_dir <- file.path(projRoot,
                             "packrat",
                             "lib",
                             rv$platform,
                             getRversion(),
                             "oatmeal")

    # After the init and before we configure a cache, the package is a
    # non-symlink. dir.exists only tells us if the package is installed, as it
    # reads through symlinks.
    expect_true(dir.exists(package_dir),package_dir)
    expect_false(packrat:::is.symlink(package_dir), package_dir)

    Sys.setenv(R_PACKRAT_CACHE_DIR = theCache)
    on.exit(Sys.unsetenv("R_PACKRAT_CACHE_DIR"))

    packrat::set_opts(use.cache = TRUE)
    on.exit(packrat::set_opts(use.cache = FALSE))

    # Initial restore. Populates the cache and creates a symlink into it.
    unlink(libRoot, recursive = TRUE)
    unlink(srcRoot, recursive = TRUE)
    packrat::restore(overwrite.dirty = TRUE,
                     prompt = FALSE,
                     restart = FALSE)

    expect_true(dir.exists(package_dir),package_dir)
    expect_true(packrat:::is.symlink(package_dir), package_dir)

    # Subsequent restore. Uses the cache.
    unlink(libRoot, recursive = TRUE)
    unlink(srcRoot, recursive = TRUE)
    packrat::restore(overwrite.dirty = TRUE,
                     prompt = FALSE,
                     restart = FALSE)

    expect_true(dir.exists(package_dir),package_dir)
    expect_true(packrat:::is.symlink(package_dir), package_dir)
  })
})
