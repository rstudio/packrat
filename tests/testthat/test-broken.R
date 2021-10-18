library(testthat)

test_that("we can re-initialize the test repositories", {
  skip_on_cran()
  rebuildTestRepo()
  rebuildEmptyTestRepo()
})
withTestContext({
  test_that("snapshot captures new dependencies", {
    skip_on_cran()
    skip_on_travis()
    skip_on_ci()

    old <- getOption("packrat.verbose.snapshot.dependencies")
    options(packrat.verbose.snapshot.dependencies = TRUE)
    on.exit(options(packrat.verbose.snapshot.dependencies = old), add = TRUE)

    cat("\nrepos", getOption("repos"), "\n")
    cat("\npackrat options begin:\n", paste(names(packrat::get_opts()), "=", packrat::get_opts(), coalesce = "\n"), "packrat options end\n")
    cat("\nR options begin:\n", paste(names(options()), "=", options(), coalesce = "\n"), "R options end\n")
    projRoot <- cloneTestProject("healthy")
    lib <- libDir(projRoot)
    init(enter = FALSE, projRoot, options = list(local.repos = "packages"))

    out <- system(sprintf("cat %s", file.path(projRoot, "packrat/packrat.lock")), intern = TRUE)
    cat("\npackrat/packrat.lock", out, "\n")

    # Simulate the addition of a dependency
    expect_false(file.exists(file.path(lib, "bread")))
    expect_false(file.exists(file.path(lib, "toast")))
    installTestPkg("bread", "1.0.0", lib)
    addTestDependency(projRoot, "toast")  # toast depends on bread
    expect_true(file.exists(file.path(lib, "bread")))

    # Snapshot the new state and make sure we picked up both toast and its
    # dependency, bread
    pkgs <- pkgNames(lockInfo(projRoot))
    expect_equal(pkgs, c("oatmeal", "packrat"))
    expect_false("bread" %in% pkgs)
    expect_false("toast" %in% pkgs)
    snapshot(projRoot)
    pkgs <- pkgNames(lockInfo(projRoot))
    expect_equal(pkgs, c("bread", "oatmeal", "packrat", "toast"))
  })
})
