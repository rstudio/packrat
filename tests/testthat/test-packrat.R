# Packrat tests
#
# To run these tests, set the working directory to packrat/tests and run
# test_check("packrat")
#
# Also run by R CMD CHECK.

library(testthat)

# Set up test context.
context("packrat")


withTestContext({

  test_that("init creates project structure and installs dependencies", {
    skip_on_cran()
    projRoot <- cloneTestProject("sated")
    init(enter = FALSE, projRoot, options = list(local.repos = "packages"))
    lib <- libDir(projRoot)
    expect_true(file.exists(lockFilePath(projRoot)))
    expect_true(file.exists(srcDir(projRoot)))
    expect_true(file.exists(libDir(projRoot)))
    expect_true(file.exists(file.path(lib, "breakfast")))
    expect_true(file.exists(file.path(lib, "bread")))
    expect_true(file.exists(file.path(lib, "oatmeal")))
    expect_true(file.exists(file.path(lib, "packrat")))
    expect_true(file.exists(file.path(lib, "toast")))
  })

  test_that("init does not install dependencies when infer.dependencies is false", {
    skip_on_cran()
    projRoot <- cloneTestProject("sated")
    init(enter = FALSE, projRoot, options = list(local.repos = "packages"),
         infer.dependencies=FALSE)
    lib <- libDir(projRoot)
    expect_true(file.exists(lockFilePath(projRoot)))
    expect_true(file.exists(srcDir(projRoot)))
    expect_true(file.exists(libDir(projRoot)))
    expect_false(file.exists(file.path(lib, "breakfast")))
    expect_false(file.exists(file.path(lib, "bread")))
    expect_false(file.exists(file.path(lib, "oatmeal")))
    expect_true(file.exists(file.path(lib, "packrat")))
    expect_false(file.exists(file.path(lib, "toast")))
  })

  test_that("restore ignores dirty packages", {
    skip_on_cran()
    projRoot <- cloneTestProject("carbs")
    lib <- libDir(projRoot)
    init(projRoot, options = list(local.repos = "packages"), enter = FALSE)
    expect_true(file.exists(file.path(lib, "bread")))

    installTestPkg("oatmeal", "1.0.0", lib)
    expect_true(file.exists(file.path(lib, "oatmeal")))
    restore(projRoot, prompt = FALSE, restart = FALSE)
    expect_true(file.exists(file.path(lib, "oatmeal")))
  })

  test_that("restore installs missing packages", {
    skip_on_cran()
    projRoot <- cloneTestProject("carbs")
    lib <- libDir(projRoot)
    init(enter = FALSE, projRoot, options = list(local.repos = "packages"))
    expect_true(file.exists(file.path(lib, "bread")))

    # Remove a used package and restore
    remove.packages("bread", lib = lib)
    expect_false(file.exists(file.path(lib, "bread")))
    restore(projRoot, prompt = FALSE, restart = FALSE)
    expect_true(file.exists(file.path(lib, "bread")))
  })

  test_that("snapshot captures new dependencies", {
    skip_on_cran()
    projRoot <- cloneTestProject("healthy")
    lib <- libDir(projRoot)
    init(enter = FALSE, projRoot, options = list(local.repos = "packages"))

    # Simulate the addition of a dependency
    expect_false(file.exists(file.path(lib, "bread")))
    expect_false(file.exists(file.path(lib, "toast")))
    installTestPkg("bread", "1.0.0", lib)
    addTestDependency(projRoot, "toast")  # toast depends on bread
    expect_true(file.exists(file.path(lib, "bread")))

    # Snapshot the new state and make sure we picked up both toast and its
    # dependency, bread
    pkgs <- pkgNames(lockInfo(projRoot))
    expect_false("bread" %in% pkgs)
    expect_false("toast" %in% pkgs)
    snapshot(projRoot)
    pkgs <- pkgNames(lockInfo(projRoot))
    expect_true("bread" %in% pkgs)
    expect_true("toast" %in% pkgs)
  })

  test_that("snapshot captures new installed dependecies but not
            inferred dependencies when infer.dependencies is FALSE", {
              skip_on_cran()
              projRoot <- cloneTestProject("healthy")
              lib <- libDir(projRoot)
              init(enter = FALSE, projRoot, options = list(local.repos = "packages"))

              # Simulate the addition of a dependency
              expect_false(file.exists(file.path(lib, "bread")))
              expect_false(file.exists(file.path(lib, "toast")))
              installTestPkg("bread", "1.0.0", lib)
              addTestDependency(projRoot, "toast")  # toast depends on bread
              expect_true(file.exists(file.path(lib, "bread")))

              # Snapshot the new state and make sure we picked up both toast and its
              # dependency, bread
              pkgs <- pkgNames(lockInfo(projRoot))
              expect_false("bread" %in% pkgs)
              expect_false("toast" %in% pkgs)
              snapshot(projRoot, infer.dependencies = FALSE)
              pkgs <- pkgNames(lockInfo(projRoot))
              expect_true("bread" %in% pkgs)
              expect_false("toast" %in% pkgs)
})

  test_that("dependencies in library directories are ignored", {
    skip_on_cran()
    makeLibrariesProject()
    projRoot <- cloneTestProject("libraries")
    lib <- libDir(projRoot)
    init(enter = FALSE, projRoot, options = list(local.repos = "packages"))

    # This test project has a file called library.R that depends on bread, and
    # three .R files inside library/, library.old/, and library.new/ that
    # depend on oatmeal.
    expect_true(file.exists(file.path(lib, "bread")))
    expect_false(file.exists(file.path(lib, "oatmeal")))
  })

  test_that("dependencies in \"ignored.directories\" are ignored", {
    skip_on_cran()
    projRoot <- cloneTestProject("partlyignored")
    lib <- libDir(projRoot)
    init(enter = FALSE, projRoot, options = list(ignored.directories="ignoreme"))

    # This test project has a file called notignored.R that depends on bread, and
    # another file called ignoreme/ignorethis.R that depends on toast.
    expect_true(file.exists(file.path(lib, "bread")))
    expect_false(file.exists(file.path(lib, "toast")))
  })

  test_that("clean removes libraries and sources", {
    skip_on_cran()
    projRoot <- cloneTestProject("smallbreakfast")
    lib <- libDir(projRoot)
    src <- srcDir(projRoot)
    init(enter = FALSE, projRoot, options = list(local.repos = "packages"))

    expect_true(file.exists(file.path(lib, "bread")))
    expect_true(file.exists(file.path(lib, "oatmeal")))
    expect_true(file.exists(file.path(src, "bread")))
    expect_true(file.exists(file.path(src, "oatmeal")))

    # Remove the dependency on oatmeal and clean
    removeTestDependencyFile(projRoot, "oatmeal.R")
    clean("oatmeal", project = projRoot)

    # bread should still be present, but we should have removed the orphaned
    # package oatmeal
    expect_true(file.exists(file.path(lib, "bread")))
    expect_false(file.exists(file.path(lib, "oatmeal")))
    expect_true(file.exists(file.path(src, "bread")))
    expect_false(file.exists(file.path(src, "oatmeal")))
  })

  test_that("init works with multiple repos", {
    skip_on_cran()
    repos <- getOption("repos")
    pkgType <- getOption("pkgType")
    on.exit({
      options("repos" = repos)
      options("pkgType" = pkgType)
    }, add = TRUE)
    options(repos = c(CRAN = getOption("repos"), custom = getOption("repos")))

    projRoot <- cloneTestProject("empty")
    init(enter = FALSE, projRoot, options = list(local.repos = "packages"))
  })

  test_that("fileDependencies.R picks up '::', ':::' dependencies", {
    file <- tempfile()
    cat("library('baz')\nlibrary('bat')\nstringr::foo(1)\nKmisc::enumerate(2)\nfunction() {{plyr::bar(plyr::baz(1, 2))}}\n", file = file)
    on.exit(unlink(file))
    deps <- fileDependencies.R(file)
    expect_identical(
      intersect(deps, c("baz", "bat", "stringr", "Kmisc", "plyr")),
      union(deps, c("baz", "bat", "stringr", "Kmisc", "plyr"))
    )
  })

  test_that("init, disable handle projects that have been initted / disabled sensibly", {
    skip_on_cran()
    skip_on_os("windows")

    projRoot <- cloneTestProject("sated")
    packrat::init(enter = FALSE, projRoot, options = list(local.repos = "packages"))
    list.files(projRoot, all.files = TRUE, recursive = TRUE)
    expect_true(file.exists(file.path(projRoot, ".Rprofile")))
    packrat::disable(projRoot, restart = FALSE)
    expect_false(file.exists(file.path(projRoot, ".Rprofile")))

    unlink(projRoot, recursive = TRUE)

    projRoot <- cloneTestProject("sated")
    text <- "## Some comments\n## That should be preserved\n"
    cat(text, file = file.path(projRoot, ".Rprofile"))
    packrat::init(enter = FALSE, projRoot, options = list(local.repos = "packages"))
    list.files(projRoot, all.files = TRUE, recursive = TRUE)
    expect_true(file.exists(file.path(projRoot, ".Rprofile")))
    content <- readLines(file.path(projRoot, ".Rprofile"))
    expect_true(grepl(text, paste(content, collapse = "\n")))
    packrat::disable(projRoot, restart = FALSE)
    path <- file.path(projRoot, ".Rprofile")
    expect_true(file.exists(path))
    if (file.exists(path)) {
      content <- readChar(path, file.info(path)$size, TRUE)
      expect_true(grepl(text, paste(content, collapse = "\n")))
    }

    unlink(projRoot, recursive = TRUE)

    ## Empty .Rprofile
    projRoot <- cloneTestProject("sated")
    file.create(file.path(projRoot, ".Rprofile"))
    packrat::init(enter = FALSE, projRoot, options = list(local.repos = "packages"))
    expect_true(file.exists(file.path(projRoot, ".Rprofile")))
    content <- readLines(file.path(projRoot, ".Rprofile"))
    packrat::disable(projRoot, restart = FALSE)
    expect_false(file.exists(file.path(projRoot, ".Rprofile")))
  })

  test_that("status does not fail", {
    skip_on_cran()
    projRoot <- cloneTestProject("sated")
    init(enter = FALSE, projRoot, options = list(local.repos = "packages"))

    status(projRoot)
    unlink(file.path(projRoot, "packrat/lib/x86_64-apple-darwin13.3.0/3.2.0/bread"), recursive = TRUE)
    status(projRoot)
    unlink(file.path(projRoot, "packrat/lib/x86_64-apple-darwin13.3.0/3.2.0/breakfast"), recursive = TRUE)
    status(projRoot)

    # Try removing an item from the lockfile
    lf <- readLines(lockFilePath(projRoot))
    blanks <- which(lf == "")
    breakfastStart <- grep("Package:\\s*breakfast", lf)
    breakfastEnd <- sort(blanks[blanks > breakfastStart])[1]
    lf <- lf[-c(breakfastStart:breakfastEnd)]
    cat(lf, file = lockFilePath(projRoot), sep = "\n")
    status(projRoot)

  })

  test_that("hash does not fail if LinkingTo packages are not available", {
    skip_on_cran()
    expect_warning(hash("packages/egg/DESCRIPTION"))
  })

  test_that("snapshot succeeds with an empty DESCRIPTION", {
    skip_on_cran()
    projRoot <- cloneTestProject("emptydesc")
    .snapshotImpl(projRoot, implicit.packrat.dependency = FALSE,
                  snapshot.sources = FALSE)
  })
})
