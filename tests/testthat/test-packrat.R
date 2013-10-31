# Packrat tests
# 
# To run these tests, set the working directory to packrat/tests and run 
# test_check("packrat")
# 
# Also run by R CMD CHECK.

library(testthat)

# Set up test context. 
context("Packrat tests")

# For the context of these tests, we need to use a private repo.
repos <- getOption("repos")
pkgType <- getOption("pkgType")
on.exit({ 
  options("repos"= repos) 
  options("pkgType" = pkgType) 
}, add = TRUE)
setupTestRepo()

test_that("bootstrap creates project structure and installs dependencies", {
  projRoot <- cloneTestProject("sated")
  bootstrap(projRoot, sourcePackagePaths = file.path("packages", "packrat"))
  lib <- libdir(projRoot)
  expect_true(file.exists(file.path(projRoot, "packrat.lock")))
  expect_true(file.exists(file.path(projRoot, "packrat.sources")))
  expect_true(file.exists(file.path(projRoot, "library")))
  expect_true(file.exists(file.path(projRoot, ".Rprofile")))
  expect_true(file.exists(file.path(projRoot, ".Renviron")))
  expect_true(file.exists(file.path(lib, "breakfast")))
  expect_true(file.exists(file.path(lib, "bread")))
  expect_true(file.exists(file.path(lib, "oatmeal")))
  expect_true(file.exists(file.path(lib, "packrat")))
  expect_true(file.exists(file.path(lib, "toast")))
})

test_that("restore ignores dirty packages", {
  projRoot <- cloneTestProject("carbs")
  lib <- libdir(projRoot)
  bootstrap(projRoot, sourcePackagePaths = file.path("packages", "packrat"))
  expect_true(file.exists(file.path(lib, "bread")))  
  
  installTestPkg("oatmeal", "1.0.0", lib)
  expect_true(file.exists(file.path(lib, "oatmeal")))
  restore(projRoot, prompt = FALSE)
  expect_true(file.exists(file.path(lib, "oatmeal")))
})

test_that("restore installs missing packages", {
  projRoot <- cloneTestProject("carbs")
  lib <- libdir(projRoot)
  bootstrap(projRoot, sourcePackagePaths = file.path("packages", "packrat"))
  expect_true(file.exists(file.path(lib, "bread")))  
  
  # Remove a used package and restore
  remove.packages("bread", lib = lib)
  expect_false(file.exists(file.path(lib, "bread")))
  restore(projRoot, prompt = FALSE)
  expect_true(file.exists(file.path(lib, "bread")))
})

test_that("snapshot captures new dependencies", {
  projRoot <- cloneTestProject("healthy")
  lib <- libdir(projRoot)
  bootstrap(projRoot, sourcePackagePaths = file.path("packages", "packrat"))
  
  # Simulate the addition of a dependency
  expect_false(file.exists(file.path(lib, "bread")))
  expect_false(file.exists(file.path(lib, "toast")))
  installTestPkg("bread", "1.0.0", lib)
  installTestPkg("toast", "1.0.0", lib)
  addTestDependency(projRoot, "toast")  # toast depends on bread
  expect_true(file.exists(file.path(lib, "toast")))  
  expect_true(file.exists(file.path(lib, "bread")))
  
  # Snapshot the new state and make sure we picked up both toast and its 
  # dependency, bread
  pkgs <- pkgNames(lockInfo(projRoot))
  expect_false("bread" %in% pkgs)
  expect_false("toast" %in% pkgs)
  snapshot(projRoot, sourcePackagePaths = file.path("packages", "packrat"))
  pkgs <- pkgNames(lockInfo(projRoot))
  expect_true("bread" %in% pkgs)  
  expect_true("toast" %in% pkgs)  
})


test_that("dependencies in library directories are ignored", {
  projRoot <- cloneTestProject("libraries")
  lib <- libdir(projRoot)
  bootstrap(projRoot, sourcePackagePaths = file.path("packages", "packrat"))
  
  # This test project has a file called library.R that depends on bread, and 
  # three .R files inside library/, library.old/, and library.new/ that 
  # depend on oatmeal. 
  expect_true(file.exists(file.path(lib, "bread")))
  expect_false(file.exists(file.path(lib, "oatmeal")))
})

test_that("clean removes libraries and sources", {
  projRoot <- cloneTestProject("smallbreakfast")
  lib <- libdir(projRoot)
  bootstrap(projRoot, sourcePackagePaths = file.path("packages", "packrat"))
  
  expect_true(file.exists(file.path(lib, "bread")))
  expect_true(file.exists(file.path(lib, "oatmeal")))
  expect_true(file.exists(file.path(projRoot, "packrat.sources", "bread")))
  expect_true(file.exists(file.path(projRoot, "packrat.sources", "oatmeal")))
  
  # Remove the dependency on oatmeal and clean 
  removeTestDependencyFile(projRoot, "oatmeal.R")
  clean(projRoot, prompt = FALSE)

  # bread should still be present, but we should have removed the orphaned
  # package oatmeal
  expect_true(file.exists(file.path(lib, "bread")))
  expect_false(file.exists(file.path(lib, "oatmeal")))  
  expect_true(file.exists(file.path(projRoot, "packrat.sources", "bread")))
  expect_false(file.exists(file.path(projRoot, "packrat.sources", "oatmeal")))
})

