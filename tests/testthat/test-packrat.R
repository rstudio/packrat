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

test_that("bootstrap creates project structure", {
  projRoot <- cloneTestProject("sated")
  bootstrap(projRoot, sourcePackagePaths = file.path("packages", "packrat"))
  expect_true(file.exists(file.path(projRoot, "packrat.lock")))
  expect_true(file.exists(file.path(projRoot, "packrat.sources")))
  expect_true(file.exists(file.path(projRoot, "library")))
  expect_true(file.exists(file.path(projRoot, ".Rprofile")))
  expect_true(file.exists(file.path(projRoot, ".Renviron")))
})

test_that("restore removes unused packages", {
  projRoot <- cloneTestProject("carbs")
  lib <- libdir(projRoot)
  bootstrap(projRoot, sourcePackagePaths = file.path("packages", "packrat"))
  expect_true(file.exists(file.path(lib, "bread")))  
  
  # Install an unused package and restore
  installTestPkg("oatmeal", "1.0.0", lib)
  expect_true(file.exists(file.path(lib, "oatmeal")))
  restore(projRoot, prompt = FALSE)
  expect_false(file.exists(file.path(lib, "oatmeal")))
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

