context("Local Repositories")

withTestContext({

  test_that("init fails when package not found in any repo", {
    projRoot <- cloneTestProject("sated")
    repos <- getOption("repos")
    options(repos = c(CRAN = paste0("file:///", normalizePath("repo-empty"))))
    ## we expect a warning signalling that the package 'breakfast' is not found
    ## in a repo or locally
    expect_error(suppressWarnings(init(enter = FALSE, projRoot)))
    options(repos = repos)
  })

  test_that("install_local fails if no repository has been defined", {
    expect_error(install_local("foo"))
  })

  test_that("packrat::get_opts can read / write atrocious paths", {

    path <- list.files(pattern = "^Ugly")
    with_dir(tempdir(), {
      opts$local.repos(path)
      readPath <- opts$local.repos()
      expect_identical(path, readPath)
    })
  })

  test_that("packrat::restore can load an old version", {
    projRoot <- cloneTestProject('breakfastcereal')
    repos <- getOption('repos')
    options(repos = c(testrepo = paste0("file:///", normalizePath("repo"))))

    ## Should be no warnings
    expect_warning(packrat::restore(projRoot, restart=FALSE), regexp=NA)

    options(repos = repos)
  })

  test_that("packrat can load packages without a repository label", {
    projRoot <- cloneTestProject('breakfastyogurt')
    repos <- getOption('repos')
    options(repos = c(testrepo = paste0("file:///", normalizePath("repo"))))

    ## Should be no warnings
    expect_warning(packrat::init(projRoot, enter=FALSE), regexp=NA)

    options(repos=repos)
  })

})
