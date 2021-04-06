context("Local Repositories")

withTestContext({

  test_that("init fails when package not found in any repo", {
    repos <- getOption("repos")
    on.exit(options(repos = repos))

    emptyRepo <- file.path(tempdir(),"repo-empty")
    on.exit(unlink(emptyRepo, recursive = TRUE))

    contribDir <- file.path(emptyRepo,"src","contrib")
    dir.create(contribDir, recursive = TRUE)
    file.create(file.path(contribDir, "PACKAGES"))
    projRoot <- cloneTestProject("sated")
    options(repos = c(CRAN = paste0("file:///", emptyRepo)))
    ## we expect a warning signalling that the package 'breakfast' is not found
    ## in a repo or locally
    expect_error(suppressWarnings(init(enter = FALSE, projRoot)))
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
})
