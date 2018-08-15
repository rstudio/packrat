context("git")

test_that("isGitProject identifies projects as sub-directories of git-managed folders as git projects", {
    skip_on_cran()

    dir <- file.path(tempdir(), "test-packrat-git")
    subdir <- file.path(dir, "subdir")
    dir.create(subdir, recursive = TRUE)
    dir.create(file.path(dir, ".git"))
    expect_true(isGitProject(dir))
    expect_true(isGitProject(subdir))
    expect_false(isGitProject(tempdir()))
    unlink(dir, recursive = TRUE)

})
