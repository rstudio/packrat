context("git")

test_that("isGitProject identifies projects as sub-directories of git-managed folders as git projects", {

  git <- Sys.which("git")
  if (git != "") {
    dir <- file.path(tempdir(), "test-packrat-git")
    subdir <- file.path(dir, "subdir")
    dir.create(subdir, recursive = TRUE)
    system2(git, c("init", dir))
    expect_true(isGitProject(dir))
    expect_true(isGitProject(subdir))
    expect_false(isGitProject(tempdir()))
    unlink(dir, recursive = TRUE)
  }

})
