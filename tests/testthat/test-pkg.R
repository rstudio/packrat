test_that("inferPackageRecord preserves fields: GitHub", {
  # GitHub with no subdir.
  df <- as.data.frame(readDcf(test_path("resources/descriptions/github")))
  expect_snapshot(inferPackageRecord(df))
})

test_that("inferPackageRecord preserves fields: GitHub, pkg in subdir", {
  # GitHub with subdir.
  df <- as.data.frame(readDcf(test_path("resources/descriptions/github_subdir")))
  expect_snapshot(inferPackageRecord(df))
})

test_that("inferPackageRecord preserves fields: GitLab", {
  # GitLab with no subdir.
  df <- as.data.frame(readDcf(test_path("resources/descriptions/gitlab")))
  expect_snapshot(inferPackageRecord(df))
})

test_that("inferPackageRecord preserves fields: GitLab, pkg in subdir", {
  # GitLab with subdir.
  df <- as.data.frame(readDcf(test_path("resources/descriptions/gitlab_subdir")))
  expect_snapshot(inferPackageRecord(df))
})
