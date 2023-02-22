test_that("remote_info is correctly generated from a GitHub pkgRecord", {
  pkgRecordGithub <- list(
    name = 'adder',
    source = 'github',
    version = '0.9.3.1',
    gh_repo = 'adder',
    gh_username = 'my-username',
    gh_ref = 'HEAD',
    gh_sha1 = 'abc123'
  )
  expected <- data.frame(
    RemoteType = "github",
    GithubRepo = "adder",
    GithubUsername = "my-username",
    GithubRef = "HEAD",
    GithubSHA1 = "abc123",
    stringsAsFactors = FALSE
  )
  expect_identical(getRemoteInfo(pkgRecordGithub), expected)
})

test_that("remote_info is correctly generated from a GitHub pkgRecord with a subdirectory", {
  pkgRecordGithubSubdir <- list(
    name = 'subadder',
    source = 'github',
    version = '0.9.3.1',
    gh_repo = 'sub_adder',
    gh_username = 'my-username',
    gh_ref = 'HEAD',
    gh_sha1 = 'abc123',
    gh_subdir = 'subadder'
  )
  expected <- data.frame(
    RemoteType     = "github",
    GithubRepo     = "sub_adder",
    GithubUsername = "my-username",
    GithubRef      = "HEAD",
    GithubSHA1     = "abc123",
    GithubSubdir   = "subadder",
    stringsAsFactors = FALSE
  )
  expect_identical(getRemoteInfo(pkgRecordGithubSubdir), expected)
})

# GitLab package records will work for Bitbucket too
test_that("remote_info is correctly generated from a GitLab pkgRecord", {
  pkgRecordGitlab <- list(
    name = 'adder',
    source = 'gitlab',
    version = '0.9.3.1',
    remote_host = 'gitlab.com',
    remote_repo = 'adder',
    remote_username = 'my-username',
    remote_ref = 'HEAD',
    remote_sha = 'abc123'
  )
  expected <- data.frame(
    RemoteType = "gitlab",
    RemoteHost = "gitlab.com",
    RemoteRepo = "adder",
    RemoteUsername = "my-username",
    RemoteRef = "HEAD",
    RemoteSha = "abc123",
    stringsAsFactors = FALSE
  )
  expect_identical(getRemoteInfo(pkgRecordGitlab), expected)
})

test_that("remote_info is correctly generated from a GitLab pkgRecord with a subdirectory", {
  pkgRecordGitlabSubdir <- list(
    name = 'subadder',
    source = 'gitlab',
    version = '0.9.3.1',
    remote_host = 'gitlab.com',
    remote_repo = 'sub_adder',
    remote_username = 'my-username',
    remote_ref = 'HEAD',
    remote_sha = 'abc123',
    remote_subdir = 'subadder'
  )
  expected <- data.frame(
    RemoteType = "gitlab",
    RemoteHost = "gitlab.com",
    RemoteRepo = "sub_adder",
    RemoteUsername = "my-username",
    RemoteRef = "HEAD",
    RemoteSha = "abc123",
    RemoteSubdir = "subadder",
    stringsAsFactors = FALSE
  )
  expect_identical(getRemoteInfo(pkgRecordGitlabSubdir), expected)
})
