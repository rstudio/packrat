test_that("modifyRemoteInfo modifies DESCRIPTION and writes expected output", {
  remote_info <- data.frame(
    RemoteType = "gitlab",
    RemoteHost = "gitlab.com",
    RemoteRepo = "museli",
    RemoteUsername = "breakfaster",
    RemoteRef = "HEAD",
    RemoteSha = "abc123"
  )

  # 1. compress one of the test packages
  # 2. run modifyRemoteInfo on it
  # 3. check that info was modified
})
