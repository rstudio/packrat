test_that("modifyRemoteInfo modifies DESCRIPTION and writes expected output", {
  remote_info <- data.frame(
    RemoteType = "gitlab",
    RemoteHost = "gitlab.com",
    RemoteRepo = "bread",
    RemoteUsername = "breakfaster",
    RemoteRef = "HEAD",
    RemoteSha = "abc123",
    stringsAsFactors = FALSE
  )

  # 1. compress one of the test packages
  # 2. run modifyRemoteInfo on it
  # 3. check that info was modified

  src_tmp <- tempfile(fileext = ".tar.tz")
  on.exit(
    if (file.exists(src_tmp)) unlink(src_tmp, recursive = TRUE),
    add = TRUE
  )
  dest_tmp <- tempfile(fileext = ".tar.gz")
  on.exit(
    if (file.exists(dest_tmp)) unlink(dest_tmp, recursive = TRUE),
    add = TRUE
  )

  basedir <- "packages/toast"

  in_dir(dirname(basedir),
        suppressWarnings(tar(tarfile = src_tmp, files = basename(basedir),
                             compression = "gzip", tar = tar_binary()))
  )

  success <- modifyRemoteInfo(
    src = src_tmp,
    dest = dest_tmp,
    remote_info = remote_info
  )

  expect_true(success)

  untarred_tmp <- tempdir()
  on.exit(
    if (file.exists(untarred_tmp)) unlink(untarred_tmp, recursive = TRUE),
    add = TRUE
  )

  untar(dest_tmp, exdir = untarred_tmp, tar = tar_binary())
  desc <- readLines(file.path(untarred_tmp, "toast", "DESCRIPTION"))

  expected_desc_tail <- c(
    "RemoteType: gitlab",
    "RemoteHost: gitlab.com",
    "RemoteRepo: bread",
    "RemoteUsername: breakfaster",
    "RemoteRef: HEAD",
    "RemoteSha: abc123"
  )
  expect_identical(tail(desc, 6), expected_desc_tail)
})
