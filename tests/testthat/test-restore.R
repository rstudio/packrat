remote_info <- data.frame(
  RemoteType = "gitlab",
  RemoteHost = "gitlab.com",
  RemoteRepo = "bread",
  RemoteUsername = "breakfaster",
  RemoteRef = "HEAD",
  RemoteSha = "abc123",
  stringsAsFactors = FALSE
)

test_that("appendRemoteInfoToDescription modifies DESCRIPTION file", {
  src_tmp <- tempfile(fileext = ".tar.gz")
  on.exit(
    if (file.exists(src_tmp)) unlink(src_tmp, recursive = TRUE),
    add = TRUE
  )
  dest_tmp <- tempfile(fileext = ".tar.gz")
  on.exit(
    if (file.exists(dest_tmp)) unlink(dest_tmp, recursive = TRUE),
    add = TRUE
  )

  basedir <- test_path("packages/toast")

  tryCatch(
    in_dir(dirname(basedir),
           suppressWarnings(tar(tarfile = src_tmp, files = basename(basedir),
                                compression = "gzip", tar = tar_binary()))
    ),
    error = function(e) {
      unlink(src_tmp)
      stop(e)
    }
  )

  success <- appendRemoteInfoToDescription(
    src = src_tmp,
    dest = dest_tmp,
    remote_info = remote_info
  )

  expect_true(success)

  untarred_tmp <- tempfile()
  on.exit(
    if (file.exists(untarred_tmp)) unlink(untarred_tmp, recursive = TRUE),
    add = TRUE
  )

  untar(dest_tmp, exdir = untarred_tmp, tar = tar_binary())
    if (length(dir(untarred_tmp)) == 1 &&
      dir.exists(file.path(untarred_tmp, dir(untarred_tmp)))) {
    basedir <- file.path(untarred_tmp, dir(untarred_tmp))
  } else {
    basedir <- untarred_tmp
  }
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
  getwd()
})

test_that("annotatePkgDesc annotates a package description", {
  project <- tempfile()
  dir.create(project)
  lib <- libDir(project)
  package <- file.path(lib, "fake")
  dir.create(package, recursive = TRUE)
  desc <- file.path(package, "DESCRIPTION")
  write_dcf(
    list(
      Package = "fake",
      Version = "1.2.3",
      InstallAgent = "testthat"
    ),
    desc
  )
  pkgRecord <- list(
    name = "fake",
    source = "CRAN",
    version = "1.2.3"
  )

  annotatePkgDesc(pkgRecord, project)
  result <- as.data.frame(readDcf(desc), stringsAsFactors = FALSE)
  expect_equal(result$Package, "fake")
  expect_equal(result$Version, "1.2.3")
  expect_equal(result$InstallAgent, paste('packrat', packageVersion('packrat')))
  expect_equal(result$InstallSource, "CRAN")
})
