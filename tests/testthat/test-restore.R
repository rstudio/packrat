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
    in_dir(
      dirname(basedir),
      suppressWarnings(tar(
        tarfile = src_tmp,
        files = basename(basedir),
        compression = "gzip",
        tar = tar_binary()
      ))
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
  if (
    length(dir(untarred_tmp)) == 1 &&
      dir.exists(file.path(untarred_tmp, dir(untarred_tmp)))
  ) {
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

test_that("appendRemoteInfoToDescription uses RemoteSubdir", {
  parent_dir <- withr::local_tempdir()
  dest_dir <- withr::local_tempdir()
  # create package dir with subdirectory
  src_dir <- file.path(parent_dir, "bread", "toast")
  dir.create(src_dir, recursive = TRUE)

  writeLines(
    readLines(test_path("packages/toast/DESCRIPTION")),
    file.path(src_dir, "DESCRIPTION")
  )

  parent_tarball <- paste0(parent_dir, ".tar.gz")
  dest_tarball <- paste0(dest_dir, ".tar.gz")

  in_dir(
    parent_dir,
    tar(
      tarfile = parent_tarball,
      files = "bread",
      compression = "gzip",
      tar = tar_binary()
    )
  )

  remote_info <- data.frame(
    RemoteType = "github",
    RemoteHost = "github.com",
    RemoteRepo = "bread",
    RemoteUsername = "breakfaster",
    RemoteRef = "HEAD",
    RemoteSha = "abc123",
    RemoteSubdir = "toast",
    stringsAsFactors = FALSE
  )

  appendRemoteInfoToDescription(
    src = parent_tarball,
    dest = dest_tarball,
    remote_info = remote_info
  )

  untar(dest_tarball, exdir = dest_dir, tar = tar_binary())
  desc <- readLines(file.path(dest_dir, "toast", "DESCRIPTION"))
  expect_true("RemoteSubdir: toast" %in% desc)
})

test_that("isFromCranlikeRepo returns TRUE for CRAN source", {
  pkgRecord <- list(
    name = "ggplot2",
    source = "CRAN",
    version = "3.4.0"
  )

  repos <- c(CRAN = "https://cran.r-project.org")

  expect_true(isFromCranlikeRepo(pkgRecord, repos))
})

test_that("isFromCranlikeRepo returns TRUE for CustomCRANLikeRepository class", {
  pkgRecord <- structure(
    list(
      name = "ggplot2",
      source = "CRAN",
      version = "3.4.0"
    ),
    class = c("packageRecord", "CustomCRANLikeRepository")
  )

  repos <- c(CRAN = "https://cran.r-project.org")

  expect_true(isFromCranlikeRepo(pkgRecord, repos))
})

test_that("isFromCranlikeRepo returns FALSE for source package", {
  pkgRecord <- list(
    name = "mypackage",
    source = "source",
    version = "1.0.0"
  )

  repos <- c(CRAN = "https://cran.r-project.org")

  expect_false(isFromCranlikeRepo(pkgRecord, repos))
})

test_that("isFromCranlikeRepo returns TRUE for BioConductor source", {
  pkgRecord <- list(
    name = "GenomicRanges",
    source = "Bioconductor",
    version = "1.50.0"
  )

  repos <- c(BioCsoft = "https://bioconductor.org/packages/3.16/bioc")

  expect_true(isFromCranlikeRepo(pkgRecord, repos))
})

test_that("isFromCranlikeRepo returns TRUE for custom repository source", {
  pkgRecord <- list(
    name = "mypackage",
    source = "MyRepo",
    version = "1.0.0"
  )

  repos <- c(MyRepo = "https://example.com/repo")

  expect_true(isFromCranlikeRepo(pkgRecord, repos))
})

test_that("isFromCranlikeRepo returns TRUE for a CRAN-like source named Github", {
  pkgRecord <- list(
    name = "mypackage",
    source = "GitHub",
    version = "1.0.0"
  )

  repos <- c(GitHub = "https://example.com/repo")

  expect_true(isFromCranlikeRepo(pkgRecord, repos))
})