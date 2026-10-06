pkgRecord <- function(name, version, source = "CRAN", ...) {
  list(name = name, version = version, source = source, hash = "hash", ...)
}

availableMatrix <- function(versions, repository) {
  matrix(
    c(versions, rep(repository, length(versions))),
    ncol = 2,
    dimnames = list(names(versions), c("Version", "Repository"))
  )
}

skip_if_no_parallel_curl <- function() {
  version <- curlVersion()
  if (is.null(version) || version < prefetchMinimumCurlVersion) {
    skip("curl with --parallel and %{exitcode} is not available")
  }
}

test_that("prefetchTargets uses the current file or the CRAN archive", {
  srcDir <- withr::local_tempdir()
  withr::local_envvar(R_PACKRAT_SRC_DIR = srcDir)
  local_mocked_bindings(cachedPackagePath = function(project, pkgRecord) NULL)

  repo <- "https://example.com/cran"
  available <- availableMatrix(
    c(current = "1.0", older = "2.0"),
    paste0(repo, "/src/contrib")
  )
  records <- list(
    pkgRecord("current", "1.0"),
    pkgRecord("older", "1.5"),
    pkgRecord("missing", "0.1")
  )
  actions <- c(current = "add", older = "downgrade", missing = "add")

  repos <- c(CRAN = paste0(repo, "/"))
  targets <- prefetchTargets(prefetchCandidates(records, actions, repos, NULL), repos, NULL, available)

  expect_equal(targets$name, c("current", "older", "missing"))
  expect_equal(
    targets$url,
    c(
      "https://example.com/cran/src/contrib/current_1.0.tar.gz",
      "https://example.com/cran/src/contrib/Archive/older/older_1.5.tar.gz",
      "https://example.com/cran/src/contrib/Archive/missing/missing_0.1.tar.gz"
    )
  )
  expect_equal(
    targets$destfile,
    file.path(srcDir, c("current", "older", "missing"), c("current_1.0.tar.gz", "older_1.5.tar.gz", "missing_0.1.tar.gz"))
  )
})

test_that("prefetchCandidates skips packages that won't be downloaded from a repository", {
  srcDir <- withr::local_tempdir()
  withr::local_envvar(R_PACKRAT_SRC_DIR = srcDir)
  local_mocked_bindings(
    cachedPackagePath = function(project, pkgRecord) {
      if (identical(pkgRecord$name, "cached")) "/cache/cached" else NULL
    }
  )
  dir.create(file.path(srcDir, "present"))
  file.create(file.path(srcDir, "present", "present_1.0.tar.gz"))

  records <- list(
    pkgRecord("wanted", "1.0"),
    pkgRecord("cached", "1.0"),
    pkgRecord("present", "1.0"),
    pkgRecord(
      "fromgit",
      "1.0",
      source = "github",
      remote_host = "api.github.com",
      remote_repo = "fromgit"
    ),
    pkgRecord("local", "1.0", source = "source"),
    pkgRecord("removed", "1.0")
  )
  actions <- c(
    wanted = "add",
    cached = "add",
    present = "add",
    fromgit = "add",
    local = "add",
    removed = "remove"
  )

  candidates <- prefetchCandidates(records, actions, c(CRAN = "https://example.com/cran"), NULL)

  expect_equal(vapply(candidates, `[[`, character(1), "name"), "wanted")
})

test_that("prefetchDownload moves successful downloads into place and reports failures", {
  skip_on_os("windows")
  skip_if_no_parallel_curl()

  repo <- withr::local_tempdir()
  writeLines("one", file.path(repo, "one_1.0.tar.gz"))
  writeLines("two", file.path(repo, "two_1.0.tar.gz"))
  srcDir <- withr::local_tempdir()
  withr::local_options(download.file.method = "curl", download.file.extra = NULL)

  repoURL <- paste0("file://", normalizePath(repo))
  targets <- data.frame(
    name = c("one", "two", "gone"),
    version = "1.0",
    url = paste0(repoURL, "/", c("one", "two", "gone"), "_1.0.tar.gz"),
    destfile = file.path(srcDir, c("one", "two", "gone"), c("one_1.0.tar.gz", "two_1.0.tar.gz", "gone_1.0.tar.gz")),
    stringsAsFactors = FALSE
  )

  failed <- prefetchDownload(targets, concurrency = 2L)

  expect_equal(failed$name, "gone")
  expect_equal(readLines(targets$destfile[1]), "one")
  expect_equal(readLines(targets$destfile[2]), "two")
  expect_false(file.exists(targets$destfile[3]))
  expect_length(list.files(srcDir, pattern = "\\.prefetch$", recursive = TRUE), 0)
})

test_that("prefetchPackageSources does nothing without a capable curl", {
  local_mocked_bindings(
    hasBinaryRepositories = function() FALSE,
    availablePackagesSource = function(repos) {
      availableMatrix(c(pkg = "1.0"), "https://example.com/cran/src/contrib")
    },
    cachedPackagePath = function(project, pkgRecord) NULL,
    inferAppropriateDownloadMethod = function(url) "curl",
    curlVersion = function() numeric_version("7.61.1"),
    prefetchDownload = function(targets, concurrency) stop("should not download")
  )
  withr::local_envvar(R_PACKRAT_SRC_DIR = withr::local_tempdir())

  expect_silent(
    prefetchPackageSources(list(pkgRecord("pkg", "1.0")), c(pkg = "add"), c(CRAN = "https://example.com/cran"), NULL)
  )
})

test_that("prefetchPackageSources turns errors into a warning", {
  local_mocked_bindings(
    hasBinaryRepositories = function() FALSE,
    availablePackagesSource = function(repos) {
      availableMatrix(c(pkg = "1.0"), "https://example.com/cran/src/contrib")
    },
    cachedPackagePath = function(project, pkgRecord) NULL,
    inferAppropriateDownloadMethod = function(url) "curl",
    curlVersion = function() numeric_version("8.5.0"),
    prefetchDownload = function(targets, concurrency) stop("network is down")
  )
  withr::local_envvar(R_PACKRAT_SRC_DIR = withr::local_tempdir())

  expect_warning(
    suppressMessages(
      prefetchPackageSources(list(pkgRecord("pkg", "1.0")), c(pkg = "add"), c(CRAN = "https://example.com/cran"), NULL)
    ),
    "network is down"
  )
})

test_that("prefetchPackageSources doesn't contact the repository when everything is cached", {
  local_mocked_bindings(
    hasBinaryRepositories = function() FALSE,
    cachedPackagePath = function(project, pkgRecord) "/cache/pkg",
    availablePackagesSource = function(repos) stop("should not fetch the package index")
  )
  withr::local_envvar(R_PACKRAT_SRC_DIR = withr::local_tempdir())

  expect_silent(
    prefetchPackageSources(list(pkgRecord("pkg", "1.0")), c(pkg = "add"), c(CRAN = "https://example.com/cran"), NULL)
  )
})

test_that("prefetchPackageSources can be turned off", {
  local_mocked_bindings(
    availablePackagesSource = function(repos) stop("should not run")
  )
  withr::local_options(packrat.prefetch.sources = FALSE)

  expect_silent(
    prefetchPackageSources(list(pkgRecord("pkg", "1.0")), c(pkg = "add"), c(CRAN = "https://example.com/cran"), NULL)
  )
})
