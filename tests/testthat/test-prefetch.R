test_that("prefetchTargets uses the current file or the archive locations", {
  srcDir <- withr::local_tempdir()
  withr::local_envvar(R_PACKRAT_SRC_DIR = srcDir)
  withr::local_options(packrat.alternative.archive.layouts = FALSE)
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

  repos <- c(CRAN = paste0(repo, "/"), other = "https://example.com/other")
  targets <- prefetchTargets(
    prefetchCandidates(records, actions, repos, NULL),
    repos,
    NULL,
    available
  )

  expect_equal(
    targets$name,
    c("current", "older", "older", "missing", "missing")
  )
  expect_equal(
    targets$url,
    c(
      "https://example.com/cran/src/contrib/current_1.0.tar.gz",
      "https://example.com/cran/src/contrib/Archive/older/older_1.5.tar.gz",
      "https://example.com/other/src/contrib/Archive/older/older_1.5.tar.gz",
      "https://example.com/cran/src/contrib/Archive/missing/missing_0.1.tar.gz",
      "https://example.com/other/src/contrib/Archive/missing/missing_0.1.tar.gz"
    )
  )
  expect_equal(
    targets$destfile,
    file.path(
      srcDir,
      c("current", "older", "older", "missing", "missing"),
      c(
        "current_1.0.tar.gz",
        "older_1.5.tar.gz",
        "older_1.5.tar.gz",
        "missing_0.1.tar.gz",
        "missing_0.1.tar.gz"
      )
    )
  )
})

test_that("archiveUrls tries every repository with each archive layout", {
  repos <- c(
    CRAN = "https://example.com/cran/",
    other = "https://example.com/other"
  )
  record <- pkgRecord("pkg", "1.0")

  expect_equal(
    archiveUrls(repos, record),
    c(
      "https://example.com/cran/src/contrib/Archive/pkg/pkg_1.0.tar.gz",
      "https://example.com/other/src/contrib/Archive/pkg/pkg_1.0.tar.gz",
      "https://example.com/cran/src/contrib/Archive/pkg/1.0/pkg_1.0.tar.gz",
      "https://example.com/other/src/contrib/Archive/pkg/1.0/pkg_1.0.tar.gz",
      "https://example.com/cran/src/contrib/pkg_1.0.tar.gz",
      "https://example.com/other/src/contrib/pkg_1.0.tar.gz"
    )
  )

  withr::local_options(packrat.alternative.archive.layouts = FALSE)
  expect_equal(
    archiveUrls(repos, record),
    c(
      "https://example.com/cran/src/contrib/Archive/pkg/pkg_1.0.tar.gz",
      "https://example.com/other/src/contrib/Archive/pkg/pkg_1.0.tar.gz"
    )
  )
})

test_that("prefetchWithRetries tries each package's next URL until one works", {
  attempts <- list()
  local_mocked_bindings(
    prefetchDownload = function(targets, concurrency) {
      attempts[[length(attempts) + 1L]] <<- targets$url
      failed <- targets[!grepl("good", targets$url), , drop = FALSE]
      failed$http_code <- rep("404", nrow(failed))
      failed
    }
  )

  targets <- data.frame(
    name = c("first", "second", "second", "never", "never"),
    version = "1.0",
    url = c(
      "good/first",
      "bad/second",
      "good/second",
      "bad/never",
      "worse/never"
    ),
    destfile = "unused",
    stringsAsFactors = FALSE
  )

  failed <- prefetchWithRetries(targets, concurrency = 2L)

  expect_equal(
    attempts,
    list(
      c("good/first", "bad/second", "bad/never"),
      c("good/second", "worse/never")
    )
  )
  expect_equal(failed$name, "never")
  expect_equal(failed$url, "worse/never")
  expect_equal(failed$http_code, "404")
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

  candidates <- prefetchCandidates(
    records,
    actions,
    c(CRAN = "https://example.com/cran"),
    NULL
  )

  expect_equal(vapply(candidates, `[[`, character(1), "name"), "wanted")
})

test_that("prefetchDownload moves successful downloads into place and reports failures", {
  skip_if_no_parallel_curl()
  skip_on_cran()

  repo <- withr::local_tempdir()
  writeLines("one", file.path(repo, "one_1.0.tar.gz"))
  writeLines("two", file.path(repo, "two_1.0.tar.gz"))
  srcDir <- withr::local_tempdir()
  withr::local_options(
    download.file.method = "curl",
    download.file.extra = NULL
  )

  repoURL <- fileURL(repo)
  targets <- data.frame(
    name = c("one", "two", "gone"),
    version = "1.0",
    url = paste0(repoURL, "/", c("one", "two", "gone"), "_1.0.tar.gz"),
    destfile = file.path(
      srcDir,
      c("one", "two", "gone"),
      c("one_1.0.tar.gz", "two_1.0.tar.gz", "gone_1.0.tar.gz")
    ),
    stringsAsFactors = FALSE
  )

  failed <- prefetchDownload(targets, concurrency = 2L)

  expect_equal(failed$name, "gone")
  expect_equal(readLines(targets$destfile[1]), "one")
  expect_equal(readLines(targets$destfile[2]), "two")
  expect_false(file.exists(targets$destfile[3]))
  expect_length(
    list.files(srcDir, pattern = "\\.prefetch$", recursive = TRUE),
    0
  )
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
    prefetchDownload = function(targets, concurrency) {
      stop("should not download")
    }
  )
  withr::local_envvar(R_PACKRAT_SRC_DIR = withr::local_tempdir())

  expect_silent(
    prefetchPackageSources(
      list(pkgRecord("pkg", "1.0")),
      c(pkg = "add"),
      c(CRAN = "https://example.com/cran"),
      NULL
    )
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
      prefetchPackageSources(
        list(pkgRecord("pkg", "1.0")),
        c(pkg = "add"),
        c(CRAN = "https://example.com/cran"),
        NULL
      )
    ),
    "network is down"
  )
})

test_that("prefetchPackageSources doesn't contact the repository when everything is cached", {
  local_mocked_bindings(
    hasBinaryRepositories = function() FALSE,
    cachedPackagePath = function(project, pkgRecord) "/cache/pkg",
    availablePackagesSource = function(repos) {
      stop("should not fetch the package index")
    }
  )
  withr::local_envvar(R_PACKRAT_SRC_DIR = withr::local_tempdir())

  expect_silent(
    prefetchPackageSources(
      list(pkgRecord("pkg", "1.0")),
      c(pkg = "add"),
      c(CRAN = "https://example.com/cran"),
      NULL
    )
  )
})

test_that("prefetchPackageSources can be turned off", {
  local_mocked_bindings(
    availablePackagesSource = function(repos) stop("should not run")
  )
  withr::local_options(packrat.prefetch.sources = FALSE)

  expect_silent(
    prefetchPackageSources(
      list(pkgRecord("pkg", "1.0")),
      c(pkg = "add"),
      c(CRAN = "https://example.com/cran"),
      NULL
    )
  )
})

test_that("prefetchUserAgentArgs sends R's user agent unless one is set", {
  withr::local_options(HTTPUserAgent = "R (4.5.0 x86_64-pc-linux-gnu)")

  expect_equal(
    prefetchUserAgentArgs(""),
    paste("", "-A", shQuote("R (4.5.0 x86_64-pc-linux-gnu)"))
  )
  expect_equal(prefetchUserAgentArgs("-A custom"), "-A custom")
  expect_equal(
    prefetchUserAgentArgs("--user-agent=custom"),
    "--user-agent=custom"
  )
  expect_equal(
    prefetchUserAgentArgs("-H 'User-Agent: custom'"),
    "-H 'User-Agent: custom'"
  )

  withr::local_options(HTTPUserAgent = NULL)
  expect_equal(prefetchUserAgentArgs("-L"), "-L")
})

test_that("prefetchPackageSources skips packages installed from a binary repository", {
  local_mocked_bindings(
    hasBinaryRepositories = function() TRUE,
    binaryRepositoriesEnabled = function() TRUE,
    availablePackagesBinary = function(repos) {
      cbind(
        Package = "binary",
        availableMatrix(c(binary = "1.0"), "https://example.com/cran/bin")
      )
    },
    availablePackagesSource = function(repos) {
      availableMatrix(
        c(binary = "1.0", source = "1.0"),
        "https://example.com/cran/src/contrib"
      )
    },
    cachedPackagePath = function(project, pkgRecord) NULL,
    inferAppropriateDownloadMethod = function(url) "curl",
    curlVersion = function() numeric_version("8.5.0"),
    prefetchDownload = function(targets, concurrency) {
      fetched <<- targets$name
      targets[0, , drop = FALSE]
    }
  )
  withr::local_envvar(R_PACKRAT_SRC_DIR = withr::local_tempdir())
  fetched <- NULL

  suppressMessages(
    prefetchPackageSources(
      list(pkgRecord("binary", "1.0"), pkgRecord("source", "1.0")),
      c(binary = "add", source = "add"),
      c(CRAN = "https://example.com/cran"),
      NULL
    )
  )

  expect_equal(fetched, "source")
})

test_that("installPkg uses a prefetched source without downloading it again", {
  skip_if_no_parallel_curl()
  skip_on_cran()

  repo <- withr::local_tempdir()
  writeLines("pkg", file.path(repo, "pkg_1.0.tar.gz"))
  srcDir <- withr::local_tempdir()
  withr::local_envvar(R_PACKRAT_SRC_DIR = srcDir)
  withr::local_options(download.file.extra = NULL)

  installed <- NULL
  local_mocked_bindings(
    hasBinaryRepositories = function() FALSE,
    availablePackagesSource = function(repos) {
      availableMatrix(c(pkg = "1.0"), fileURL(repo))
    },
    cachedPackagePath = function(project, pkgRecord) NULL,
    inferAppropriateDownloadMethod = function(url) "curl",
    restoreWithCopyFromCache = function(...) FALSE,
    restoreWithCopyFromUntrustedCache = function(...) FALSE,
    getSourceForPkgRecord = function(...) stop("should not download"),
    archivePackageType = function(...) "source",
    install_local_path = function(path, ...) installed <<- path,
    annotatePkgDesc = function(...) NULL,
    isUsingCache = function(project) FALSE
  )

  record <- pkgRecord("pkg", "1.0")
  repos <- c(CRAN = "https://example.com/cran")
  suppressMessages(
    prefetchPackageSources(list(record), c(pkg = "add"), repos, NULL)
  )
  prefetched <- file.path(srcDir, "pkg", "pkg_1.0.tar.gz")
  expect_equal(readLines(prefetched), "pkg")

  installPkg(record, NULL, repos, lib = withr::local_tempdir())

  expect_equal(installed, prefetched)
})

test_that("prefetchPackageSources ends the announcement line before downloading", {
  local_mocked_bindings(
    hasBinaryRepositories = function() FALSE,
    availablePackagesSource = function(repos) {
      availableMatrix(
        c(a = "1.0", b = "1.0"),
        "https://example.com/cran/src/contrib"
      )
    },
    cachedPackagePath = function(project, pkgRecord) NULL,
    inferAppropriateDownloadMethod = function(url) "curl",
    curlVersion = function() numeric_version("8.5.0"),
    prefetchDownload = function(targets, concurrency) {
      message("<download>")
      targets[0, , drop = FALSE]
    }
  )
  withr::local_envvar(R_PACKRAT_SRC_DIR = withr::local_tempdir())
  withr::local_options(packrat.prefetch.concurrency = 8)

  messages <- capture_messages(
    prefetchPackageSources(
      list(pkgRecord("a", "1.0"), pkgRecord("b", "1.0")),
      c(a = "add", b = "add"),
      c(CRAN = "https://example.com/cran"),
      NULL
    )
  )

  expect_equal(
    messages[1:2],
    c("Prefetching sources for 2 packages (8 at a time) ...\n", "<download>\n")
  )
})
