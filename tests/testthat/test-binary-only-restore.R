newRestoreTestProject <- function(packages = "bread", sources = "source") {
  project <- tempfile("packrat-binary-only-")
  packratDir <- file.path(project, "packrat")
  dir.create(file.path(packratDir, "src"), recursive = TRUE)
  dir.create(packrat:::libDir(project), recursive = TRUE)
  set_opts(use.cache = FALSE, project = project, persist = FALSE)

  if (length(sources) == 1L) {
    sources <- rep(sources, length(packages))
  }
  names(sources) <- packages

  cranPackages <- packages[sources == "CRAN"]
  if (length(cranPackages)) {
    repo <- tempfile("packrat-source-repo-")
    contrib <- file.path(repo, "src", "contrib")
    dir.create(contrib, recursive = TRUE)
    for (package in cranPackages) {
      withr::with_dir(
        dirname(test_path("packages", package)),
        utils::tar(
          file.path(contrib, paste0(package, "_1.0.0.tar.gz")),
          files = package,
          compression = "gzip",
          tar = packrat:::tar_binary()
        )
      )
    }
    tools::write_PACKAGES(contrib, type = "source")
    options(repos = c(CRAN = fileURL(repo)))
  }

  records <- lapply(packages, function(package) {
    description <- test_path("packages", package, "DESCRIPTION")
    desc <- as.data.frame(read.dcf(description), stringsAsFactors = FALSE)
    record <- list(
      name = package,
      source = sources[[package]],
      version = desc$Version,
      hash = packrat:::hash(description),
      depends = list()
    )
    if (identical(record$source, "source")) {
      record$source_path <- file.path("packages", package)
      sourceArchive <- file.path(
        packratDir,
        "src",
        package,
        paste0(package, "_", record$version, ".tar.gz")
      )
      dir.create(dirname(sourceArchive), recursive = TRUE)
      withr::with_dir(
        dirname(test_path("packages", package)),
        utils::tar(
          sourceArchive,
          files = package,
          compression = "gzip",
          tar = packrat:::tar_binary()
        )
      )
    }
    record
  })
  names(records) <- packages
  packrat:::writeLockFile(
    packrat:::lockFilePath(project),
    records
  )
  project
}

setLockfilePackageSource <- function(project, package, source) {
  path <- packrat:::lockFilePath(project)
  lines <- readLines(path)
  start <- grep(paste0("^Package: ", package, "$"), lines)
  stopifnot(length(start) == 1L)

  nextBlank <- which(seq_along(lines) > start & lines == "")[1]
  end <- if (is.na(nextBlank)) length(lines) else nextBlank - 1L
  field <- grep("^Source:", lines[seq.int(start, end)])
  if (length(field)) {
    fieldLine <- start + field[1] - 1L
    if (is.null(source)) {
      lines <- lines[-fieldLine]
    } else {
      lines[fieldLine] <- paste("Source:", source)
    }
  } else if (!is.null(source)) {
    lines <- append(lines, paste("Source:", source), after = start)
  }
  writeLines(lines, path)
}

makeBinaryPackageArchive <- function(project, package, version) {
  library <- withr::local_tempdir()
  packrat:::install_local_path(
    test_path("packages", package),
    reload = FALSE,
    args = paste("-l", library),
    dependencies = FALSE,
    quick = TRUE,
    quiet = TRUE
  )

  archive <- file.path(
    packrat:::srcDir(project),
    package,
    paste0(package, "_", version, ".tar.gz")
  )
  dir.create(dirname(archive), recursive = TRUE, showWarnings = FALSE)

  oldwd <- setwd(library)
  on.exit(setwd(oldwd), add = TRUE)
  utils::tar(
    archive,
    files = package,
    compression = "gzip",
    tar = packrat:::tar_binary()
  )
  archive
}

test_that("restore refuses every local source package before installing any", {
  skip_on_cran()
  scopeTestContext()

  project <- newRestoreTestProject(
    packages = c("bread", "oatmeal")
  )
  lib <- packrat:::libDir(project)
  unlink(file.path(lib, c("bread", "oatmeal")), recursive = TRUE)

  withr::local_options(packrat.allow.source.packages = FALSE)
  err <- tryCatch(
    restore(
      project,
      overwrite.dirty = TRUE,
      prompt = FALSE,
      restart = FALSE
    ),
    error = identity
  )

  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "bread")
  expect_match(conditionMessage(err), "oatmeal")
  expect_false(file.exists(file.path(lib, "bread")))
  expect_false(file.exists(file.path(lib, "oatmeal")))
})

test_that("restore refuses source records before installing any package", {
  skip_on_cran()
  scopeTestContext()

  cases <- list(
    github = "GitHub",
    gitlab = "GitLab",
    bitbucket = "Bitbucket",
    source = "local source",
    unknown = "unknown",
    missing = NULL
  )
  for (source in names(cases)) {
    project <- cloneTestProject("falsy-github")
    lib <- packrat:::libDir(project)
    setLockfilePackageSource(
      project,
      "falsy",
      if (identical(source, "missing")) NULL else source
    )
    withr::local_options(packrat.allow.source.packages = FALSE)

    err <- suppressWarnings(tryCatch(
      restore(
        project,
        overwrite.dirty = TRUE,
        prompt = FALSE,
        restart = FALSE
      ),
      error = identity
    ))

    expect_s3_class(err, "error")
    expect_match(conditionMessage(err), "falsy")
    if (!is.null(cases[[source]])) {
      expect_match(conditionMessage(err), cases[[source]])
    } else {
      expect_match(conditionMessage(err), "does not record a package source")
    }
    expect_false(file.exists(file.path(lib, "falsy")))
  }
})

test_that("restore refuses source-only CRAN archives before installing", {
  skip_on_cran()
  scopeTestContext()

  project <- newRestoreTestProject(source = "CRAN")
  lib <- packrat:::libDir(project)
  unlink(file.path(lib, "bread"), recursive = TRUE)

  withr::local_options(packrat.allow.source.packages = FALSE)
  err <- tryCatch(
    restore(
      project,
      overwrite.dirty = TRUE,
      prompt = FALSE,
      restart = FALSE
    ),
    error = identity
  )

  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "bread")
  expect_match(conditionMessage(err), "no binary package is available")
  expect_false(file.exists(file.path(lib, "bread")))
})

test_that("restore unpacks Linux binary archives when source packages are disabled", {
  skip_on_cran()
  scopeTestContext()

  project <- newRestoreTestProject(source = "CRAN")
  lib <- packrat:::libDir(project)
  archive <- makeBinaryPackageArchive(project, "bread", "1.0.0")
  expect_identical(packrat:::archivePackageType(archive), "binary")
  unlink(file.path(lib, "bread"), recursive = TRUE)

  withr::local_options(packrat.allow.source.packages = FALSE)
  restore(
    project,
    overwrite.dirty = TRUE,
    prompt = FALSE,
    restart = FALSE
  )

  expect_true(file.exists(file.path(lib, "bread", "DESCRIPTION")))
  expect_true(dir.exists(file.path(lib, "bread", "Meta")))
  expect_true("bread" %in% rownames(installed.packages(lib.loc = lib)))
})

test_that("restore installs packages from binary repositories", {
  skip_on_cran()
  scopeTestContext()
  if (!packrat:::hasBinaryRepositories()) {
    skip("binary repositories are not supported on this platform")
  }

  project <- newRestoreTestProject(source = "CRAN")
  lib <- packrat:::libDir(project)
  archive <- makeBinaryPackageArchive(project, "bread", "1.0.0")
  repos <- lockInfo(project, "repos")
  binaryContribUrl <- utils::contrib.url(repos, .Platform$pkgType)
  binaryContrib <- sub("^file:/+", "/", binaryContribUrl)
  dir.create(binaryContrib, recursive = TRUE)
  file.copy(
    archive,
    file.path(binaryContrib, "bread_1.0.0.tgz")
  )
  binaryType <- if (packrat:::is.windows()) "win.binary" else "mac.binary"
  tools::write_PACKAGES(binaryContrib, type = binaryType)

  withr::local_options(
    packrat.allow.source.packages = FALSE,
    pkgType = .Platform$pkgType
  )
  restore(
    project,
    overwrite.dirty = TRUE,
    prompt = FALSE,
    restart = FALSE
  )

  expect_true(file.exists(file.path(lib, "bread", "DESCRIPTION")))
  expect_true("bread" %in% rownames(installed.packages(lib.loc = lib)))
})

test_that("restore accepts source package cache hits when source is disabled", {
  skip_on_cran()
  scopeTestContext()

  project <- newRestoreTestProject()
  lib <- packrat:::libDir(project)

  cacheRoot <- tempfile("packrat-cache-")
  dir.create(cacheRoot)
  withr::local_envvar(R_PACKRAT_CACHE_DIR = cacheRoot)
  set_opts(use.cache = TRUE, project = project, persist = FALSE)

  pkgRecord <- packrat:::searchPackages(lockInfo(project), "bread")[[1]]
  packageLibrary <- withr::local_tempdir()
  packrat:::install_local_path(
    test_path("packages", "bread"),
    reload = FALSE,
    args = paste("-l", packageLibrary),
    dependencies = FALSE,
    quick = TRUE,
    quiet = TRUE
  )
  cacheEntry <- packrat:::cacheLibDir("bread", pkgRecord$hash, "bread")
  dir.create(dirname(cacheEntry), recursive = TRUE)
  packrat:::dir_copy(file.path(packageLibrary, "bread"), cacheEntry)

  withr::local_options(packrat.allow.source.packages = FALSE)
  restore(
    project,
    overwrite.dirty = TRUE,
    prompt = FALSE,
    restart = FALSE
  )

  expect_true(file.exists(file.path(lib, "bread", "DESCRIPTION")))
})

test_that("restore keeps source installs enabled by default and when TRUE", {
  skip_on_cran()
  scopeTestContext()

  for (allowSourcePackages in list(NULL, TRUE)) {
    if (is.null(allowSourcePackages)) {
      withr::local_options(packrat.allow.source.packages = NULL)
      expect_identical(
        getOption("packrat.allow.source.packages", TRUE),
        TRUE
      )
    } else {
      withr::local_options(
        packrat.allow.source.packages = allowSourcePackages
      )
    }

    project <- newRestoreTestProject()
    lib <- packrat:::libDir(project)
    unlink(file.path(lib, "bread"), recursive = TRUE)
    restore(
      project,
      overwrite.dirty = TRUE,
      prompt = FALSE,
      restart = FALSE
    )
    expect_true(file.exists(file.path(lib, "bread")))
  }
})
