# Download package sources for a restore concurrently, before any package is
# installed.
#
# installPkg() downloads each package just before installing it, one curl
# process at a time. When most of the restore is spent in those downloads,
# fetching every needed source up front with a single `curl --parallel` is
# much faster. installPkg() skips the download for any source that is already
# in srcDir(), so the install loop is unchanged; anything the prefetch fails
# to fetch is downloaded there as before.
#
# The prefetch only handles packages from CRAN-like repositories, and only
# when curl is the download method. It never fails the restore.

prefetchInstallActions <- c("add", "upgrade", "downgrade", "crossgrade")

# Minimum curl version: --parallel arrived in 7.66.0 and the %{exitcode}
# write-out variable in 7.75.0.
prefetchMinimumCurlVersion <- "7.75.0"

prefetchEnabled <- function() {
  isTRUE(getOption("packrat.prefetch.sources", TRUE))
}

prefetchConcurrency <- function() {
  concurrency <- as.integer(getOption("packrat.prefetch.concurrency", 8L))
  if (length(concurrency) != 1L || is.na(concurrency) || concurrency < 1L) {
    stop("'packrat.prefetch.concurrency' must be a positive integer")
  }
  concurrency
}

# Returns the curl version as a numeric_version, or NULL if curl can't be run.
curlVersion <- function() {
  output <- tryCatch(
    suppressWarnings(system2(
      "curl",
      "--version",
      stdout = TRUE,
      stderr = FALSE
    )),
    error = function(e) character()
  )
  version <- regmatches(
    output[1],
    regexpr("(?<=^curl )[0-9.]+", output[1], perl = TRUE)
  )
  if (!length(version)) {
    return(NULL)
  }
  numeric_version(version)
}

# Package records that will be installed and need a source download: from a
# CRAN-like repository, not in the cache, and not already in srcDir(). Uses
# only local checks, so a restore served entirely from the cache doesn't
# contact the repository.
prefetchCandidates <- function(pkgRecords, actions, repos, project) {
  installs <- names(actions)[actions %in% prefetchInstallActions]
  if (!length(installs) || !length(repos)) {
    return(list())
  }

  records <- Filter(Negate(is.null), searchPackages(pkgRecords, installs))
  Filter(
    function(pkgRecord) {
      isFromCranlikeRepo(pkgRecord, repos) &&
        !identical(pkgRecord$source, "unknown") &&
        is.null(cachedPackagePath(project, pkgRecord)) &&
        !file.exists(prefetchDestfile(pkgRecord, project))
    },
    records
  )
}

prefetchDestfile <- function(pkgRecord, project) {
  file.path(srcDir(project), pkgRecord$name, pkgSrcFilename(pkgRecord))
}

# Download URL and destination for each candidate. Returns a data frame with
# one row per package: name, version, url, and destfile. Mirrors the URL
# choice in getSourceForPkgRecord(): the repository's current file when the
# version matches available.packages(), otherwise the CRAN archive layout of
# the first repository.
prefetchTargets <- function(records, repos, project, available) {
  rows <- lapply(records, function(pkgRecord) {
    pkgSrcFile <- pkgSrcFilename(pkgRecord)
    current <- pkgRecord$name %in%
      rownames(available) &&
      identical(pkgRecord$version, available[pkgRecord$name, "Version"])
    url <- if (current) {
      paste(available[pkgRecord$name, "Repository"], pkgSrcFile, sep = "/")
    } else {
      paste(
        sub("/+$", "", repos[[1]]),
        "src/contrib/Archive",
        pkgRecord$name,
        pkgSrcFile,
        sep = "/"
      )
    }
    data.frame(
      name = pkgRecord$name,
      version = pkgRecord$version,
      url = url,
      destfile = prefetchDestfile(pkgRecord, project),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Quote a value for a curl config file.
curlConfigQuote <- function(value) {
  paste0("\"", gsub("([\"\\\\])", "\\\\\\1", value), "\"")
}

# Send R's user agent unless 'extra' already sets one. Repositories such as
# Posit Package Manager use it to decide whether to serve binaries, and a
# single-package download (through libcurl, or through curl with a
# download.file.extra that sets it) sends it too.
prefetchUserAgentArgs <- function(extra) {
  userAgent <- getOption("HTTPUserAgent")
  setsUserAgent <- grepl("(^|\\s)(-A|--user-agent)(\\s|=|$)", extra) ||
    grepl("user-agent:", extra, ignore.case = TRUE)
  if (length(userAgent) != 1L || !nzchar(userAgent) || setsUserAgent) {
    return(extra)
  }
  paste(extra, "-A", shQuote(userAgent))
}

# Download every target with one `curl --parallel`. Each file is written to a
# temporary name and moved into place only when its transfer succeeded, so a
# failed or partial download never looks like a usable source. Returns the
# targets that were not downloaded.
prefetchDownload <- function(targets, concurrency) {
  partial <- paste0(targets$destfile, ".prefetch")
  for (dir in unique(dirname(targets$destfile))) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  on.exit(unlink(partial[file.exists(partial)]), add = TRUE)

  config <- tempfile("packrat-prefetch-", fileext = ".txt")
  on.exit(unlink(config), add = TRUE)
  writeLines(
    paste0(
      "url = ",
      curlConfigQuote(targets$url),
      "\noutput = ",
      curlConfigQuote(partial)
    ),
    config
  )

  # The same arguments a single-package download uses, including the user's
  # download.file.extra (user agent, .netrc file, and so on). The trailing
  # --write-out replaces any earlier one so each transfer reports its result.
  extra <- ""
  if (identical(getOption("download.file.method"), "curl")) {
    extra <- paste(getOption("download.file.extra", ""), collapse = " ")
  }
  extra <- prefetchUserAgentArgs(extra)
  writeOut <- "packrat-prefetch %{exitcode} %{http_code} %{filename_effective}\\n"
  command <- paste(
    "curl",
    "--speed-limit 1 --speed-time",
    as.integer(getOption("timeout", 60)),
    curlExtraArgs(extra),
    "--parallel --parallel-max",
    concurrency,
    "-K",
    shQuote(config),
    "-w",
    shQuote(writeOut)
  )
  output <- suppressWarnings(system(command, intern = TRUE))

  results <- regmatches(
    output,
    regexec("^packrat-prefetch ([0-9]+) ([0-9]+) (.*)$", output)
  )
  results <- Filter(length, results)
  succeeded <- vapply(results, function(r) r[2] == "0", logical(1))
  succeededFiles <- vapply(results[succeeded], `[`, character(1), 4)

  ok <- partial %in% succeededFiles & file.exists(partial)
  ok[ok] <- file.rename(partial[ok], targets$destfile[ok])

  failed <- targets[!ok, , drop = FALSE]
  failed$http_code <- vapply(
    partial[!ok],
    function(file) {
      match <- Filter(function(r) identical(r[4], file), results)
      if (length(match)) match[[1]][3] else NA_character_
    },
    character(1),
    USE.NAMES = FALSE
  )
  failed
}

prefetchPackageSources <- function(pkgRecords, actions, repos, project) {
  if (!prefetchEnabled()) {
    return(invisible())
  }

  tryCatch(
    {
      candidates <- prefetchCandidates(pkgRecords, actions, repos, project)
      # installPkg() installs these from a binary repository and doesn't need
      # their sources.
      candidates <- Filter(
        function(pkgRecord) !installsFromBinaryRepository(pkgRecord, repos),
        candidates
      )
      if (!length(candidates)) {
        return(invisible())
      }
      available <- availablePackagesSource(repos = repos)
      targets <- prefetchTargets(candidates, repos, project, available)
      if (!identical(inferAppropriateDownloadMethod(targets$url[1]), "curl")) {
        return(invisible())
      }
      version <- curlVersion()
      if (is.null(version) || version < prefetchMinimumCurlVersion) {
        return(invisible())
      }

      concurrency <- prefetchConcurrency()
      message(
        "Prefetching sources for ",
        nrow(targets),
        " packages (",
        concurrency,
        " at a time) ... ",
        appendLF = FALSE
      )
      start <- Sys.time()
      failed <- prefetchDownload(targets, concurrency)
      elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
      message(sprintf(
        "fetched %d of %d in %.1f seconds",
        nrow(targets) - nrow(failed),
        nrow(targets),
        elapsed
      ))
      for (i in seq_len(nrow(failed))) {
        message(sprintf(
          "\tCould not prefetch %s (%s) from %s (HTTP %s); it will be downloaded during installation",
          failed$name[i],
          failed$version[i],
          failed$url[i],
          failed$http_code[i]
        ))
      }
    },
    error = function(e) {
      warning(
        "Prefetching package sources failed; packages will be downloaded during installation: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  invisible()
}
