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

# A file:// URL for a local directory, on any platform.
fileURL <- function(path) {
  path <- normalizePath(path, winslash = "/")
  paste0(if (startsWith(path, "/")) "file://" else "file:///", path)
}
