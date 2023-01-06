# - Equivalent to other git provider download functions.
# - Called by `getSourceForPkgRecord` (which manages the lifecycle of
#   `destfile`). Responsible for dispatching different download implementations
#   depending on environment and configuration, passing them `url` and
#   `destfile`.
# - Returns nothing if successful, and does not check the return values of inner
#   download methods (`renvDownload`, `providerDownloadHttr`, and
#   `downloadWithRetries`). Those functions are responsible for detecting errors
#   and calling `stop` when they occur.
# - For authenticated download methods (`renvDownload`, `providerDownloadHttr`),
#   catches errors append a note advising the user to check
#   configuration-related environment variables. This happens no matter what the
#   cause of the error.
bitbucketDownload <- function(url, destfile, ...) {
  if (bitbucketAuthenticated() && canUseRenvDownload()) {
    tryCatch(renvDownload(url, destfile, type = "bitbucket"), error = authDownloadAdvice("bitbucket", TRUE, "renv"))
  } else if (bitbucketAuthenticated() && canUseHttr()) {
    tryCatch(bitbucketDownloadHttr(url, destfile), error = authDownloadAdvice("bitbucket", TRUE, "httr"))
  } else {
    tryCatch(downloadWithRetries(url, destfile = destfile), error = authDownloadAdvice("bitbucket", FALSE, "internal"))
  }
}

# - The original function for authenticated downloads. Requires `httr` to be
#   installed. Called by this git provider's top-level download function if
#   `renvDownload`'s requirements are not met, but this function's are.
# - Returns `TRUE` if it succeeds. Calls `stop()` if any errors are encountered.
# - Writes to `destfile`, whose lifecycle is managed by `getSourceForPkgRecord`.
bitbucketDownloadHttr <- function(url, destfile, ...) {
  authenticate    <- yoink("httr", "authenticate")
  GET             <- yoink("httr", "GET")
  content         <- yoink("httr", "content")

  user <- bitbucket_user(quiet = TRUE)
  pwd <- bitbucket_pwd(quiet = TRUE)
  auth <- if (!is.null(user) && !is.null(pwd)) {
    authenticate(user, pwd, type = "basic")
  } else {
    list()
  }

  result <- GET(url, auth)
  if (result$status != 200) {
    stop(httr::http_status(result)$message)
  }
  writeBin(content(result, "raw"), destfile)
  if (!file.exists(destfile)) {
    stop("No data received.")
  }
  # Success!
  return(TRUE)
}

bitbucketArchiveUrl <- function(pkgRecord) {
  # API URLs get recorded when packages are downloaded with devtools /
  # remotes, but Packrat just wants to use 'plain' URLs when downloading
  # package sources.
  remoteHost <- sub("api.bitbucket.org/2.0",
                                "bitbucket.org",
                                pkgRecord$remote_host,
                                fixed = TRUE)

  fmt <- "%s/%s/%s/get/%s.tar.gz"
  archiveUrl <- sprintf(fmt,
                        remoteHost,
                        pkgRecord$remote_username,
                        pkgRecord$remote_repo,
                        pkgRecord$remote_sha)

  # Ensure the protocol is prepended. We prefer using https if possible. Note
  # that 'wininet' can fail if attempting to download from an 'http' URL that
  # redirects to an 'https' URL. https://github.com/rstudio/packrat/issues/269
  method <- tryCatch(
    secureDownloadMethod(),
    error = function(e) "internal"
  )
  protocol <- if (identical(method, "internal")) "http" else "https"
  if (!grepl("^http", archiveUrl)) {
    archiveUrl <- paste(protocol, archiveUrl, sep = "://")
  }
  return(archiveUrl)
}

isBitbucketURL <- function(url) {
  is.string(url) && grepl("^http(?:s)?://(?:www|api).bitbucket.(org|com)", url, perl = TRUE)
}

bitbucketAuthenticated <- function() {
  !is.null(bitbucket_user(quiet = TRUE)) &&
  !is.null(bitbucket_pwd(quiet = TRUE))
}

bitbucket_user <- function(quiet = TRUE) {
  user <- Sys.getenv("BITBUCKET_USERNAME")
  if (nzchar(user)) {
    if (!quiet) {
      message("Using Bitbucket username from envvar BITBUCKET_USERNAME")
    }
    return(user)
  }
  return(NULL)
}

bitbucket_pwd <- function(quiet = TRUE) {
  pwd <- Sys.getenv("BITBUCKET_PASSWORD")
  if (nzchar(pwd)) {
    if (!quiet) {
      message("Using Bitbucket password from envvar BITBUCKET_PASSWORD")
    }
    return(pwd)
  }
  return(NULL)
}
