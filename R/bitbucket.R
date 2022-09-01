# It needs to stop with an error if it does not succeed. The call site will
# return the error message and print the URL. The "stop" here is a fallback to
# catch errors that may occur lower down the stack that don't properly create an
# error message, just cause a... lack of success.
bitbucketDownload <- function(url, destfile, ...) {
  success <- bitbucketDownloadImpl(url, destfile, ...)
  if (!success) {
    stop("Download failure.")
  }
}

# This will either return a boolean success value or raise an error. A non-TRUE
# success value will be turned into an error in the outer function.

# This doesn't make sense, but why? It doesn't make sense because these all
# return TRUE. There's no inner tryCatch to generate an error message for them.

# This should either return TRUE or stop with an error. The outer function is the same.

# TODO But now we don't need the outer downloader.
bitbucketDownloadImpl <- function(url, destfile, ...) {
  tryCatch({
    if (bitbucketAuthenticated()) {
      if (canUseRenvDownload()) {
        renvDownload(url, destfile, type = "bitbucket")
      } else if (canUseHttr()) {
        bitbucketDownloadHttr(url, destfile)
      }
    } else {
      downloadWithRetries(url, destfile = destfile)
    }
  }, error = function(e) {
    stop(sprintf("Error in downloader:\n%s", e))
  })

  return(TRUE)
}

bitbucketDownloadHttr <- function(url, destfile, ...) {
  authenticate    <- yoink("httr", "authenticate")
  GET             <- yoink("httr", "GET")
  content         <- yoink("httr", "content")

  user <- bitbucket_user(quiet = TRUE)
  pwd <- bitbucket_pwd(quiet = TRUE)
  auth <- if (!is.null(user) & !is.null(pwd)) {
    authenticate(user, pwd, type = "basic")
  } else {
    list()
  }

  result <- GET(url, auth)
  if (result$status != 200) {
    stop(
      sprintf(
        "Unable to download package from Bitbucket; check the BITBUCKET_USERNAME and BITBUCKET_PASSWORD environment variables: %s",
        httr::http_status(result)$message))
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

