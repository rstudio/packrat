# Download a file, using http, https, or ftp
#
# This is a wrapper for \code{\link{download.file}} and takes all the same
# arguments. The only difference is that, if the protocol is https, it changes
# some settings to make it work. How exactly the settings are changed
# differs among platforms.
#
# This function also should follow http redirects on all platforms, which is
# something that does not happen by default when \code{curl} is used, as on
# Mac OS X.
#
# With Windows, it calls \code{setInternet2}, which tells R to use the
# \code{internet2.dll}. Then it downloads the file by calling
# \code{\link{download.file}} using the \code{"internal"} method.
#
# On other platforms, it will try to use \code{wget}, then \code{curl}, and
# then \code{lynx} to download the file. Typically, Linux platforms will have
# \code{wget} installed, and Mac OS X will have \code{curl}.
#
# Note that for many (perhaps most) types of files, you will want to use
# \code{mode="wb"} so that the file is downloaded in binary mode.
#
# @param url The URL to download.
# @param ... Other arguments that are passed to \code{\link{download.file}}.
#
# @seealso \code{\link{download.file}} for more information on the arguments
#   that can be used with this function.
#
# @examples
# \dontrun{
# # Download the downloader source, in binary mode
# download("https://github.com/wch/downloader/zipball/master",
#          "downloader.zip", mode = "wb")
# }
#
download <- function(url,
                     destfile,
                     method = inferAppropriateDownloadMethod(url),
                     ...)
{
  # download to temporary file, then attempt to move to required location
  tempfile <- tempfile(tmpdir = dirname(destfile))
  on.exit(unlink(tempfile))

  # call downloadImpl -- returns '0' on success
  status <- downloadImpl(url, destfile = tempfile, method = method, ...)
  if (status) return(status)

  # attempt to rename the downloaded file, and return 0 on success
  success <- file.rename(tempfile, destfile)
  if (success) 0 else 1
}

downloadImpl <- function(url, method, ...) {

  # If this is a path to a GitHub URL, attempt to download with authentication,
  # so that private GitHub repositories can be handled.
  if (isGitHubURL(url) && canUseGitHubDownloader()) {
    result <- try(githubDownload(url, ...), silent = TRUE)
    if (!inherits(result, "try-error"))
      return(result)
  }

  # If this is a path to a Bitbucket URL, attempt to download.
  if (isBitbucketURL(url) && canUseBitbucketDownloader()) {
    result <- try(bitbucketDownload(url, ...), silent = TRUE)
    if (!inherits(result, "try-error"))
      return(result)
  }

  # When on Windows using an 'internal' method, we need to call
  # 'setInternet2' to set some appropriate state.
  if (is.windows() && method == "internal") {

    # If we directly use setInternet2, R CMD CHECK gives a Note on Mac/Linux
    seti2 <- `::`(utils, 'setInternet2')

    # Check whether we are already using internet2 for internal
    usingInternet2 <- seti2(NA)

    # If not then temporarily set it
    if (!usingInternet2) {

      # Store initial settings, and restore on exit
      on.exit(suppressWarnings(seti2(usingInternet2)), add = TRUE)

      # Needed for https. Will get warning if setInternet2(FALSE) already run
      # and internet routines are used. But the warnings don't seem to matter.
      suppressWarnings(seti2(TRUE))
    }
  }

  downloadFile(url, method, ...)
}

downloadFile <- function(url,
                         method = inferAppropriateDownloadMethod(url),
                         extra = "",
                         ...)
{
  # If the download method we're using matches the current option for
  # 'download.file.method', then propagate 'extra' options.
  if (!nzchar(extra) && identical(getOption("download.file.method"), method))
    extra <- getOption("download.file.extra", default = "")

  # ensure 'extra' is a string
  extra <- paste(extra, collapse = " ")

  # pass extra arguments for 'curl' downloader
  if (method == "curl") {

    # use '-L' to follow redirects
    if (!grepl("\\b-L\\b", extra))
      extra <- paste(extra, "-L")

    # switch off the curl globbing parser
    if (!grepl("\\b-g\\b", extra))
      extra <- paste(extra, "-g")

    # use '-f' to ensure we fail on server errors
    if (!grepl("\\b-f\\b", extra))
      extra <- paste(extra, "-f")

    # make curl quiet -- avoid polluting console with e.g.
    # curl: (22) The requested URL returned error: 404 Not Found
    if (!grepl("\\b-s\\b", extra))
      extra <- paste(extra, "-s")

    # lower connection timeout
    connect.timeout <- getOption("packrat.connect.timeout")
    if (!is.null(connect.timeout) && !grepl("\\b--connect-timeout\\b", extra))
      extra <- paste(extra, "--connect-timeout", connect.timeout)

    # redirect stderr to stdout, for nicer output in RStudio
    if (!grepl("\\b--stderr -\\b", extra))
      extra <- paste(extra, "--stderr -")
  }

  # catch warnings in the call
  caughtWarning <- NULL
  result <- withCallingHandlers(
    download.file(url = url, method = method, extra = extra, ...),
    warning = function(w) {
      caughtWarning <<- w
      invokeRestart("muffleWarning")
    }
  )

  # If we're using 'wget' or 'curl', upgrade the warning to an error.
  if (method %in% c("curl", "wget") && length(caughtWarning)) {
    msg <- sprintf("Failed to download '%s' ('%s' had status code '%s')",
                   url,
                   method,
                   result)
    stop(msg)
  }

  return(result)

}

# TODO: we cannot use libcurl until R no longer sends HEAD requests when
# attempting to download files
canUseLibCurlDownloadMethod <- function() {
  FALSE
}


# Attempt download.packages multiple times.
#
# Assumes we are downloading a single package.
downloadPackagesWithRetries <- function(name, destdir, repos, type, maxTries = 5L) {
  maxTries <- as.integer(maxTries)
  stopifnot(maxTries > 0L)
  stopifnot(length(name) > 0L)

  fileLoc <- matrix(character(), 0L, 2L)
  for (i in 1:maxTries) {
    fileLoc <- download.packages(name,
                                 destdir = destdir,
                                 repos = repos,
                                 type = type,
                                 quiet = TRUE)
    if (nrow(fileLoc)) {
      break
    }
  }
  fileLoc
}


# Download from a URL with a certain number of retries -- returns TRUE
# if the download succeeded, and FALSE otherwise
downloadWithRetries <- function(url, ..., maxTries = 5L) {
  maxTries <- as.integer(maxTries)
  stopifnot(maxTries > 0L)

  success <- FALSE

  for (i in 1:maxTries) {
    # NOTE: Windows seems to return a warning that the status code was 200
    # even on a successful download...
    tryCatch(
      expr = {
        result <- suppressWarnings(download(url, ...))
        if (result %in% c(0, 200))
          success <- TRUE
        else
          Sys.sleep(1)
      },
      error = function(e) {
        Sys.sleep(1)
      }
    )
    if (success) break
  }
  success
}

inferAppropriateDownloadMethod <- function(url) {

  ## If the user wants to explicitly use their own download method,
  ## they can set 'packrat.download.method' and we'll honor that.
  packrat.download.method <- getOption("packrat.download.method")
  if (is.function(packrat.download.method))
    return(packrat.download.method(url))

  # If the user has already opted into using a certain download method,
  # don't stomp on that.
  download.file.method <- getOption("download.file.method")
  if (!is.null(download.file.method))
    return(download.file.method)

  # Prefer using external programs (they can better handle redirects
  # than R's internal downloader, or so it seems)
  isSecureWebProtocol <- grepl("^(?:ht|f)tps://", url, perl = TRUE)
  if (is.linux() || is.mac() || isSecureWebProtocol)
    return(secureDownloadMethod())


  # Use "wininet" as default for R >= 3.2
  if (is.windows() && getRversion() >= "3.2")
    return("wininet")

  # default
  return("internal")
}

# Attempt to determine a secure download method for the current
# platform/configuration. Returns NULL if no such method can
# be ascertained.
secureDownloadMethod <- function() {

  # Check whether we are running R 3.2 and whether we have libcurl
  isR32 <- getRversion() >= "3.2"

  if (is.windows()) {

    # For windows we prefer binding directly to wininet if we can (since
    # that doesn't rely on the value of setInternet2). If it's R <= 3.1
    # then we can use "internal" for https so long as internet2 is enabled
    # (we don't use libcurl on Windows because it doesn't check certs).
    if (isR32)
        return("wininet")

    # Otherwise, make a call to 'setInternet2' and use the 'internal' method
    # if that call succeeds.
    seti2 <- `::`(utils, 'setInternet2')
    if (suppressWarnings(seti2(NA)))
      return("internal")
  }

  # For Darwin and Linux we use libcurl if we can and then fall back
  # to curl or wget as appropriate. We prefer libcurl because it honors
  # the same proxy configuration that "internal" does so it less likely
  # to break downloads for users behind proxy servers.
  if (canUseLibCurlDownloadMethod())
    return("libcurl")

  # Otherwise, fall back to 'wget' or 'curl' (preferring 'wget')
  candidates <- c("curl", "wget")
  for (candidate in candidates)
    if (isProgramOnPath(candidate))
      return(candidate)

  stop("Failed to discover a secure download method.")
}
