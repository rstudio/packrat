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
download <- function(url, method = inferAppropriateDownloadMethod(url), ...) {

  # If this is a path to a GitHub URL, attempt to download with authentication,
  # so that private GitHub repositories can be handled.
  if (isGitHubURL(url) && canUseGitHubDownloader()) {
    result <- try(githubDownload(url, ...), silent = TRUE)
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
  if (identical(getOption("download.file.method"), method))
    extra <- getOption("download.file.extra", default = "")

  extra <- if (is.character(extra))
    paste(extra, collapse = " ")
  else
    ""

  # If we're using 'curl', we need to set '-L' to follow
  # redirects, and '-f' to ensure HTTP error codes are treated
  # as errors.
  if (method == "curl") {
    if (!grepl("\\b-L\\b", extra))
      extra <- paste(extra, "-L")

    if (!grepl("\\b-f\\b", extra))
      extra <- paste(extra, "-f")

    # Also, redirect stderr to stdout, just for nicer
    # printing in RStudio.
    if (!grepl("\\b--stderr -\\b", extra))
      extra <- paste(extra, "--stderr -")
  }

  # Catch warnings in the call.
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

# libcurl is broken in older versions of R as it does not
# respect HTTP error codes (and instead just downloads the 404
# page and returns a zero status code)
canUseLibCurlDownloadMethod <- function() {

  if (!getRversion() >= "3.3.0")
    return(FALSE)

  svnRev <- R.version$`svn rev`
  if (!svnRev >= 69197)
    return(FALSE)

  "libcurl" %in% names(capabilities()) && capabilities("libcurl")
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
