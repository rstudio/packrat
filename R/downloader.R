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
download <- function(url, method = inferDownloadMethod(url), ...) {

  ## Do some appropriate setup for Windows platforms, when downloading
  ## from a secure protocol.
  if (isWindowsSystem() && grepl("^(?:ht|f)tps?://", url)) {

      # If we directly use setInternet2, R CMD CHECK gives a Note on Mac/Linux
      seti2 <- `::`(utils, 'setInternet2')

      # Store initial settings, and restore on exit
      internet2_start <- seti2(NA)
      on.exit(suppressWarnings(seti2(internet2_start)))

      # Needed for https. Will get warning if setInternet2(FALSE) already run
      # and internet routines are used. But the warnings don't seem to matter.
      suppressWarnings(seti2(TRUE))
  }

  # Download and suppress printing of warnings (they will still
  # be caught and appropriately handled in 'downloadFile()')
  suppressWarnings(downloadFile(url, method = method, ...))
}

# Get an appropriate download method for this system.
inferDownloadMethod <- function(url) {

  # Vectorization, just in case...
  if (length(url) > 1)
    return(vapply(url, inferDownloadMethod, FUN.VALUE = character(1)))

  isInternetURL <- grepl("^(?:ht|f)tps?", url, perl = TRUE)
  isSecureProtocol <- grepl("^(?:ht|f)tps://", url, perl = TRUE)

  # Prefer 'wininet' on Windows for web requests.
  if (Sys.info()[["sysname"]] == "Windows" && isInternetURL)
    return("wininet")

  # For non-secure protocols, prefer the internal download method.
  if (!isSecureProtocol)
    return("internal")

  # Prefer 'libcurl' for secure protocol downloads on appropriate
  # versions of R.
  svnRev <- R.version$`svn rev`
  if (isSecureProtocol && length(svnRev) && as.numeric(svnRev) >= 69197)
    return("libcurl")

  # Use external programs otherwise.
  candidates <- c("wget", "curl", "lynx")
  for (program in candidates)
    if (hasProgram(program))
      return(program)

  stopf("No appropriate method for downloading from URL '%s' found.", url)
}

# A wrapper around 'utils::download.file()' that properly
# turns non-zero status codes when using 'curl' or 'wget'
# into errors.
downloadFile <- function(url, method = inferDownloadMethod(url), ...) {

  # Extract certain arguments from '...'
  dots <- list(...)
  extra <- if ("extra" %in% names(dots))
    dots$extra
  else
    getOption("download.file.extra")

  # If method is 'auto', default to 'libcurl'.
  if (method == "auto")
    method <- "libcurl"

  # If we're trying to use 'libcurl' on a version of R that
  # doesn't properly forward errors, fall back to a separate method.
  svnRev <- R.version$`svn rev`
  if (length(svnRev) && as.numeric(svnRev) < 69197 && method == "libcurl") {
    fallbackMethod <- inferDownloadMethod(url)
    warning("'libcurl' does not properly handle 404s with this version of R; ",
            "falling back to method '", fallbackMethod, "'")

    method <- method <- fallbackMethod
  }

  # Add 'extra' arguments to ensure 'curl' follows redirects
  # and also fails on HTTP error codes.
  if (method == "curl")
    extra <- "-L -f"

  # Call 'download.file()' and catch warnings; we need to
  # elevate these to errors (as they imply the download failed)
  caughtWarning <- NULL
  result <- withCallingHandlers(
    download.file(url, method = method, extra = extra, ...),
    warning = function(w) {
      caughtWarning <<- w
      invokeRestart("muffleWarning")
    }
  )

  # Note that bogus warnings can be thrown when on Windows, so
  # just warn if we're using an external program ('curl' or 'wget')
  # respectively
  if (method %in% c("curl", "wget")) {
    if (length(caughtWarning)) {
      msg <- sprintf("Failed to download '%s' (%s had exit status %s)",
                     url,
                     method,
                     result)
      stop(msg)
    }
  }

  result
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

