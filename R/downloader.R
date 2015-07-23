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
download <- function(url, ...) {
  # First, check protocol. If http or https, check platform:
  if (grepl('^https?://', url)) {

    # Check whether we are running R 3.2 and whether we have libcurl
    isR32 <- getRversion() >= "3.2"

    # Windows
    if (.Platform$OS.type == "windows") {

      if (isR32) {
        method <- "wininet"
      } else {

        # If we directly use setInternet2, R CMD CHECK gives a Note on Mac/Linux
        seti2 <- `::`(utils, 'setInternet2')

        # Check whether we are already using internet2 for internal
        internet2_start <- seti2(NA)

        # If not then temporarily set it
        if (!internet2_start) {
          # Store initial settings, and restore on exit
          on.exit(suppressWarnings(seti2(internet2_start)))

          # Needed for https. Will get warning if setInternet2(FALSE) already run
          # and internet routines are used. But the warnings don't seem to matter.
          suppressWarnings(seti2(TRUE))
        }

        method <- "internal"
      }

      # download.file will complain about file size with something like:
      #       Warning message:
      #         In download.file(url, ...) : downloaded length 19457 != reported length 200
      # because apparently it compares the length with the status code returned (?)
      # so we supress that
      suppressWarnings(download.file(url, method = method, ...))

    } else {
      # If non-Windows, check for libcurl/curl/wget/lynx, then call download.file with
      # appropriate method.

      if (isR32 && capabilities("libcurl")) {
        method <- "libcurl"
      } else if (nzchar(Sys.which("wget")[1])) {
        method <- "wget"
      } else if (nzchar(Sys.which("curl")[1])) {
        method <- "curl"

        # curl needs to add a -L option to follow redirects.
        # Save the original options and restore when we exit.
        orig_extra_options <- getOption("download.file.extra")
        on.exit(options(download.file.extra = orig_extra_options))

        options(download.file.extra = paste("-L", orig_extra_options))

      } else if (nzchar(Sys.which("lynx")[1])) {
        method <- "lynx"
      } else {
        stop("no download method found")
      }

      download.file(url, method = method, ...)
    }

  } else {
    download.file(url, ...)
  }
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

# Attempt to determine a secure download method for the current platform/configuration
# (returns NULL if none can be ascertaine, this will yield default behavior by R)
secureDownloadMethod <- function() {

  # Check whether we are running R 3.2 and whether we have libcurl
  isR32 <- getRversion() >= "3.2"
  haveLibcurl <- isR32 && capabilities("libcurl")

  # Utility function to bind to libcurl or a fallback utility (e.g. wget)
  posixMethod <- function(utility) {
    if (haveLibcurl)
      "libcurl"
    else if (nzchar(Sys.which(utility)[1]))
      utility
    else
      NULL
  }

  # Determine the right secure download method per-system
  sysName <- Sys.info()[['sysname']]

  # For windows we prefer binding directly to wininet if we can (since
  # that doesn't rely on the value of setInternet2). If it's R <= 3.1
  # then we can use "internal" for https so long as internet2 is enabled
  # (we don't use libcurl on Windows because it doesn't check certs).
  if (identical(sysName, "Windows")) {
    if (isR32) {
      "wininet"
    }
    else {
      seti2 <- `::`(utils, 'setInternet2')
      if (seti2(NA))
        "internal"
      else
        NULL
    }
  }

  # For Darwin and Linux we use libcurl if we can and then fall back
  # to curl or wget as appropriate. We prefer libcurl because it honors
  # the same proxy configuration that "internal" does so it less likely
  # to break downloads for users behind proxy servers.

  # OS X
  else if (identical(sysName, "Darwin")) {
    posixMethod("curl")
  }

  # Other UNIX
  else {
    method <- posixMethod("wget")
    if (!nzchar(method))
      method <- posixMethod("curl")
    method
  }
}
