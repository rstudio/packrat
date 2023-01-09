
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
gitlabDownload <- function(url, destfile, ...) {
  if (gitlabAuthenticated() && canUseRenvDownload()) {
    tryCatch(renvDownload(url, destfile, type = "gitlab"), error = authDownloadAdvice("gitlab", TRUE, "renv"))
  } else if (gitlabAuthenticated() && canUseHttr()) {
    tryCatch(gitlabDownloadHttr(url, destfile), error = authDownloadAdvice("gitlab", TRUE, "httr"))
  } else {
    tryCatch(downloadWithRetries(url, destfile = destfile), error = authDownloadAdvice("gitlab", FALSE, "internal"))
  }
}

# - The original function for authenticated downloads. Requires `httr` to be
#   installed. Called by this git provider's top-level download function if
#   `renvDownload`'s requirements are not met, but this function's are.
# - Returns `TRUE` if it succeeds. Calls `stop()` if any errors are encountered.
# - Writes to `destfile`, whose lifecycle is managed by `getSourceForPkgRecord`.
gitlabDownloadHttr <- function(url, destfile, ...) {
  authenticate   <- yoink("httr", "authenticate")
  add_headers    <- yoink("httr", "add_headers")
  GET            <- yoink("httr", "GET")
  content        <- yoink("httr", "content")

  token <- gitlab_pat(quiet = TRUE)

  auth <- if (!is.null(token)) {
    add_headers("Private-Token" = token)
  } else {
    list()
  }

  result <- GET(url, auth)
  if (result$status != 200) {
    stop(httr::http_status(result)$message)
  }
  writeBin(content(result, "raw"), destfile)
  if (!file.exists(destfile)) {
    stop("No data received.", call. = FALSE)
  }
  # Success!
  return(TRUE)
}

gitlabArchiveUrl <- function(pkgRecord) {
  # Determine what protocol we can use, preferring https. Note that 'wininet'
  # can fail if attempting to download from an 'http' URL that redirects to an
  # 'https' URL. https://github.com/rstudio/packrat/issues/269
  method <- tryCatch(
    secureDownloadMethod(),
    error = function(e) "internal"
  )
  protocol <- if (identical(method, "internal")) "http" else "https"

  # If remote_host is empty, set it.
  if (is.null(pkgRecord$remote_host) || !nzchar(pkgRecord$remote_host)) {
    pkgRecord$remote_host <- "gitlab.com"
  }

  fmt <- "%s/api/v4/projects/%s/repository/archive?sha=%s"
  archiveUrl <- sprintf(fmt,
                        pkgRecord$remote_host,
                        URLencode(paste0(pkgRecord$remote_username, "/", pkgRecord$remote_repo), reserved = TRUE),
                        pkgRecord$remote_sha)

  protocol <- if (identical(method, "internal")) "http" else "https"
  if (!grepl("^http", archiveUrl)) {
    archiveUrl <- paste(protocol, archiveUrl, sep = "://")
  }
  return(archiveUrl)
}

isGitlabURL <- function(url) {
  is.string(url) && grepl("^http(?:s)?://(?:www|api).gitlab.(org|com)", url, perl = TRUE)
}

gitlabAuthenticated <- function() {
  !is.null(gitlab_pat(quiet = TRUE))
}

gitlab_pat <- function(quiet = TRUE) {
  token <- Sys.getenv("GITLAB_PAT")
  if (nzchar(token)) {
    if (!quiet) {
      message("Using GitLab PAT from envvar GITLAB_PAT")
    }
    return(token)
  }
  return(NULL)
}
