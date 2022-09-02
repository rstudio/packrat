githubDownload <- function(url, destfile, ...) {
  if (githubAuthenticated()) {
    # Because we cannot guarantee consistency of error codes across all
    # combinations of download method and API, we inject a message to check
    # provider credentials.
    tryCatch({
      if (canUseRenvDownload()) {
        renvDownload(url, destfile, type = "github")
      } else if (canUseHttr()) {
        githubDownloadHttr(url, destfile)
      }
    }, error = function(e) {
      e$message <- paste0(e, "Check the GITHUB_PAT environment variables.")
      stop(e)
    })
  } else {
    downloadWithRetries(url, destfile = destfile)
  }
}

githubDownloadHttr <- function(url, destfile, ...) {
  authenticate    <- yoink("httr", "authenticate")
  GET             <- yoink("httr", "GET")
  content         <- yoink("httr", "content")

  token <- github_pat(quiet = TRUE)
  auth <- if (!is.null(token)) {
    authenticate(token, "x-oauth-basic", "basic")
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

githubArchiveUrl <- function(pkgRecord) {
  # Determine what protocol we can use, preferring https. Note that 'wininet'
  # can fail if attempting to download from an 'http' URL that redirects to an
  # 'https' URL. https://github.com/rstudio/packrat/issues/269
  method <- tryCatch(
    secureDownloadMethod(),
    error = function(e) "internal"
  )
  protocol <- if (identical(method, "internal")) "http" else "https"

  if (is.null(pkgRecord$remote_host) || !nzchar(pkgRecord$remote_host)) {
    # Guard against packages installed with older versions of devtools
    # (it's possible the associated package record will not contain a
    # 'remote_host' entry)
    fmt <- "api.github.com/repos/%s/%s/tarball/%s"
    archiveUrl <- sprintf(fmt,
                          pkgRecord$gh_username,
                          pkgRecord$gh_repo,
                          pkgRecord$gh_sha1)
  } else {
    # Prefer using the 'remote_host' entry as it allows for successfully
    # installation of packages available on private GitHub repositories
    # (which will not use api.github.com)
    fmt <- "%s/repos/%s/%s/tarball/%s"
    archiveUrl <- sprintf(fmt,
                          pkgRecord$remote_host,
                          pkgRecord$remote_username,
                          pkgRecord$remote_repo,
                          pkgRecord$remote_sha)

    # Ensure the protocol is prepended
    if (!grepl("^http", archiveUrl)) {
      archiveUrl <- paste(protocol, archiveUrl, sep = "://")
    }
  }
}

isGitHubURL <- function(url) {
  is.string(url) && grepl("^http(?:s)?://(?:www|api).github.com", url, perl = TRUE)
}

githubAuthenticated <- function() {
  !is.null(github_pat(quiet = TRUE))
}

github_pat <- function(quiet = TRUE) {
  pat <- Sys.getenv("GITHUB_PAT")
  if (nzchar(pat)) {
    if (!quiet) {
      message("Using GitHub PAT from envvar GITHUB_PAT")
    }
    return(pat)
  }
  return(NULL)
}
