gitlabDownload <- function(url, destfile, ...) {
  if (gitlabAuthenticated()) {
    if (canUseRenvDownload()) {
      renvDownload(url, destfile, type = "gitlab")
    } else if (canUseHttr()) {
      gitlabDownloadHttr(url, destfile)
    }
  } else {
    # Success handling needs to happen here.
    downloadWithRetries(url, destfile = destfile)
  }
}

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
    stop(
      sprintf(
        "Unable to download package from GitLab; check the GITLAB_PAT environment variable: %s",
        httr::http_status(result)$message), call. = FALSE)
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

  fmt <- "%s/api/v4/projects/%s%%2F%s/repository/archive?sha=%s"
  archiveUrl <- sprintf(fmt,
                        pkgRecord$remote_host,
                        pkgRecord$remote_username,
                        pkgRecord$remote_repo,
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

