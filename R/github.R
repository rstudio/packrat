isGitHubURL <- function(url) {
  is.string(url) && grepl("^http(?:s)?://(?:www|api).github.com", url, perl = TRUE)
}

canUseGitHubDownloader <- function() {
  (all(packageVersionInstalled(httr = "1.0.0")) &&
     !is.null(github_pat()))
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

githubDownload <- function(url, destfile, ...) {
  tryCatch(
    githubDownloadImpl(url, destfile, ...),
    error = function(e) {
      stop(sprintf("GitHub request failed: %s", e), call. = FALSE)
    })
}

githubDownloadImpl <- function(url, destfile, ...) {
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
    stop(
      sprintf(
        "Unable to download package from GitHub; check the GITHUB_PAT environment variable: %s",
        httr::http_status(result)$message), call. = FALSE)
  }
  writeBin(content(result, "raw"), destfile)
  if (!file.exists(destfile)) {
    stop("No data received.", call. = FALSE)
  }
  # Success!
  return(TRUE)
}
