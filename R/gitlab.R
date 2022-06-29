isGitlabURL <- function(url) {
  is.string(url) && grepl("^http(?:s)?://(?:www|api).gitlab.(org|com)", url, perl = TRUE)
}

canUseGitlabDownloader <- function() {
  (all(packageVersionInstalled(httr = "1.0.0")) &&
     !is.null(gitlab_pat(quiet = TRUE)))
}

gitlabDownload <- function(url, destfile, ...) {
  tryCatch(
    gitlabDownloadImpl(url, destfile, ...),
    error = function(e) {
      stop(sprintf("GitLab request failed: %s", e), call. = FALSE)
    })
}

gitlabDownloadImpl <- function(url, destfile, ...) {
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

gitlab_pat <- function(quiet = FALSE) {
  token <- Sys.getenv("GITLAB_PAT")
  if (nzchar(token)) {
    if (!quiet) {
      message("Using GitLab PAT from envvar GITLAB_PAT")
    }
    return(token)
  }
  return(NULL)
}
