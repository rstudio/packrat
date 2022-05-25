isBitbucketURL <- function(url) {
  is.string(url) && grepl("^http(?:s)?://(?:www|api).bitbucket.(org|com)", url, perl = TRUE)
}

canUseBitbucketDownloader <- function() {
  (all(packageVersionInstalled(httr = "1.0.0")) &&
     !is.null(bitbucket_user(quiet = TRUE)) &&
     !is.null(bitbucket_pwd(quiet = TRUE)))
}

bitbucketDownload <- function(url, destfile, ...) {
  tryCatch(
    bitbucketDownloadImpl(url, destfile, ...),
    error = function(e) {
      stop(sprintf("Bitbucket request failed: %s", e), call. = FALSE)
    })
}

bitbucketDownloadImpl <- function(url, destfile, ...) {
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


#' Retrieve Bitbucket user.
#'
#' A bitbucket user
#' Looks in env var \code{BITBUCKET_USERNAME}
#'
#' @keywords internal
#'
bitbucket_user <- function(quiet = FALSE) {
  user <- Sys.getenv("BITBUCKET_USERNAME")
  if (nzchar(user)) {
    if (!quiet) {
      message("Using Bitbucket username from envvar BITBUCKET_USERNAME")
    }
    return(user)
  }
  return(NULL)
}


#' Retrieve Bitbucket password
#'
#' A bitbucket password
#' Looks in env var \code{BITBUCKET_PASSWORD}
#'
#' @keywords internal
#'
bitbucket_pwd <- function(quiet = FALSE) {
  pwd <- Sys.getenv("BITBUCKET_PASSWORD")
  if (nzchar(pwd)) {
    if (!quiet) {
      message("Using Bitbucket password from envvar BITBUCKET_PASSWORD")
    }
    return(pwd)
  }
  return(NULL)
}

