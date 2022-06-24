isGitlabURL <- function(url) {
  is.string(url) && grepl("^http(?:s)?://(?:www|api).gitlab.(org|com)", url, perl = TRUE)
}

canUseGitlabDownloader <- function() {
  (all(packageVersionInstalled(httr = "1.0.0")) &&
     !is.null(gitlab_user(quiet = TRUE)) &&
     !is.null(gitlab_pwd(quiet = TRUE)))
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
  user <- gitlab_user(quiet = TRUE)
  pwd <- gitlab_pwd(quiet = TRUE)

  auth <- if (!is.null(token)) {
    add_headers("Private-Token" = token)
  } else if (!is.null(user) && !is.null(pwd)) {
    authenticate(user, pwd, type = "basic")
  } else {
    list()
  }

  result <- GET(url, auth)
  if (result$status != 200) {
    stop(
      sprintf(
        "Unable to download package from GitLab; check the GITLAB_USERNAME and GITLAB_PASSWORD environment variables: %s",
        httr::http_status(result)$message), call. = FALSE)
  }
  writeBin(content(result, "raw"), destfile)
  if (!file.exists(destfile)) {
    stop("No data received.", call. = FALSE)
  }
  # Success!
  return(TRUE)
}

#' Retrieve GitLab user.
#'
#' A GitLab user
#' Looks in env var \code{GITLAB_USERNAME}
#'
#' @keywords internal
#'
gitlab_user <- function(quiet = FALSE) {
  user <- Sys.getenv("GITLAB_USERNAME")
  if (nzchar(user)) {
    if (!quiet) {
      message("Using GitLab username from envvar GITLAB_USERNAME")
    }
    return(user)
  }
  return(NULL)
}


#' Retrieve GitLab password
#'
#' A GitLab password
#' Looks in env var \code{GITLAB_PASSWORD}
#'
#' @keywords internal
#'
gitlab_pwd <- function(quiet = FALSE) {
  pwd <- Sys.getenv("GITLAB_PASSWORD")
  if (nzchar(pwd)) {
    if (!quiet) {
      message("Using GitLab password from envvar GITLAB_PASSWORD")
    }
    return(pwd)
  }
  return(NULL)
}


#' Retrieve GitLab PAT.
#'
#' A GitLab PAT
#' Looks in env var \code{GITLAB_PAT}
#'
#' @keywords internal
#'
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
