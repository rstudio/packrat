isGitlabURL <- function(url) {
  is.string(url) && grepl("^http(?:s)?://(?:www|api).gitlab.(org|com)", url, perl = TRUE)
}

canUseGitlabDownloader <- function() {
  all(packageVersionInstalled(devtools = "1.9.1", httr = "1.0.0"))
}

gitlabDownload <- function(url, destfile, ...) {
  onError(1, gitlabDownloadImpl(url, destfile, ...))
}

gitlabDownloadImpl <- function(url, destfile, ...) {
  authenticate   <- yoink("httr", "authenticate")
  GET            <- yoink("httr", "GET")
  content        <- yoink("httr", "content")

  user <- gitlab_user(quiet = TRUE)
  pwd <- gitlab_pwd(quiet = TRUE)
  auth <- if (!is.null(user) && !is.null(pwd)) {
    authenticate(user, pwd, type = "basic")
  } else {
    list()
  }

  request <- GET(url, auth)
  if (request$status == 401) {
    warning("Failed to download package from GitLab: not authorized. ",
            "Did you set GITLAB_USERNAME and GITLAB_PASSWORD env vars?",
            call. = FALSE)
    return(1)
  }

  if (request$status == 200) writeBin(content(request, "raw"), destfile)
  if (file.exists(destfile)) 0 else 1
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
