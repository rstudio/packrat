isGitlabURL <- function(url) {
  is.string(url) && grepl("^http(?:s)?://(?:www|api).gitlab.(org|com)", url, perl = TRUE)
}

canUseGitlabDownloader <- function() {
  all(packageVersionInstalled(devtools = "1.9.1", httr = "1.0.0"))
}

gitlabDownload <- function(url, destfile, ...) {
  onError(1,{
    gitlab_user    <- gitlab_user
    gitlab_pwd     <- gitlab_pwd
    authenticate   <- yoink("httr", "authenticate")
    GET            <- yoink("httr", "GET")
    content        <- yoink("httr", "content")

    user <- gitlab_user(quiet = TRUE)
    pwd <- gitlab_pwd(quiet = TRUE)
    auth <- if(!is.null(user) & !is.null(pwd)) {
      authenticate(user, pwd, type = "basic")
    } else {
      list()
    }

    request <- GET(url, auth)
    if(request$status == 401) {
      warning("Failed to download package from gitlab: not authorized. ",
              "Did you set GITLAB_USERNAME and GITLAB_PASSWORD env vars?",
              call. = FALSE)
      return(1)
    }
    writeBin(content(request, "raw"), destfile)
    if (file.exists(destfile)) 0 else 1
  })
}

#' Retrieve Gitlab user.
#'
#' A gitlab user
#' Looks in env var \code{GITLAB_USERNAME}
#'
#' @keywords internal
#'
gitlab_user <- function(quiet = FALSE) {
  user <- Sys.getenv("GITLAB_USERNAME")
  if (nzchar(user)) {
    if (!quiet) {
      message("Using Gitlab username from envvar GITLAB_USERNAME")
    }
    return(user)
  }
  return(NULL)
}


#' Retrieve Gitlab password
#'
#' A bitbucket password
#' Looks in env var \code{GITLAB_PASSWORD}
#'
#' @keywords internal
#'
gitlab_pwd <- function(quiet = FALSE) {
  pwd <- Sys.getenv("GITLAB_PASSWORD")
  if (nzchar(pwd)) {
    if (!quiet) {
      message("Using Gitlab password from envvar GITLAB_PASSWORD")
    }
    return(pwd)
  }
  return(NULL)
}
