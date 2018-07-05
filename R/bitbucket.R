isBitbucketURL <- function(url) {
  is.string(url) && grepl("^http(?:s)?://(?:www|api).bitbucket.(org|com)", url, perl = TRUE)
}

canUseBitbucketDownloader <- function() {
  all(packageVersionInstalled(devtools = "1.9.1", httr = "1.0.0"))
}

bitbucketDownload <- function(url, destfile, ...) {
  onError(1, {
    bitbucket_user  <- bitbucket_user
    bitbucket_pwd   <- bitbucket_pwd
    authenticate    <- yoink("httr", "authenticate")
    GET             <- yoink("httr", "GET")
    content         <- yoink("httr", "content")

    user <- bitbucket_user(quiet=TRUE)
    pwd <- bitbucket_pwd(quiet=TRUE)
    auth <- if (!is.null(user) & !is.null(pwd)) {
      authenticate(user, pwd, type="basic")
    } else {
      list()
    }

    request <- GET(url, auth)
    if(request$status == 401) {
      warning("Failed to download package from Bitbucket: not authorized. ",
              "Did you set BITBUCKET_USER and BITBUCKET_PWD env vars?",
              call. = FALSE)
      return(1)
    }
    writeBin(content(request, "raw"), destfile)
    if (file.exists(destfile)) 0 else 1
  })
}


#' Retrieve Bitbucket user.
#'
#' A bitbucket user
#' Looks in env var \code{BITBUCKET_USER}
#'
#' @keywords internal
#'
bitbucket_user <- function(quiet = FALSE) {
  user <- Sys.getenv("BITBUCKET_USER")
  if (nzchar(user)) {
    if (!quiet) {
      message("Using Bibtucket user from envvar BITBUCKET_USER")
    }
    return(user)
  }
  return(NULL)
}


#' Retrieve Bitbucket password
#'
#' A bitbucket password
#' Looks in env var \code{BITBUCKET_PWD}
#'
#' @keywords internal
#'
bitbucket_pwd <- function(quiet = FALSE) {
  pwd <- Sys.getenv("BITBUCKET_PWD")
  if (nzchar(pwd)) {
    if (!quiet) {
      message("Using Bibtucket password from envvar BITBUCKET_PWD")
    }
    return(pwd)
  }
  return(NULL)
}

