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
              "Did you set BITBUCKET_USERNAME and BITBUCKET_PASSWORD env vars?",
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

