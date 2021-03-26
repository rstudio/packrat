isGitHubURL <- function(url) {
  is.string(url) && grepl("^http(?:s)?://(?:www|api).github.com", url, perl = TRUE)
}

canUseGitHubDownloader <- function() {
  all(packageVersionInstalled(httr = "1.0.0"))
}

githubDownload <- function(url, destfile, ...) {
  onError(1, {
    authenticate    <- yoink("httr", "authenticate")
    GET             <- yoink("httr", "GET")
    content         <- yoink("httr", "content")

    token <- github_pat(quiet = TRUE)
    auth <- if (!is.null(token))
      authenticate(token, "x-oauth-basic", "basic")
    else
      list()
    request <- GET(url, auth)
    writeBin(content(request, "raw"), destfile)
    if (file.exists(destfile)) 0 else 1
  })
}

#' Retrieve GitHub personal access token.
#'
#' A GitHub personal access token
#' Looks in env var `GITHUB_PAT`
#'
#' @keywords internal
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
