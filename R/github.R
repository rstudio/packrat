isGitHubURL <- function(url) {
  is.string(url) && grepl("^http(?:s)?://(?:www|api).github.com", url, perl = TRUE)
}

canUseGitHubDownloader <- function() {
  all(packageVersionInstalled(devtools = "1.9.1", httr = "1.0.0"))
}

githubDownload <- function(url, destfile, ...) {
  github_pat      <- yoink("devtools", "github_pat")
  authenticate    <- yoink("httr", "authenticate")
  GET             <- yoink("httr", "GET")
  content         <- yoink("httr", "content")

  token <- github_pat(quiet = TRUE)
  auth <- authenticate(token, "x-oauth-basic", "basic")
  request <- GET(url, auth)
  writeBin(content(request, "raw"), destfile)
  if (file.exists(destfile)) 0 else 1
}
