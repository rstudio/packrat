isGitHubURL <- function(url) {
  is.string(url) && grepl("^http(?:s)?://www.github.com", url, perl = TRUE)
}

canUseGitHubDownloader <- function() {
  all(packageVersionInstalled(devtools = "1.9.1", httr = "1.0.0"))
}

githubDownload <- function(url, destfile, ...) {
  github_pat      <- yoink("devtools", "github_pat")
  github_auth     <- yoink("devtools", "github_auth")
  GET             <- yoink("httr", "GET")
  content         <- yoink("httr", "content")

  pat <- github_pat(quiet = TRUE)
  auth <- github_auth(pat)
  request <- GET(url, auth)
  writeBin(content(request, "raw"), destfile)
  if (file.exists(destfile)) 0 else 1
}
