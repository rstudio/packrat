isBitbucketURL <- function(url) {
  is.string(url) && grepl("^http(?:s)?://(?:www|api).bitbucket.(org|com)", url, perl = TRUE)
}

canUseBitbucketDownloader <- function() {
  all(packageVersionInstalled(devtools = "1.9.1", httr = "1.0.0"))
}

bitbucketDownload <- function(url, destfile, ...) {
  onError(1, {
    GET             <- yoink("httr", "GET")
    content         <- yoink("httr", "content")

    request <- GET(url) #, auth)
    writeBin(content(request, "raw"), destfile)
    if (file.exists(destfile)) 0 else 1
  })
}
