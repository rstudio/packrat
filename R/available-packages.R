# Call 'available.packages()' with an invalid URL to get the
# 'skeleton' output of 'available.packages()' (ie, an empty matrix
# with all appropriate fields populated)
availablePackagesSkeleton <- function() {

  # Use internal download file method just to ensure no errors leak.
  download.file.method <- getOption("download.file.method")
  on.exit(options(download.file.method = download.file.method), add = TRUE)
  options(download.file.method = "internal")

  # Use 'available.packages()' to query a URL that doesn't exist
  result <- withCallingHandlers(
    available.packages("/no/such/path/here/i/hope/"),
    warning = function(w) invokeRestart("muffleWarning"),
    message = function(m) invokeRestart("muffleMessage")
  )

  result
}

availablePackagesBinary <- function(repos = getOption("repos")) {
  available.packages(repos = repos, type = .Platform$pkgType)
}

availablePackagesSource <- function(repos = getOption("repos")) {
  available.packages(repos = repos, type = "source")
}
