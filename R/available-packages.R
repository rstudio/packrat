# Call 'available.packages()' with an invalid URL to get the
# 'skeleton' output of 'available.packages()' (ie, an empty matrix
# with all appropriate fields populated)
availablePackagesSkeleton <- function() {

  if (!is.null(.packrat$repos[["skeleton"]]))
    return(.packrat$repos[["skeleton"]])

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

  .packrat$repos[["skeleton"]] <- result
  result
}

availablePackagesBinary <- function(repos = getOption("repos")) {
  availablePackages(repos = repos, type = .Platform$pkgType)
}

availablePackagesSource <- function(repos = getOption("repos")) {
  availablePackages(repos = repos, type = "source")
}

hasBinaryRepositories <- function() {
  !identical(.Platform$pkgType, "source")
}

availablePackages <- function(repos = getOption("repos"),
                              type = getOption("pkgType"))
{
  # check cache for entry
  key <- paste(deparse(repos), deparse(type), sep = " ", collapse = " ")
  if (!is.null(.packrat$repos[[key]]))
    return(.packrat$repos[[key]])

  # catch errors related to e.g. missing PACKAGES file (could happen for
  # source-only repositories, if we tried to query a binary repository)
  result <- tryCatch(
    available.packages(repos = repos, type = type),
    error = function(e) {
      availablePackagesSkeleton()
    }
  )

  # cache and return
  .packrat$repos[[key]] <- result
  result
}
