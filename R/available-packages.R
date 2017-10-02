availablePackagesSkeleton <- function() {

  default_fields <- yoink("tools", ".get_standard_repository_db_fields")

  fields <- c(
    default_fields(),
    "File",
    "Repository"
  )

  data <- array(
    character(),
    dim = c(0L, length(fields)),
    dimnames = list(NULL, fields)
  )

  data
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
  #
  # NOTE: older versions of R don't support the 'repos' argument to
  # available.packages
  result <- tryCatch(
    available.packages(
      contriburl = utils::contrib.url(repos, type),
      type = type
    ),
    error = function(e) {
      availablePackagesSkeleton()
    }
  )

  # cache and return
  .packrat$repos[[key]] <- result
  result
}
