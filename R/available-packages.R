defaultRepositoryDbFields <- function() {
  c(
    "Package",
    "Version",
    "Priority",
    "Depends",
    "Imports",
    "LinkingTo",
    "Suggests",
    "Enhances",
    "License",
    "License_is_FOSS",
    "License_restricts_use",
    "OS_type",
    "Archs",
    "MD5sum",
    "NeedsCompilation"
  )
}

availablePackagesSkeleton <- function() {

  tools <- asNamespace("tools")
  defaults <- tryCatch(
    tools$.get_standard_repository_db_fields(type = "source"),
    error = identity
  )

  if (inherits(defaults, "error"))
    defaults <- defaultRepositoryDbFields()

  fields <- c(defaults, "File", "Repository")

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
