# Package dependency:
# list(
#   name = 'ggplot2',
#   source = 'CRAN',
#   version = '0.9.3.1', # or: '>= 3.0', 'github:hadley/ggplot2/fix/axis', ''
# )

# Package record:
# list(
#   name = 'ggplot2',
#   source = 'github',
#   version = '0.9.3.1',
#   gh_repo = 'ggplot2',
#   gh_username = 'hadley',
#   gh_ref = 'master',
#   gh_sha1 = '66b81e9307793029f6083fc6108592786a564b09'
# )


# Returns a package records for the given packages
getPackageRecords <- function(pkgNames) {
  lapply(pkgNames, function(pkgName) {
    pkgDescFile <- system.file('DESCRIPTION', package=pkgName)
    return(inferRecordFromDescriptionFile(pkgDescFile))
  })
}

# Reads a description file and attempts to infer where the package came from.
# Currently works only for packages installed from CRAN or from GitHub using
# devtools 1.4 or later.
inferRecordFromDescriptionFile <- function(file) {
  df <- as.data.frame(read.dcf(file))
  name <- as.character(df$Package)
  ver <- as.character(df$Version)
  
  if (!is.null(df$Repository) && identical(df$Repository, 'CRAN')) {
    # It's CRAN!
    return(structure(list(
      name = name,
      source = 'CRAN',
      version = ver
    ), class='packageRecord CRAN'))
  } else if (!is.null(df$GithubRepo)) {
    # It's GitHub!
    return(structure(list(
      name = name,
      source = 'github',
      version = ver,
      gh_repo = as.character(df$GithubRepo),
      gh_username = as.character(df$GithubUsername),
      gh_ref = as.character(df$GithubRef),
      gh_sha1 = as.character(df$GithubSHA1)
    ), class='packageRecord github'))
  } else {
    warning("Couldn't figure out the origin of package ", name)
    return(structure(list(
      name = name,
      version = ver
    ), class='packageRecord'))
  }
}