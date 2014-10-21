# Used in case we need to special-case packages what packages are cached
isCacheable <- function(package) {
  TRUE
}

isUsingCache <- function(project) {
  isTRUE(get_opts("use.cache", project = project))
}

# We assume 'path' is the path to a DESCRIPTION file
#' @importFrom tools md5sum
hash <- function(path) {
  if (!file.exists(path))
    stop("No DESCRIPTION file at path '", path, "'!")

  pkgName <- basename(dirname(path))

  DESCRIPTION <- as.data.frame(readDcf(path), stringsAsFactors = FALSE)

  # If we already have a GitHub SHA1, just use that
  if ("GithubSHA1" %in% names(DESCRIPTION))
    return(DESCRIPTION$GithubSHA1)

  # TODO: Do we want the 'Built' field used for hashing? The main problem with using that is
  # it essentially makes packages installed from source un-recoverable, since they will get
  # built transiently and installed (and so that field could never be replicated).
  fields <- c("Package", "Version", "Depends", "Imports", "Suggests", "LinkingTo")
  sub <- DESCRIPTION[names(DESCRIPTION) %in% fields]

  # Handle LinkingTo specially -- we need to discover what version of packages in LinkingTo
  # were actually linked against in order to properly disambiguate e.g. httpuv 1.0 linked
  # against Rcpp 0.11.2 and httpuv 1.0 linked against Rcpp 0.11.2.1

  # TODO: It would really be best if, on installation, we recorded what version of LinkingTo
  # packages were actually linked to, in case that package is not available in the library
  # (or, even worse, is actually a different version!)
  linkingToField <- unlist(strsplit(as.character(sub[["LinkingTo"]]), "\\s*,\\s*"))
  linkingToPkgs <- gsub("\\s*\\(.*", "", linkingToField)
  linkingToPkgs <- gsub("^\\s*(.*?)\\s*$", "\\1", linkingToPkgs, perl = TRUE)

  linkingToHashes <- lapply(linkingToPkgs, function(x) {
    DESCRIPTION <- system.file("DESCRIPTION", package = x)
    if (!file.exists(DESCRIPTION)) return(NULL) ## warn later
    else hash(DESCRIPTION)
  })

  missingLinkingToPkgs <- linkingToPkgs[vapply(linkingToHashes, is.null, logical(1))]
  if (length(missingLinkingToPkgs)) {
    warning("The following packages specified in the LinkingTo field for package '",
            pkgName,
            "' are unavailable:\n- ",
            paste(shQuote(missingLinkingToPkgs), collapse = ", "),
            "\nThese packages are required to be installed when attempting to hash this package for caching.",
            call. = FALSE)
  }
  linkingToHashes <- dropNull(linkingToHashes)

  # Normalize for hashing and add in the linkingTo hashes as well
  ready <- normalizeForHash(sub)
  ready <- paste0(ready, do.call(paste0, linkingToHashes))
  tempfile <- tempfile()
  cat(ready, file = tempfile)
  result <- md5sum(tempfile)
  unlink(tempfile)
  if (is.na(result)) stop("Failed to hash file!")
  unname(result)
}

normalizeForHash <- function(item) {
  gsub("[[:space:]]", "", paste(unlist(item), collapse = ""))
}

moveInstalledPackagesToCache <- function(project = NULL) {
  project <- getProjectDir(project)

  # Only do this is we're actually using the packrat cache
  if (!isUsingCache(project)) return(invisible())

  if (!file.exists(cacheLibDir()))
    dir.create(cacheLibDir(), recursive = TRUE)

  project <- getProjectDir(project)

  ## All directories within the 'lib' directory which are not symlinks are fresh
  ## and may need to be moved
  installedPkgPaths <- list.files(libDir(project), full.names = TRUE)
  if (!length(installedPkgPaths)) return(invisible())
  cachedPkgPaths <- list.files(cacheLibDir(), full.names = TRUE)
  cachedPkgHashes <- list.files(cachedPkgPaths)

  needsMove <- installedPkgPaths[sapply(installedPkgPaths, Negate(is.symlink))]

  ## for each package installed that is not a symlink, we migrate it to the cache
  for (package in needsMove) {
    hash <- hash(file.path(package, "DESCRIPTION"))

    ## if the package doesn't exist in the cache, copy it there
    if (!file.exists(cacheLibDir(basename(package), hash))) {
      dir_copy(
        package,
        cacheLibDir(basename(package), hash)
      )
    }

    ## replace the local package with a symlink
    if (!is.symlink(package)) unlink(package, recursive = TRUE)
    symlink(
      normalizePath(cacheLibDir(basename(package), hash)),
      package
    )
  }

}

# Pull out cached package information from the DESCRIPTION
cachedPackages <- function(cacheDir = cacheLibDir(), fields = NULL) {
  pkgCachePaths <- list.files(cacheDir, full.names = TRUE)
  pkgPaths <- setNames(lapply(pkgCachePaths, function(x) {
    list.files(x, full.names = TRUE)
  }), basename(pkgCachePaths))

  lapply(pkgPaths, function(hashedPath) {
    result <- setNames(lapply(hashedPath, function(path) {
      as.list(readDcf(file.path(path, "DESCRIPTION"), all = TRUE))
    }), basename(hashedPath))
    if (!is.null(fields)) {
      result[fields]
    } else {
      result
    }
  })
}

listCachedPackages <- cachedPackages

clearPackageCache <- function(cacheDir = cacheLibDir(), ask = TRUE) {

  if (ask) {
    message("The packrat cache directory was resolved to:\n- ",
            shQuote(cacheDir))
    msg <- "Are you sure you want to clear the packrat cache? [Y/n]: "
    response <- readline(msg)
    if (tolower(substring(response, 1, 1)) != "y") {
      message("Operation aborted.")
      return(invisible(NULL))
    }
  }

  unlink(cacheDir, recursive = TRUE)

}

deletePackagesFromCache <- function(packages, cacheDir = cacheLibDir()) {
  paths <- file.path(cacheDir, packages)
  lapply(paths, function(path) {
    unlink(path, recursive = TRUE)
  })
}
