# Used in case we need to special-case packages what packages are cached
isCacheable <- function(package) {
  TRUE
}

isUsingCache <- function(project) {
  isTRUE(get_opts("use.cache", project = project))
}

installedDescLookup <- function(pkgName) {
  system.file("DESCRIPTION", package = pkgName)
}

# We assume 'path' is the path to a DESCRIPTION file, or a data frame (the
# data frame data must have stringsAsFactors = FALSE).
#
# descLookup is a function that takes a single argument pkgName and must
# return one of: 1) a file path to DESCRIPTION file, 2) a data frame (with
# stringsAsFactors = FALSE) of the DESCRIPTION dcf data, or 3) NULL if
# the DESCRIPTION is not available. By default, installedDescLookup is
# used, which looks in the active lib paths for the desired DESCRIPTION
# files.
#
#' @importFrom tools md5sum
hash <- function(path, descLookup = installedDescLookup) {

  if (!file.exists(path))
    stop("No DESCRIPTION file at path '", path, "'!")

  if (is.data.frame(path)) {
    DESCRIPTION <- path
  } else {
    DESCRIPTION <- as.data.frame(readDcf(path), stringsAsFactors = FALSE)
  }
  pkgName <- DESCRIPTION[["Package"]]

  # Remote SHA backwards compatible with cache v2: use 'GithubSHA1' if exists, otherwise all 'Remote' fields
  remote_fields <- if ("GithubSHA1" %in% names(DESCRIPTION)) {
    "GithubSHA1"
  } else if (is.null(DESCRIPTION[["RemoteType"]]) || DESCRIPTION[["RemoteType"]] == "cran") {
    # Packages installed with install.packages or locally without remotes
    c()
  } else {
    # Mirror the order used by devtools when augmenting the DESCRIPTION.
    c("RemoteType", "RemoteHost", "RemoteRepo", "RemoteUsername", "RemoteRef", "RemoteSha", "RemoteSubdir")
  }

  # Mirror the order of DESCRIPTION fields produced by `package.skeleton` and
  # `devtools::create_description`.
  fields <- c("Package", "Version", "Depends", "Imports", "Suggests", "LinkingTo", remote_fields)

  # TODO: Do we want the 'Built' field used for hashing? The main problem with using that is
  # it essentially makes packages installed from source un-recoverable, since they will get
  # built transiently and installed (and so that field could never be replicated).

  # Create a "sub" data frame with a consistently ordered set of columns.
  #
  # This ensures that package hashing is not sensitive to DESCRIPTION field
  # order.
  common <- intersect(fields, names(DESCRIPTION))
  sub <- DESCRIPTION[common]

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
    linkingToDesc <- descLookup(x)
    # If we return NULL
    if (is.null(linkingToDesc))
      return(NULL)
    else if (is.character(linkingToDesc) && !file.exists(linkingToDesc))
      return(NULL)
    else
      hash(linkingToDesc, descLookup = descLookup)
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


  linkingToHashes <- if (length(linkingToHashes))
    paste(
      collapse = "",
      sort_c(unlist(dropNull(linkingToHashes)))
    )

  # Normalize for hashing and add in the linkingTo hashes as well
  ready <- normalizeForHash(sub)
  ready <- paste(ready, linkingToHashes)
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

isVerboseCache <- function() {
  return(isTRUE(getOption("packrat.verbose.cache")))
}

# helper function to remove the package from its original location and
# create a symlink to the cached version.
symlinkPackageToCache <- function(packagePath, cachedPackagePath) {
  packageName <- basename(packagePath)
  backupPackagePath <- tempfile(tmpdir = dirname(packagePath))
  if (!file.rename(packagePath, backupPackagePath)) {
    stop("failed to back up package directory '", packagePath, "'; cannot safely link to cache.")
  }
  on.exit(unlink(backupPackagePath, recursive = TRUE), add = TRUE)

  if (!symlink(cachedPackagePath, packagePath)) {
    # symlink failed; attempt to restore the backup back to its original name.
    if (!file.rename(backupPackagePath, packagePath)) {
      stop("failed to restore package from '", backupPackagePath, "' to ",
           "'", packagePath, "' after symlink to ",
           "'", cachedPackagePath, "' failed; package may be lost")
    }
    stop("failed to create a symlink from '", packagePath, "' to '", cachedPackagePath, "'")
  }

  if (isVerboseCache()) {
    message("Using cached ", packageName, ".")
  }
  return(cachedPackagePath)
}

# Given a path to an installed package (outside the packrat cache), move that
# package into the cache and replace the original directory with a symbolic
# link into the package cache.
#
# If the package already exists inside the cache, overwrite=TRUE causes
# replacement of the cached content while overwrite=FALSE with fatal=FALSE
# uses the cached package. Using overwrite=TRUE with fatal=TRUE will err.
moveInstalledPackageToCache <- function(packagePath,
                                        hash,
                                        overwrite = TRUE,
                                        fatal = FALSE,
                                        cacheDir = cacheLibDir())
{
  ensureDirectory(cacheDir)

  packageName <- basename(packagePath)
  cachedPackagePath <- file.path(cacheDir, packageName, hash, packageName)
  backupPackagePath <- tempfile(tmpdir = dirname(cachedPackagePath))

  # check for existence of package in cache
  if (file.exists(cachedPackagePath)) {
    if (fatal && !overwrite) {
      stop("cached package already exists at path '", cachedPackagePath, "'")
    }

    if (!fatal) {
      return(symlinkPackageToCache(packagePath, cachedPackagePath))
    }
  }

  # back up a pre-existing cached package (restore on failure)
  if (file.exists(cachedPackagePath)) {
    if (!file.rename(cachedPackagePath, backupPackagePath)) {
      stop("failed to back up package '", packageName, "'; cannot safely copy to cache")
    }
    on.exit(unlink(backupPackagePath, recursive = TRUE), add = TRUE)
  }

  if (isVerboseCache()) {
    message("Caching ", packageName, ".")
  }

  # attempt to rename to cache
  if (suppressWarnings(file.rename(packagePath, cachedPackagePath))) {
    return(symlinkPackageToCache(packagePath, cachedPackagePath))
  }

  # rename failed; copy to temporary destination in same directory
  # and then attempt to rename from there
  tempPath <- tempfile(tmpdir = dirname(cachedPackagePath))
  on.exit(unlink(tempPath, recursive = TRUE), add = TRUE)
  if (all(dir_copy(packagePath, tempPath))) {

    # check to see if the cached package path exists now; if it does,
    # assume that this was generated by another R process that successfully
    # populated the cache
    if (file.exists(cachedPackagePath)) {
      return(symlinkPackageToCache(packagePath, cachedPackagePath))
    }

    # attempt to rename to target path
    if (suppressWarnings(file.rename(tempPath, cachedPackagePath))) {
      return(symlinkPackageToCache(packagePath, cachedPackagePath))
    }
  }

  # failed to insert package into cache -- clean up and return error
  if (!file.rename(backupPackagePath, cachedPackagePath)) {
    stop("failed to restore package '", packageName, "' in cache; package may be lost from cache")
  }

  # return failure
  stop("failed to copy package '", packageName, "' to cache")
}

# Pull out cached package information from the DESCRIPTION
cachedPackages <- function(cacheDir = cacheLibDir(), fields = NULL) {

  pkgCachePaths <- list.files(cacheDir, full.names = TRUE)
  pkgPaths <- setNames(lapply(pkgCachePaths, function(x) {
    list.files(x, full.names = TRUE)
  }), basename(pkgCachePaths))

  lapply(seq_along(pkgPaths), function(i) {

    pkgName <- names(pkgPaths)[[i]]
    hashedPaths <- pkgPaths[[i]]

    result <- setNames(lapply(hashedPaths, function(path) {
      as.list(readDcf(file.path(path, pkgName, "DESCRIPTION"), all = TRUE))
    }), pkgName)

    if (!is.null(fields)) {
      lapply(result, `[`, fields)
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
