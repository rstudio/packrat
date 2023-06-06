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
  } else if (is.null(DESCRIPTION[["RemoteType"]]) || DESCRIPTION[["RemoteType"]] %in% c("cran", "standard", "url")) {
    # Package installed from a CRAN-like repository by install.packages (null),
    # remotes (cran, url), or pak (standard).
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
  backedUp <- FALSE
  if (file.exists(packagePath)) {
    backupPackagePath <- tempfile(tmpdir = dirname(packagePath))
    if (!file.rename(packagePath, backupPackagePath)) {
      stop("failed to back up package directory '", packagePath, "'; cannot safely link to cache.")
    }
    on.exit(unlink(backupPackagePath, recursive = TRUE), add = TRUE)
  }

  if (!symlink(cachedPackagePath, packagePath)) {
    # symlink failed; attempt to restore the backup back to its original name.
    if (backedUp) {
      if (!file.rename(backupPackagePath, packagePath)) {
        stop("failed to restore package from '", backupPackagePath, "' to ",
             "'", packagePath, "' after symlink to ",
             "'", cachedPackagePath, "' failed; package may be lost")
      }
    }
    stop("failed to create a symlink from '", packagePath, "' to '", cachedPackagePath, "'")
  }

  if (isVerboseCache()) {
    message("Using cached ", packageName, ".")
  }
  return(cachedPackagePath)
}

# Given a path, move that location to a temporary location alongside the
# original path. Returns an exit handler that either removes the temporary
# location (when something else exists at the original location) or restores
# the file back to its original location when nothing is there.
#
# The temporary location is not revealed to the caller.
cacheFileBackup <- function(path) {
  if (!file.exists(path)) {
    return(function() {})
  }

  backupPath <- tempfile(tmpdir = dirname(path))

  if (!file.rename(path, backupPath)) {
      stop("cannot back-up existing cache directory '", path, "'; cannot safely copy into cache")
  }

  # Return a function appropriate as an on.exit clean-up handler.
  function() {
    if (!file.exists(path)) {
      if (!file.rename(backupPath, path)) {
        stop("failed to reset cache directory back-up '", path, "'; package may be lost from cache")
      }
    } else {
      unlink(backupPath, recursive = TRUE)
    }
  }

}

# Given a path to an installed package (outside the packrat cache), move that
# package into the cache and replace the original directory with a symbolic
# link into the package cache.
#
# If the package already exists inside the cache, overwrite=TRUE causes
# replacement of the cached content while overwrite=FALSE with fatal=FALSE
# uses the cached package. Using overwrite=TRUE with fatal=TRUE will err.
#
# When rename=TRUE, a file-system rename from the package library to the cache
# is attempted without before falling-back to a directory copy. Used by tests
# to bypass the initial file.rename and force the directory copy.
moveInstalledPackageToCache <- function(packagePath,
                                        hash,
                                        cacheDir = cacheLibDir(),
                                        overwrite = TRUE,
                                        fatal = FALSE,
                                        rename = TRUE)
{
  ensureDirectory(cacheDir)

  packageName <- basename(packagePath)
  cachedPackagePath <- file.path(cacheDir, packageName, hash, packageName)

  # check for existence of package in cache
  if (file.exists(cachedPackagePath)) {
    if (fatal && !overwrite) {
      stop("cached package already exists at path '", cachedPackagePath, "'")
    }

    if (!fatal) {
      return(symlinkPackageToCache(packagePath, cachedPackagePath))
    }
  }

  if (isVerboseCache()) {
    message("Caching ", packageName, ".")
  }

  # back up a pre-existing cached package (restore on failure)
  restore <- cacheFileBackup(cachedPackagePath)
  on.exit(restore(), add = TRUE)

  # attempt to rename to cache
  if (rename) {
    if (suppressWarnings(file.rename(packagePath, cachedPackagePath))) {
      return(symlinkPackageToCache(packagePath, cachedPackagePath))
    }
  }

  # rename failed; copy into temporary destination within the same directory
  # as our target path and attempt to rename from there.
  #
  # this is common when the cache is on a different file-system than the
  # project library.
  tempPath <- tempfile(tmpdir = dirname(cachedPackagePath))
  on.exit(unlink(tempPath, recursive = TRUE), add = TRUE)
  copied <- dir_copy(packagePath, tempPath)
  if (all(copied)) {

    # The cache location was moved aside before our (expensive) directory
    # copy. Check to see if has reappeared, which means that some other
    # process successfully populated the cache.
    if (file.exists(cachedPackagePath)) {
      return(symlinkPackageToCache(packagePath, cachedPackagePath))
    }

    # attempt to rename to target path
    if (suppressWarnings(file.rename(tempPath, cachedPackagePath))) {
      return(symlinkPackageToCache(packagePath, cachedPackagePath))
    } else {
      # Error. Rename failed (within same directory structure).
      # Check one last time if the package exists.
      if (file.exists(cachedPackagePath)) {
        return(symlinkPackageToCache(packagePath, cachedPackagePath))
      }

    }
  } else {
    # Error. Copy failed.

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
