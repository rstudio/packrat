# Used in case we need to special-case packages what packages are cached
isCacheable <- function(package) {
  TRUE
}

isUsingCache <- function(project) {
  isTRUE(get_opts("use.cache", project = project))
}

hashLib <- function(libDir, pkgName) {
  if (is.windows()) {
    hashLibWindows(libDir, pkgName)
  } else if (is.mac()) {
    hashLibMac(libDir, pkgName)
  } else if (is.linux()) {
    hashLibLinux(libDir, pkgName)
  }
}

findOtool <- function(failureMessage) {
  otool <- Sys.which("otool")
  if (otool == "")
    stop(message)
  otool
}

findMd5sum <- function(failureMessage) {
  md5 <- Sys.which("md5sum")
  if (md5 == "") {
    md5 <- Sys.which("md5")
    if (md5 == "") {
      stop(failureMessage)
    }
  }
  md5
}

findObjdump <- function(failureMessage) {
  objdump <- Sys.which("objdump")
  if (objdump == "")
    stop(failureMessage)
  objdump
}

hashLibMac <- function(libDir, pkgName) {
  soPath <- list.files(libDir, full.names = TRUE)
  if (!length(soPath)) return(character())
  otool <- findOtool("Could not find 'otool': Please install command line tools.")
  md5 <- findMd5sum("Could not find 'md5' or 'md5sum': Please install command line tools.")
  hash <- system(paste(shQuote(otool), "-t", shQuote(soPath), "|", shQuote(md5)), intern = TRUE)
  return(hash)
}

hashLibLinux <- function(libDir, pkgName) {
  soPath <- list.files(libDir, full.names = TRUE)
  if (!length(soPath)) return(character())
  objdump <- findObjdump("Could not locate 'objdump': do you have the appropriate command line tools installed?")
  md5 <- findMd5sum("Could not locate 'md5': do you have the appropriate command line tools installed?")
  hash <- system(paste(shQuote(objdump), "-d", shQuote(soPath), "|", shQuote(md5)), intern = TRUE)
  gsub(" .*", "", hash)

}

hashLibWindows <- function(libDir, pkgName) {
  dllPaths <- list.files(libDir, recursive = TRUE, full.names = TRUE, pattern = glob2rx("*.dll"))
  if (!length(dllPaths)) return(character())
  objdump <- findObjdump("Could not locate 'objdump': please ensure Rtools is installed and on your PATH.")
  tmpFile <- tempfile(fileext = ".txt")
  on.exit(unlink(tmpFile))
  lapply(dllPaths, function(path) {
    path <- normalizePath(path)
    tmpFile <- normalizePath(tmpFile)
    shell(paste(shQuote(objdump), "-d", shQuote(path), ">>", shQuote(tmpFile)))
  })
  tools:::md5sum(tmpFile)
  gsub(" .*", "", hash)
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

  fields <- c("Package", "Version", "Depends", "Imports", "Suggests", "LinkingTo")
  sub <- DESCRIPTION[names(DESCRIPTION) %in% fields]

  # If the package has a shared object file, hash components of that as well
  libDir <- file.path(dirname(path), "libs")
  libHash <- hashLib(libDir, pkgName)

  # Normalize for hashing
  ready <- normalizeForHash(sub)
  tempfile <- tempfile()
  on.exit(unlink(tempfile))
  cat(ready, file = tempfile)
  cat(libHash, file = tempfile, append = TRUE)
  result <- md5sum(tempfile)
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
      readDcf(file.path(path, "DESCRIPTION"), all = TRUE)
    }), basename(hashedPath))
    if (!is.null(fields)) {
      result[fields]
    } else {
      result
    }
  })
}
