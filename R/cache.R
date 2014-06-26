# Used in case we need to special-case packages what packages are cached
isCacheable <- function(package) {
  TRUE
}

# We assume 'path' is the path to a DESCRIPTION file
hash <- function(path) {
  DESCRIPTION <- as.data.frame(readDcf(path), stringsAsFactors = FALSE)
  fields <- c("Package", "Version", "Depends", "Imports", "Suggests", "LinkingTo", "Built")
  sub <- DESCRIPTION[names(DESCRIPTION) %in% fields]
  ready <- normalizeForHash(sub)
  tempfile <- tempfile()
  cat(ready, file = tempfile)
  result <- tools:::md5sum(tempfile)
  unlink(tempfile)
  if (is.na(result)) stop("Failed to hash file!")
  unname(result)
}

normalizeForHash <- function(item) {
  gsub("[[:space:]]", "", paste(unlist(item), collapse = ""))
}

moveInstalledPackagesToCache <- function(project = NULL) {

  if (!file.exists(cacheLibDir()))
    dir.create(cacheLibDir(), recursive = TRUE)

  project <- getProjectDir(project)

  ## All directories within the 'lib' directory which are not symlinks are fresh
  ## and may need to be moved
  installedPkgPaths <- list.files(libDir(project), full.names = TRUE)
  if (!length(installedPkgPaths)) return(invisible())
  cachedPkgHashes <- list.files(cacheLibDir())

  needsMove <- installedPkgPaths[sapply(installedPkgPaths, Negate(is.symlink))]

  ## for each package installed that is not a symlink, we migrate it to the cache
  for (package in needsMove) {
    hash <- hash(file.path(package, "DESCRIPTION"))

    ## if the package doesn't exist in the cache, copy it there
    if (!file.exists(cacheLibDir(hash))) {
      dir_copy(
        package,
        cacheLibDir(hash)
      )
    }

    ## replace the local package with a symlink
    if (!is.symlink(package)) unlink(package, recursive = TRUE)
    file.symlink(normalizePath(cacheLibDir(hash)), package)
  }

}
