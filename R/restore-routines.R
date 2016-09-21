restoreWithCopyFromCache <- function(project,
                                     pkgRecord,
                                     cache,
                                     cacheCopyStatus)
{
  # don't copy from cache if disabled for this project
  if (!isUsingCache(project))
    return(FALSE)

  # don't try to use cache if we don't have a hash
  if (!length(pkgRecord$hash))
    return(FALSE)

  # don't try to cache uncacheable packages (ie, packages that
  # need to be reinstalled each time for whatever reason)
  if (!isCacheable(pkgRecord$name))
    return(FALSE)

  # ensure that the hash exists in the cache
  if (!(pkgRecord$hash %in% cache))
    return(FALSE)

  # ensure that the cache package path exists
  source <- cacheLibDir(pkgRecord$name, pkgRecord$hash, pkgRecord$name)
  if (!file.exists(source))
    return(FALSE)

  # attempt to form a symlink to the packrat library
  target <- file.path(libDir(project), pkgRecord$name)
  suppressWarnings(symlink(source, target))
  success <- file.exists(target)
  if (success) {
    cacheCopyStatus$type <- "symlinked cache"
    return(TRUE)
  }

  # symlinking failed; attempt a copy from the cache to the target directory
  success <- all(dir_copy(
    cacheLibDir(pkgRecord$name, pkgRecord$hash),
    file.path(libDir(project), pkgRecord$name)
  ))

  if (success) {
    cacheCopyStatus$type <- "copied cache"
    return(TRUE)
  }

  # failed to copy or symlink from cache; report warning and return false
  warning("failed to symlink or copy package '", pkgRecord$name, "' from cache")
  return(FALSE)
}
