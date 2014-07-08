# Read only package entries in the lock file, and do not expand package dependencies
# Useful when a package + its requirements is of interest, and expansion of
# sub-dependencies is unnecessary
readLockFilePackages <- function(file) {

  # Drop the first row as it contains lockfile-specific info
  lock <- readDcf(file)[-1, , drop = FALSE]
  result <- apply(lock, 1, function(x) {
    x <- as.list(x)
    list(
      name = x$Package,
      source = x$Source,
      version = x$Version,
      requires = as.character(unlist(strsplit(as.character(x$Requires), ",[[:space:]]*", perl = TRUE))),
      hash = x$Hash
    )
  })
  names(result) <- lock[, "Package"]
  result

}
