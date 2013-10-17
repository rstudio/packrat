writeLockFile <- function(file, lockinfo) {
  
  rver <- as.character(getRversion())
  
  json <- toJSON(list(
    packrat_format = '1.0',
    packrat_version = packageVersion('packrat'),
    r_version = rver,
    packages = lockinfo
  ), pretty=TRUE)
  writeLines(json, con = file)
  invisible()
}

readLockFile <- function(file) {
  lines <- readLines(file, warn=FALSE, encoding='UTF-8')
  obj <- fromJSON(paste(lines, collapse='\n'))
  if (is.null(obj$packrat_format)) {
    # Earliest format. We can auto-upgrade.
    return(list(
      packrat_format = '1.0',
      packrat_version = '0.0.0',
      r_version = NULL,
      packages = obj
    ))
  } else if (compareVersion('1.0', obj$packrat_format) < 0) {
    # Future format. Abort.
    stop("The lockfile was written by an incompatible version of packrat (",
         obj$packrat_version,
         ").\nPlease upgrade and try again.")
  } else {
    return(obj)
  }
}
