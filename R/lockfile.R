writeLockFile <- function(file, lockinfo) {
  json <- toJSON(lockinfo)
  writeLines(json, con = file)
  invisible()
}

readLockFile <- function(file) {
  lines <- readLines(file, warn=FALSE, encoding='UTF-8')
  return(fromJSON(paste(lines, collapse='\n')))
}
