# This constant represents the version of the description file format Packrat 
# currently understands. It should be incremented when a breaking change is made 
# to the file's format (such that older versions of Packrat are no longer able 
# to properly interpret the file's contents.)
# 
# Not to be confused with the version of Packrat itself. 
CURRENT_DESCRIPTION_VERSION <- 1

getDescription <- function(appDir) {
  descFilePath <- file.path(appDir, "DESCRIPTION")
  if (!file.exists(descFilePath)) {
    stop(descFilePath, " not found. Run packrat::bootstrap('", appDir, 
         "') to initialize packrat for this directory.")
  }
  description <- as.data.frame(read.dcf(descFilePath))
  if (is.null(description$FormatVersion)) {
    stop(descFilePath, " exists but is not a valid Packrat application ",
         "description file.");
  } else if (as.integer(as.character(description$FormatVersion)) > 
             CURRENT_DESCRIPTION_VERSION) {
    stop(appDir, " is managed by a newer version of Packrat. Upgrade to the ", 
         "newest version of Packrat and try again.")
  }
  description
}

setDescription <- function(appDir, desc) {
  desc$FormatVersion <- CURRENT_DESCRIPTION_VERSION
  descFilePath <- file.path(appDir, "DESCRIPTION")
  write.dcf(desc, descFilePath)
}

addDependencies <- function(appDir, packages) {
  desc <- getDescription(appDir)
  if (is.null(desc$Depends))
    depends <- character(0)
  else
    depends <- strsplit(as.character(desc$Depends), '\\s*,\\s*')[[1]]
  toAdd <- sapply(packages, function(package) {
    !any(grepl(paste('\\b\\Q', package, '\\E\\b', sep=''), depends))
  })
  depends <- c(depends, packages[toAdd])
  desc$Depends <- paste(depends, collapse=', ')
  setDescription(appDir, desc)
}

# check whether the specified file ends with newline
ends_with_newline <- function(path) {
  conn <- file(path, open = "rb", raw = TRUE)
  on.exit(close(conn))
  seek(conn, where = -1, origin = "end")
  lastByte <- readBin(conn, "raw", n = 1)
  lastByte == 0x0a
}

appendToDcf <- function(path, records) {
  if (!ends_with_newline(path))
    cat('\n', sep='', file=path, append=TRUE)
  write.dcf(records, path, append = TRUE)
}