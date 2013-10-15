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
  } else if (as.integer(description$FormatVersion) > 
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
