getDescription <- function(appDir) {
  descFilePath <- file.path(appDir, "DESCRIPTION")
  if (!file.exists(descFilePath)) {
    stop(paste(descFilePath, " not found. Run packrat::bootstrap('", appDir, 
               "') to initialize packrat for this directory.", sep = ""))
  }
  as.data.frame(read.dcf(descFilePath))
}

setDescription <- function(appDir, desc) {
  descFilePath <- file.path(appDir, "DESCRIPTION")
  write.dcf(desc, descFilePath)
}

addDependencies <- function(appDir, packages) {
  desc <- getDescription(appDir)
  depends <- strsplit(as.character(desc$Depends), '\\s*,\\s*')[[1]]
  toAdd <- sapply(packages, function(package) {
    !any(grepl(paste('\\b\\Q', package, '\\E\\b', sep=''), depends))
  })
  depends <- c(depends, packages[toAdd])
  desc$Depends <- paste(depends, collapse=', ')
  setDescription(appDir, desc)
}
