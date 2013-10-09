getDescription <- function(appDir) {
  descFilePath <- file.path(appDir, "DESCRIPTION")
  if (!file.exists(descFilePath)) {
    stop(paste(descFilePath, " not found. Run packrat::bootstrap('", appDir, 
               "') to initialize packrat for this directory.", sep = ""))
  }
  as.data.frame(read.dcf(descFilePath))
}