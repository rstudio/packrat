#' Given an application directory, computes the application's dependencies, 
#' and places the application's dependencies under packrat control. 
#' 
#' @export
bootstrap <- function(appDir = getwd()) {
  dependencies <- data.frame(Source = getOption("repos")[[1]],
                             Dependencies = paste(appDependencies(appDir),
                                                  collapse=", "))
  write.dcf(dependencies, file = paste(appDir, "/DESCRIPTION", sep = ""))
  snapshot(appDir)
}

install <- function(appDir = getwd()) {
  lockFilePath <- file.path(appDir, "packrat.lock")
  if (!file.exists(lockFilePath)) {
    stop(paste(lockFilePath, " is missing. Run packrat::bootstrap('",
               appDir, "') to generate it.", sep = ""))
  }
  packages <- readLockFile(lockFilePath)
  installList <- makeInstallList(packages)
  print(installList)
}

pack <- function() {
}
