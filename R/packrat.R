#' Given an application directory, computes the application's dependencies, 
#' and places the application's dependencies under packrat control. 
#' 
#' @export
bootstrap <- function(appDir = getwd()) {
  dependencies <- data.frame(Source = getOption("repos")[[1]],
                             Dependencies = paste(appDependencies(appDir),
                                                  collapse=", "))
  write.dcf(dependencies, file = paste(appDir, "/DESCRIPTION", sep = ""))
}

install <- function() {
}

pack <- function() {
}
