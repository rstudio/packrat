# Clone a test project into a temporary directory for manipulation; returns the
# path to the test project
cloneTestProject <- function(projectName) {
  root <- file.path(system.file("tests", package = "packrat"), 
                    "projects", projectName)
  target <- tempdir()
  if (file.exists(file.path(target, projectName))) {
    file.remove(file.path(target, projectName, recursive = TRUE))
  }
  file.copy(root, target, recursive = TRUE)
  return(file.path(target, projectName))
}

# Sets up the fake repo used for testing
setupTestRepo <- function() {
  repo <- paste("file://", 
                file.path(system.file("tests", package = "packrat"), "repo"),
                sep = "")
  names(repo) <- "CRAN"
  options("repos" = repo) 
  options("pkgType" = "source")
}