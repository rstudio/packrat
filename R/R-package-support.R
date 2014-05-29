## Expected to be used with .Rbuildignore, .Rinstignore
updateIgnoreFile <- function(projDir = NULL, file) {

  projDir <- getProjectDir(projDir)
  ignoreDirective <- paste("^", .packrat$packratFolderName, "/", sep = "")

  if (!file.exists("DESCRIPTION")) {
    stop("This project does not appear to be an R package -- there is no 'DESCRIPTION' file.")
  }

  ## If the file doesn't exist, create and fill it
  path <- file.path(projDir, "file")
  if (!file.exists("file")) {
    cat("^packrat/", file = file.path(projDir, "file"))
  }

  ## If it already exists, check for a '^packrat/' directive; add it if none
  file <- readLines(path)
  if (!(ignoreDirective %in% file)) {
    file <- c(file, ignoreDirective)
    cat(file, file = path, sep = "\n")
  }

}

updateRBuildIgnore <- function(projDir = NULL) {
  updateIgnoreFile(projDir = projDir, file = ".Rbuildignore")
}

updateRInstIgnore <- function(projDir = NULL) {
  updateIgnoreFile(projDir = projDir, file = ".Rinstignore")
}
