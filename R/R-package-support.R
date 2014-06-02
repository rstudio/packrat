## Expected to be used with .Rbuildignore, .Rinstignore
updateIgnoreFile <- function(projDir = NULL, file, fields) {

  projDir <- getProjectDir(projDir)

  if (!file.exists("DESCRIPTION")) {
    stop("This project does not appear to be an R package -- there is no 'DESCRIPTION' file.")
  }

  ## If the file doesn't exist, create and fill it
  path <- file.path(projDir, file)
  if (!file.exists(path)) {
    cat(fields, file = file, sep = "\n")
    return(invisible())
  }

  ## If it already exists, fill as necessary
  content <- readLines(path)
  for (field in fields) {
    if (!(field %in% content)) {
      content <- c(content, field)
    }
  }
  cat(content, file = path, sep = "\n")
  return(invisible())

}

updateRBuildIgnore <- function(projDir = NULL) {
  updateIgnoreFile(projDir = projDir, file = ".Rbuildignore", fields = "^packrat/")
}

updateGitIgnore <- function(projDir = NULL) {
  updateIgnoreFile(projDir = projDir, file = ".gitignore", fields = "packrat/lib/")
}
