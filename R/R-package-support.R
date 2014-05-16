## Update the .Rbuildignore so that packrat-related files are not included
updateRBuildIgnore <- function(projDir = NULL) {

  projDir <- getProjectDir(projDir)
  ignoreDirective <- paste("^", .packrat$packratFolderName, "/", sep = "")

  if (!file.exists("DESCRIPTION")) {
    stop("This project does not appear to be an R package -- there is no 'DESCRIPTION' file.")
  }

  ## If the .Rbuildignore doesn't exist, create and fill it
  path <- file.path(projDir, ".Rbuildignore")
  if (!file.exists(".Rbuildignore")) {
    cat("^packrat/", file = file.path(projDir, ".Rbuildignore"))
  }

  ## If it already exists, check for a '^packrat/' directive; add it if none
  .Rbuildignore <- readLines(path)
  if (!("^packrat/" %in% .Rbuildignore)) {
    .Rbuildignore <- c(.Rbuildignore, ignoreDirective)
    cat(.Rbuildignore, file = path, sep = "\n")
  }

}
