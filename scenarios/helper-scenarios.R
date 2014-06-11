## Initialize some directories
mkdir <- function(path)
  dir.create(path, showWarnings = FALSE, recursive = TRUE)

git <- function(...) {
  system(paste("git", ..., collapse = " "))
}
