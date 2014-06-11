## Initialize some directories
mkdir <- function(path)
  dir.create(path, showWarnings = FALSE, recursive = TRUE)

git <- function(...) {
  system(paste("git", ..., collapse = " "))
}

normalize_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE)
}
