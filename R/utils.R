silent <- function(expr) {
  suppressWarnings(suppressMessages(
    capture.output(result <- eval(expr, envir = parent.frame()))
  ))
  result
}

list_files <- function(path = ".", pattern = NULL, all.files = FALSE,
                       full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
                       include.dirs = FALSE, no.. = TRUE) {

  files <- list.files(path = path, pattern = pattern, all.files = all.files,
                      full.names = full.names, recursive = recursive,
                      ignore.case = ignore.case, include.dirs = include.dirs, no.. = no..)

  dirs <- list.dirs(path = path, full.names = full.names, recursive = recursive)
  setdiff(files, dirs)

}
