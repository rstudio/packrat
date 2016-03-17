# Tools for storing state in environment variables.
getenv <- function(x) {
  strsplit(Sys.getenv(x, unset = ""), .Platform$path.sep, fixed = TRUE)[[1]]
}

setenv <- function(name, value) {
  value <- paste(value, collapse = .Platform$path.sep)
  call <- list(value)
  names(call) <- name
  do.call(Sys.setenv, call)
}

unsetenv <- function(name) {
  Sys.unsetenv(name)
}

# for autocompletion
.packrat.env <- list(
  "R_PACKRAT_DEFAULT_LIBPATHS",
  "R_PACKRAT_SYSTEM_LIBRARY",
  "R_PACKRAT_SITE_LIBRARY"
)
names(.packrat.env) <- unlist(.packrat.env)
