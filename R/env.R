# Tools for storing state in environment variables.
getenv <- function(x) {
  strsplit(Sys.getenv(x, unset = ""), .Platform$path.sep, fixed = TRUE)[[1]]
}

setenv <- function(...) {
  dots <- list(...)

  # validate argument length
  n <- length(dots)
  if (n %% 2 != 0)
    stop("expected even number of arguments to 'setenv'")

  # extract keys, values from '...'
  indices <- seq(1, length(dots), by = 2)
  keys <- dots[indices]
  vals <- dots[indices + 1]

  # construct call to Sys.setenv
  names(vals) <- keys
  vals <- lapply(vals, function(val) {
    paste(val, collapse = .Platform$path.sep)
  })
  do.call(Sys.setenv, vals)
}

unsetenv <- function(name) {
  Sys.unsetenv(name)
}
