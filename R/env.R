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

# for autocompletion
.packrat.env <- list(
  "R_PACKRAT_PROJECT_DIR",
  "R_PACKRAT_SRC_DIR",
  "R_PACKRAT_LIB_DIR",
  "R_PACKRAT_CACHE_DIR",
  "R_PACKRAT_DEFAULT_LIBPATHS",
  "R_PACKRAT_SYSTEM_LIBRARY",
  "R_PACKRAT_SITE_LIBRARY"
)
names(.packrat.env) <- unlist(.packrat.env)
