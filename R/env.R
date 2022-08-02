# Tools for getting info about the execution environment

tar_binary <- function() {
  # If TAR is specified in the environment, use that.
  tar <- Sys.getenv("TAR", unset = NA)
  if (!is.na(tar)) {
    return(tar)
  }

  # If we're on Unix, look for a tar binary on the PATH.
  if (is.unix()) {
    tar <- file.path(Sys.which("tar"))
    if (file.exists(tar)) {
      return(tar)
    }
  }

  # If we're on Windows, look for the system tar binary.
  if (is.windows()) {
    root <- Sys.getenv("SystemRoot", unset = NA)
    if (is.na(root)) {
      root <- "C:/Windows"
    }
    tar <- file.path(root, "System32/tar.exe")
    if (file.exists(tar)) {
      return(tar)
    }
  }

  # Return internal only as a fallback with a warning.
  warning("No external tar binary found. Using R's internal TAR, which may cause failures with long filenames.")
  return("internal")
}

# Tools for storing state in environment variables. Possibly unused.
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
