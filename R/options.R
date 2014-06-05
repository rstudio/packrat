VALID_OPTIONS <- list(
  auto.snapshot = list(TRUE, FALSE)
)

read.options <- function(project = NULL) {
  path <- packratOptionsFilePath(project)
  if (!file.exists(path)) return(invisible(NULL))
  opts <- readDcf(path)
  as.list(apply(opts, 2, function(x) {
    eval(parse(text = x))
  }))
}

packrat.getOption <- function(option) {
  read.options()[[option]]
}

packrat.setOption <- function(...) {
  dots <- list(...)
  validateOptions(dots)
  keys <- names(dots)
  values <- unname(unlist(dots))
  opts <- read.options()
  for (i in seq_along(keys)) {
    opts[[keys[[i]]]] <- values[[i]]
  }
  write.dcf(opts, file = packratOptionsFilePath())
  invisible(TRUE)
}

validateOptions <- function(opts) {
  for (i in seq_along(opts)) {
    key <- names(opts)[[i]]
    value <- opts[[i]]
    if (!(key %in% names(VALID_OPTIONS))) {
      stop("'", key, "' is not a valid packrat option")
    }
    if (!(value %in% VALID_OPTIONS[[key]])) {
      stop("'", value, "' is not a valid setting for packrat option '", key, "'")
    }
  }
}
