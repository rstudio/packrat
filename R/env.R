# Tools for storing state in environment variables.
escape <- function(x, what = "\"") {
  from <- what
  to <- paste("\\", what, sep = "")
  gsub(from, to, x, fixed = TRUE)
}

unescape <- function(x, what = "\"") {
  from <- paste("\\", what, sep = "")
  to <- what
  gsub(from, to, x, fixed = TRUE)
}

envparse <- function(string) {
  parsed <- parse(text = paste("dummy(", string, ")"))[[1]]
  if (length(parsed) == 1)
    character()
  else
    as.character(parsed[2:length(parsed)])
}

getenv <- function(x, unset = "") {

  env <- Sys.getenv(x, unset = NA)
  if (is.na(env))
    return(unset)

  envparse(env)
}

setenv <- function(name, value, overwrite = TRUE) {
  if (!overwrite && !is.na(Sys.getenv(name, unset = NA)))
    return(FALSE)
  value <- paste(surround(escape(value, "\""), "\""), collapse = ",")
  call <- list(value)
  names(call) <- name
  do.call(Sys.setenv, call)
  return(TRUE)
}

unsetenv <- function(name) {
  Sys.unsetenv(name)
}

# for autocompletion
.packrat.env <- list(
  "R_PACKRAT_PROJECT_DIR",
  "R_PACKRAT_DEFAULT_LIBPATHS",
  "R_PACKRAT_LIBRARY",
  "R_PACKRAT_LIBRARY_SITE",
  "R_PACKRAT_PKGTYPE"
)
names(.packrat.env) <- unlist(.packrat.env)
