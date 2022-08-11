is.windows <- function() {
  .Platform$OS.type == "windows"
}

is.unix <- function() {
  .Platform$OS.type == "unix"
}

is.mac <- function() {
  Sys.info()["sysname"] == "Darwin"
}

is.linux <- function() {
  Sys.info()["sysname"] == "Linux"
}
