
renv <- new.env()

loadRenv <- function() {
  script <- system.file("resources/renv.R", package = "packrat")
  sys.source(script, envir = renv)
}
