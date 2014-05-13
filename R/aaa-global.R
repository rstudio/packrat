.packrat <- new.env(parent = emptyenv())

# Settings seen when packrat is loaded (used when turning packrat mode on and off)
.onLoad <- function(libname, pkgname) {
  .packrat$packratFolderName <- ".packrat"
  .packrat$promptOnLoad <- getOption("prompt")
  .packrat$origLibPaths <- .libPaths()
}
