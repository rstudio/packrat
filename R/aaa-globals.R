.packrat <- new.env(parent = emptyenv())

## The location where we place all packrat-related files
.packrat$packratFolderName <- "packrat"

# Settings seen when packrat is loaded
# These will also be modified when entering and exiting packrat_mode
.onLoad <- function(libname, pkgname) {

  ## The project directory is checked with 'getProjectDir()'; first it checks
  ## this value; if it is NULL then the current directory is returned
  .packrat$projectDir <- NULL

  ## Used for modifying the prompt; instructing a user that they are in packrat mode
  .packrat$promptOnLoad <- getOption("prompt")

  ## Save the original library path
  .packrat$origLibPaths <- .libPaths()

  ## Check if we need migration
  checkNeedsLibraryMigration()
}
