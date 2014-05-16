## Different paths that are used for a packrat project

getProjectDir <- function(projDir = NULL) {
  if (is.null(projDir)) projDir <- .packrat$projectDir
  file.path(
    normalizePath(projDir, winslash = '/', mustWork = TRUE)
  )
}

## We differentiate between the 'libDir' -- the actual architecture-specific
## directory containing libraries for the current system, and the 'libraryRootDir'
## containing all libraries for a given project (which may want to be copied around
## -- unlikely since we encourage people to build from snapshots, but we leave it
## possible)
libDir <- function(projDir = NULL) {
  projDir <- getProjectDir(projDir)
  file.path(
    libraryRootDir(projDir),
    R.version$platform,
    getRversion()
  )
}

## The root library directory for a project, e.g. <proj>/<packrat>/lib
libraryRootDir <- function(projDir = NULL) {
  projDir <- getProjectDir(projDir)
  file.path(
    projDir,
    .packrat$packratFolderName,
    'lib'
  )
}

relativeLibDir <- function(libraryRoot) {
  file.path(
    libraryRoot,
    R.version$platform,
    getRversion()
  )
}

# Temporary library directory when modifying an in-use library
newLibraryDir <- function(projDir = NULL) {
  projDir <- getProjectDir(projDir)
  file.path(
    projDir,
    .packrat$packratFolderName,
    'library.new'
  )
}

oldLibraryDir <- function(projDir = NULL) {
  projDir <- getProjectDir(projDir)
  file.path(
    projDir,
    .packrat$packratFolderName,
    'library.old'
  )
}

srcDir <- function(projDir = NULL) {
  projDir <- getProjectDir(projDir)
  file.path(
    projDir,
    .packrat$packratFolderName,
    'src'
  )
}

bundlesDir <- function(projDir = NULL) {
  projDir <- getProjectDir(projDir)
  file.path(
    projDir,
    .packrat$packratFolderName,
    'bundles'
  )
}

lockFilePath <- function(projDir) {
  file.path(projDir, .packrat$packratFolderName, "packrat.lock")
}
