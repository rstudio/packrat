## Different paths that are used for a packrat project

getProjectDir <- function(project = NULL) {

  ## If project is NULL, and .packrat$project is NULL, then we should look
  ## in the current working directory
  cachedDir <- .packrat_mutables$get("project")
  if (is.null(project)) {
    if (is.null(cachedDir)) {
      project <- getwd()
    } else {
      project <- cachedDir
    }
  }

  file.path(
    normalizePath(project, winslash = '/', mustWork = TRUE)
  )
}

getPackratDir <- function(project = NULL) {
  project <- getProjectDir(project)
  file.path(
    project,
    .packrat$packratFolderName
  )
}

## We differentiate between the 'libDir' -- the actual architecture-specific
## directory containing libraries for the current system, and the 'libraryRootDir'
## containing all libraries for a given project (which may want to be copied around
## -- unlikely since we encourage people to build from snapshots, but we leave it
## possible)
libDir <- function(project = NULL) {
  project <- getProjectDir(project)
  file.path(
    libraryRootDir(project),
    R.version$platform,
    getRversion()
  )
}

relLibDir <- function() {
  file.path(
    .packrat$packratFolderName,
    relativeLibDir('lib')
  )
}

## The root library directory for a project, e.g. <proj>/<packrat>/lib
libraryRootDir <- function(project = NULL) {
  project <- getProjectDir(project)
  file.path(
    project,
    .packrat$packratFolderName,
    'lib'
  )
}

relLibraryRootDir <- function() {
  file.path(
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
newLibraryDir <- function(project = NULL) {
  file.path(
    getPackratDir(project),
    'library.new'
  )
}

relNewLibraryDir <- function() {
  file.path(
    .packrat$packratFolderName,
    'library.new'
  )
}

oldLibraryDir <- function(project = NULL) {
  file.path(
    getPackratDir(project),
    'library.old'
  )
}

relOldLibraryDir <- function() {
  file.path(
    .packrat$packratFolderName,
    'library.old'
  )
}

srcDir <- function(project = NULL) {
  file.path(
    getPackratDir(project),
    'src'
  )
}

relSrcDir <- function() {
  file.path(
    .packrat$packratFolderName,
    'src'
  )
}

bundlesDir <- function(project = NULL) {
  file.path(
    getPackratDir(project),
    'bundles'
  )
}

lockFilePath <- function(project = NULL) {
  file.path(getPackratDir(project), "packrat.lock")
}

snapshotLockFilePath <- function(project = NULL) {
  file.path(getPackratDir(project), "snapshot.lock")
}

## Use a file instead of env. variables as it's better handled across
## multiple R sessions
packratModeFilePath <- function(project = NULL) {
  packratDir <- getPackratDir(project)
  file.path(packratDir, "packrat.mode")
}

instInitFilePath <- function() {
  file.path(system.file(package = "packrat"), "resources", "init.R")
}

instInitRprofileFilePath <- function() {
  file.path(system.file(package = "packrat"), "resources", "init-rprofile.R")
}

instMacRUserlibFilePath <- function() {
  file.path(system.file(package = "packrat"), "resources", "mac_r_userlib.sh")
}

packratOptionsFilePath <- function(project = NULL) {
  file.path(getPackratDir(project), "packrat.opts")
}

libRdir <- function(project = NULL) {
  file.path(getPackratDir(project), "lib-R")
}

prettyLibDir <- function(project) {
  homeDir <- path.expand("~")
  if (substring(project, 1, nchar(homeDir)) == homeDir)
    project <- gsub(homeDir, "~", project, fixed = TRUE)
  file.path(project, .packrat$packratFolderName, "lib")
}
