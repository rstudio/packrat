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
    normalizePath(project, winslash = "/", mustWork = TRUE)
  )
}

getPackratDir <- function(project = NULL) {
  project <- getProjectDir(project)
  file.path(project, "packrat")
}

# We differentiate between the 'libDir' -- the actual architecture-specific
# directory containing libraries for the current system, and the 'libraryRootDir'
# containing all libraries for a given project (which may want to be copied around
# -- unlikely since we encourage people to build from snapshots, but we leave it
# possible)
libDir <- function(project = NULL) {

  envLibDir <- Sys.getenv("R_PACKRAT_LIB_DIR", unset = NA)
  if (!is.na(envLibDir))
    return(envLibDir)

  project <- getProjectDir(project)
  file.path(
    libraryRootDir(project),
    R.version$platform,
    getRversion()
  )
}

relLibDir <- function() {
  file.path("packrat", relativeLibDir("lib"))
}

## The root library directory for a project, e.g. <proj>/<packrat>/lib
libraryRootDir <- function(project = NULL) {
  project <- getProjectDir(project)
  file.path(project, "packrat/lib")
}

relLibraryRootDir <- function() {
  "packrat/lib"
}

relativeLibDir <- function(libraryRoot) {
  file.path(libraryRoot, R.version$platform, getRversion())
}

# Temporary library directory when modifying an in-use library
newLibraryDir <- function(project = NULL) {
  file.path(getPackratDir(project), "library.new")
}

relNewLibraryDir <- function() {
  file.path("packrat", "library.new")
}

oldLibraryDir <- function(project = NULL) {
  file.path(getPackratDir(project), "library.old")
}

relOldLibraryDir <- function() {
  "packrat/library.old"
}

srcDir <- function(project = NULL) {
  file.path(getPackratDir(project), "src")
}

relSrcDir <- function() {
  "packrat/src"
}

bundlesDir <- function(project = NULL) {
  file.path(getPackratDir(project), "bundles")
}

lockFilePath <- function(project = NULL) {
  file.path(getPackratDir(project), "packrat.lock")
}

snapshotLockFilePath <- function(project = NULL) {
  file.path(getPackratDir(project), "snapshot.lock")
}

## Use a file instead of env. variables as it"s better handled across
## multiple R sessions
packratModeFilePath <- function(project = NULL) {
  packratDir <- getPackratDir(project)
  file.path(packratDir, "packrat.mode")
}

instInitFilePath <- function() {
  file.path(system.file(package = "packrat"), "resources/init.R")
}

instInitRprofileFilePath <- function() {
  file.path(system.file(package = "packrat"), "resources/init-rprofile.R")
}

instMacRUserlibFilePath <- function() {
  file.path(system.file(package = "packrat"), "resources/mac_r_userlib.sh")
}

packratOptionsFilePath <- function(project = NULL) {
  file.path(getPackratDir(project), "packrat.opts")
}

libRdir <- function(project = NULL) {
  file.path(getPackratDir(project), "lib-R")
}

libExtDir <- function(project = NULL) {
  file.path(getPackratDir(project), "lib-ext")
}

startsWithBytes <- function(x, y) {
  Encoding(x) <- Encoding(y) <- "bytes"
  return(substring(x, 1, nchar(y, type = "bytes")) == y)
}

prettyDir <- function(project = NULL, ...) {
  project <- getProjectDir(project)
  homeDir <- path.expand("~/")
  if (startsWithBytes(project, homeDir))
    project <- gsub(homeDir, "~/", project, fixed = TRUE)
  file.path(project, ...)
}

prettyProjectDir <- function(project = NULL) {
  prettyDir(project = project)
}

prettyLibDir <- function(project = NULL) {
  prettyDir(project = project, "packrat/lib")
}

#' @rdname packrat-external
#' @name packrat-external
#' @export
user_lib <- function() {
  libraries <- unlist(strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep, fixed = TRUE))
  normalizePath(libraries, winslash = "/", mustWork = FALSE)
}

userLib <- user_lib

#' @rdname packrat-external
#' @name packrat-external
#' @export
packrat_lib <- function() {
  project <- getProjectDir()
  libDir(project)
}

## A location where "global" packrat data is stored, e.g. the library cache
appDataDir <- function() {
  Sys.getenv("R_PACKRAT_CACHE_DIR",
             unset = defaultAppDataDir())
}

defaultAppDataDir <- function() {

  # borrowed and modified from shinyapps

  # get the home directory from the operating system (in case
  # the user has redefined the meaning of ~) but fault back
  # to ~ if there is no HOME variable defined
  homeDir <- Sys.getenv("HOME", unset = "~")

  # determine application config dir (platform specific)
  if (is.windows())
    appDataDirBase <- Sys.getenv("APPDATA")
  else if (is.mac())
    appDataDirBase <- file.path(homeDir, "Library/Application Support")
  else
    appDataDirBase <- Sys.getenv("XDG_CONFIG_HOME", file.path(homeDir, ".config"))

  # normalize path
  appDataDir <- normalizePath(file.path(appDataDirBase, "packrat"),
                              mustWork = FALSE)

  # return it
  appDataDir

}

packratCacheVersion <- function() {
  "v2"
}

cacheLibDir <- function(...) {
  file.path(appDataDir(), packratCacheVersion(), "library", ...)
}
