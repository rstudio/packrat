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

# Although a project's resources might live in a separate directory,
# the packrat folder is always a sub-directory of the project (ie --
# packrat.lock, packrat.opts)
getPackratDir <- function(project = NULL) {
  project <- getProjectDir(project)
  file.path(project, "packrat")
}

## We differentiate between the 'libDir' -- the actual architecture-specific
## directory containing libraries for the current system, and the 'libraryRootDir'
## containing all libraries for a given project (which may want to be copied around
## -- unlikely since we encourage people to build from snapshots, but we leave it
## possible)
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

resourcesDir <- function(..., project = NULL) {
  project <- getProjectDir(project)
  optDir <- opts$project.resources.path()
  if (is.null(optDir))
    file.path(getPackratDir(project), ...)
  else
    file.path(optDir, basename(project), ...)
}

## The root library directory for a project, e.g. <proj>/<packrat>/lib
libraryRootDir <- function(project = NULL) {
  resourcesDir("lib", project = project)
}

# Temporary library directory when modifying an in-use library
newLibraryDir <- function(project = NULL) {
  resourcesDir("library.new", project = project)
}

oldLibraryDir <- function(project = NULL) {
  resourcesDir("library.old", project = project)
}

srcDir <- function(project = NULL) {
  resourcesDir("src", project = project)
}

bundlesDir <- function(project = NULL) {
  resourcesDir("bundles", project = project)
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

packratOptionsFilePath <- function(project = NULL) {
  file.path(getPackratDir(project), "packrat.opts")
}

libRdir <- function(project = NULL) {
  resourcesDir("lib-R", project = project)
}

libExtDir <- function(project = NULL) {
  resourcesDir("lib-ext", project = project)
}

startsWithBytes <- function(x, y) {
  Encoding(x) <- Encoding(y) <- "bytes"
  return(substring(x, 1, nchar(y, type = "bytes")) == y)
}

prettyLibDir <- function(project = NULL) {
  project <- getProjectDir(project)
  resDir <- resourcesDir(project = project)
  homeDir <- path.expand("~/")
  if (startsWithBytes(resDir, homeDir))
    resDir <- gsub(homeDir, "~/", resDir, fixed = TRUE)
  file.path(resDir, "lib")
}

prettyProjectDir <- function(project = NULL) {
  project <- getProjectDir(project)
  homeDir <- path.expand("~/")
  if (startsWithBytes(project, homeDir))
    project <- gsub(homeDir, "~/", project, fixed = TRUE)
  project
}


##' @rdname packrat-external
##' @name packrat-external
##' @export
user_lib <- function() {
  libraries <- unlist(strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep, fixed = TRUE))
  normalizePath(libraries, winslash = "/", mustWork = FALSE)
}

userLib <- user_lib

##' @rdname packrat-external
##' @name packrat-external
##' @export
packrat_lib <- function() {
  project <- getProjectDir()
  libDir(project)
}

## A location where 'global' packrat data is stored, e.g. the library cache
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
  "v1"
}

cacheLibDir <- function(...) {
  file.path(appDataDir(), packratCacheVersion(), "library", ...)
}
