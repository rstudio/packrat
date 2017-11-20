#' Paths to Packrat Resources
#'
#' These functions provide a mechanism for retrieving the paths to
#' Packrat resource directories. Each of these directories can be
#' overridden by setting either an environment variable, or an \R
#' option.
#'
#' @section Project Directory:
#'
#' \code{project_dir()} is special -- the \code{R_PACKRAT_PROJECT_DIR}
#' environment variable is set and unset by \code{\link{on}} and
#' \code{\link{off}}, respectively, and generally should not be
#' overridden by the user.
#'
#' @section Directory Resolution:
#'
#' The following table shows the order in which resource directories
#' are discovered (from left to right). The first non-empty result is
#' used.
#'
#' \tabular{llll}{
#' \strong{API}         \tab \strong{Environment Variable} \tab \strong{R Option}          \tab \strong{Default Value} \cr
#' \code{project_dir()} \tab \code{R_PACKRAT_PROJECT_DIR}  \tab \code{packrat.project.dir} \tab \code{getwd()} \cr
#' \code{src_dir()}     \tab \code{R_PACKRAT_SRC_DIR}      \tab \code{packrat.src.dir}     \tab \code{"packrat/src"} \cr
#' \code{lib_dir()}     \tab \code{R_PACKRAT_LIB_DIR}      \tab \code{packrat.lib.dir}     \tab \code{"packrat/lib"} \cr
#' \code{bundles_dir()} \tab \code{R_PACKRAT_BUNDLES_DIR}  \tab \code{packrat.bundles.dir} \tab \code{"packrat/bundles"} \cr
#' \emph{(none)}        \tab \code{R_PACKRAT_LIB_R_DIR}    \tab \code{packrat.lib-r.dir}   \tab \code{"packrat/lib-R"} \cr
#' \emph{(none)}        \tab \code{R_PACKRAT_LIB_EXT_DIR}  \tab \code{packrat.lib-ext.dir} \tab \code{"packrat/lib-ext"} \cr
#' }
#'
#' @param project The project directory.
#' @rdname packrat-resources
#' @name packrat-resources
NULL

#' @rdname packrat-resources
#' @export
project_dir <- function(project = NULL) {
  getProjectDir(project = project)
}

#' @rdname packrat-resources
#' @export
src_dir <- function(project = NULL) {
  srcDir(project = project)
}

#' @rdname packrat-resources
#' @export
lib_dir <- function(project = NULL) {
  libDir(project = project)
}

#' @rdname packrat-resources
#' @export
bundles_dir <- function(project = NULL) {
  bundlesDir(project = project)
}

# Internal Implementations ----

getProjectDir <- function(project = NULL) {

  if (!is.null(project) && length(project) > 0)
    return(normalizePath(project, winslash = "/", mustWork = TRUE))

  packratOption(
    "R_PACKRAT_PROJECT_DIR",
    "packrat.project.dir",
    if (length(getwd()) > 0) normalizePath(getwd(), winslash = "/", mustWork = TRUE) else ""
  )
}

getPackratDir <- function(project = NULL) {
  project <- getProjectDir(project)
  file.path(project, "packrat")
}

platformRelDir <- function() {
  file.path(R.version$platform, getRversion())
}

libDir <- function(project = NULL) {
  packratOption(
    "R_PACKRAT_LIB_DIR",
    "packrat.lib.dir",
    file.path(libraryRootDir(project), platformRelDir())
  )
}

libRdir <- function(project = NULL) {
  packratOption(
    "R_PACKRAT_LIB_R_DIR",
    "packrat.lib-r.dir",
    file.path(getPackratDir(project), "lib-R", platformRelDir())
  )
}

libExtDir <- function(project = NULL) {
  packratOption(
    "R_PACKRAT_LIB_EXT_DIR",
    "packrat.lib-ext.dir",
    file.path(getPackratDir(project), "lib-ext", platformRelDir())
  )
}

srcDir <- function(project = NULL) {
  packratOption(
    "R_PACKRAT_SRC_DIR",
    "packrat.src.dir",
    file.path(getPackratDir(project), "src")
  )
}

bundlesDir <- function(project = NULL) {
  packratOption(
    "R_PACKRAT_BUNDLES_DIR",
    "packrat.bundles.dir",
    file.path(getPackratDir(project), "bundles")
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

relSrcDir <- function() {
  "packrat/src"
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
  packratOption(
    "R_PACKRAT_CACHE_DIR",
    "packrat.cache.dir",
    defaultAppDataDir()
  )
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

untrustedCacheLibDir <- function(...) {
  file.path(appDataDir(), packratCacheVersion(), "library-client", ...)
}
