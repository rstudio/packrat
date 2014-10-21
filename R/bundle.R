#' Bundle a Packrat Project
#'
#' Bundle a packrat project, for easy sharing.
#'
#' The project is bundled as a gzipped tarball (\code{.tar.gz}), which can
#' be unbundled either with \code{packrat::\link{unbundle}} (which
#' restores the project as well), \R's own \code{\link{untar}}, or
#' through most system \code{tar} implementations.
#'
#' @param project The project directory. Defaults to the currently activate
#'  project. By default, the current project active under \code{packratMode}
#'  is checked.
#' @param file The path to write the bundle. By default, we write
#'  the bundle to \code{packrat/bundles/<project>-<date>.tar.gz}, with
#'  \code{<date>} as returned by \code{Sys.date()}.
#' @param include.src Include the packrat sources?
#' @param include.lib Include the packrat private library?
#' @param include.bundles Include other packrat bundle tarballs
#'  (as in \code{packrat/bundles/})?
#' @param include.vcs.history Include version control history (ie, \code{.git/}
#'  or \code{.svn/} folders)?
#' @param overwrite Boolean; overwrite the file at \code{file} if it already exists?
#' @param ... Optional arguments passed to \code{\link{tar}}.
#' @export
#' @return The path (invisibly) to the bundled project.
bundle <- function(project = NULL,
                   file = NULL,
                   include.src = TRUE,
                   include.lib = FALSE,
                   include.bundles = TRUE,
                   include.vcs.history = FALSE,
                   overwrite = FALSE,
                   ...) {

  TAR <- Sys.getenv("TAR")
  if (identical(TAR, "internal") || identical(TAR, "")) {
    bundle_internal(project = project,
                    file = file,
                    include.src = include.src,
                    include.lib = include.lib,
                    include.bundles = include.bundles,
                    include.vcs.history = include.vcs.history,
                    overwrite = overwrite,
                    ...)
  } else {
    bundle_external(project = project,
                    file = file,
                    include.src = include.src,
                    include.lib = include.lib,
                    include.bundles = include.bundles,
                    include.vcs.history = include.vcs.history,
                    overwrite = overwrite,
                    ...)
  }

}

bundle_internal <- function(project = NULL,
                            file = NULL,
                            include.src = TRUE,
                            include.lib = FALSE,
                            include.bundles = TRUE,
                            include.vcs.history = TRUE,
                            overwrite = FALSE,
                            ...) {

  project <- getProjectDir(project)
  stopIfNotPackified(project)

  # Make sure we're in the project dir so relative paths are properly set
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(project)

  # If file is NULL, write to a local file with the current date
  if (is.null(file)) {
    tarName <- paste(basename(project), Sys.Date(), sep = "-")
    tarName <- paste(tarName, ".tar.gz", sep = "")
    bundlesDir <- bundlesDir(project)
    if (!file.exists(bundlesDir)) {
      dir.create(bundlesDir, recursive = TRUE)
    }
    file <- file.path(bundlesDir(project), tarName)
  }

  file <- file.path(
    normalizePath(dirname(file), mustWork = FALSE),
    basename(file)
  )

  if (file.exists(file) && !overwrite) {
    stop("A file already exists at file location '", file, "'.")
  }

  # Regex negation patterns that we use to selectively leave some items out
  pattern <- c(
    "^(?!\\.Rproj\\.user)",
    if (!include.src) "^(?!packrat/src/)",
    if (!include.lib) "^(?!packrat/lib.*)",
    if (!include.bundles) "^(?!packrat/bundles/)",
    if (!include.vcs.history) "^(?!\\.(git|svn))"
  )

  ## Make sure the base folder name is inheritted from the project name
  ##
  ## The internal version of 'tar' used by R on Windows fails if one tries to include
  ## a file in a sub-directory, without actually including that subdirectory.
  ## To work around this, we are forced to copy the files we want to tar to
  ## a temporary directory, and then tar that.
  dir_copy(
    from = getwd(),
    to = file.path(tempdir(), basename(project)),
    pattern = pattern,
    overwrite = TRUE
  )

  ## Clean up after ourselves
  on.exit(unlink(file.path(tempdir(), basename(project))), add = TRUE)

  ## Now bundle up that copied directory, from the tempdir path
  setwd(tempdir())
  result <- tar(
    tarfile = file,
    files = basename(project),
    compression = "gzip",
    tar = Sys.getenv("TAR"),
    ...
  )

  if (result != 0) {
    stop("Failed to bundle the packrat project.")
  }
  message("The packrat project has been bundled at:\n- \"", file, "\"")
  invisible(file)
}

bundle_external <- function(project = NULL,
                   file = NULL,
                   include.src = TRUE,
                   include.lib = FALSE,
                   include.bundles = TRUE,
                   include.vcs.history = TRUE,
                   overwrite = FALSE,
                   ...) {

  project <- getProjectDir(project)
  stopIfNotPackified(project)

  # If file is NULL, write to a local file with the current date
  if (is.null(file)) {
    tarName <- paste(basename(project), Sys.Date(), sep = "-")
    tarName <- paste(tarName, ".tar.gz", sep = "")
    bundlesDir <- bundlesDir(project)
    if (!file.exists(bundlesDir)) {
      dir.create(bundlesDir, recursive = TRUE)
    }
    file <- file.path(bundlesDir(project), tarName)
  }

  file <- file.path(
    normalizePath(dirname(file), mustWork = FALSE),
    basename(file)
  )

  # Make sure we're in the project dir so relative paths are properly set
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(project)

  # Blacklist certain files / folders
  blackList <- c(
    "^\\.Rproj\\.user"
  )

  # Collect all files and folders we want to zip up. We list all
  # files and directories within the project root, excluding packrat
  # -- which we add back in piecemeal later.
  projectFiles <- list.files(all.files = TRUE, no.. = TRUE)
  for (item in blackList) {
    projectFiles <- grep(item, projectFiles, perl = TRUE, invert = TRUE, value = TRUE)
  }

  # Exclude the packrat folder at this stage -- we re-add the components we
  # need piece by piece
  projectFiles <- projectFiles[
    !startswith(projectFiles, .packrat$packratFolderName)
  ]

  # Make sure we add packrat
  basePackratFiles <- list_files(
    .packrat$packratFolderName,
    all.files = TRUE,
    recursive = FALSE,
    full.names = TRUE
  )

  filesToZip <- c(projectFiles, basePackratFiles)

  # These need to be relative paths
  if (include.src) {
    filesToZip <- c(filesToZip, relSrcDir())
  }

  if (include.lib) {
    filesToZip <- c(filesToZip, relLibDir())
  }

  if (!include.vcs.history) {
    filesToZip <- grep("^\\.(git|svn)", filesToZip, invert = TRUE, value = TRUE)
  }

  if (file.exists(file) && !overwrite) {
    stop("A file already exists at file location '", file, "'.")
  }

  ## Make sure the base folder name is inheritted from the project name
  ## NOTE: logic earlier sends us back to original directory
  setwd("../")
  result <- tar(file,
                files = file.path(basename(project), filesToZip),
                compression = "gzip",
                tar = Sys.getenv("TAR"),
                ...)

  if (result != 0) {
    stop("Failed to bundle the packrat project.")
  }

  message("The packrat project has been bundled at:\n- \"", file, "\"")
  invisible(file)

}

##' Unbundle a Packrat Project
##'
##' Unbundle a previously \code{\link{bundle}}d project.
##'
##' @param bundle Path to the bundled file.
##' @param where The directory where we will unbundle the project.
##' @param ... Optional arguments passed to \code{\link{tar}}.
##' @param restore Boolean; should we \code{\link{restore}} the library
##'   after \code{unbundle}-ing the project?
##' @export
unbundle <- function(bundle, where, ..., restore = TRUE) {

  bundle <- normalizePath(bundle, winslash = "/", mustWork = TRUE)
  if (!file.exists(where) && is_dir(where)) {
    dir.create(where, recursive = TRUE)
  }
  where <- normalizePath(where, winslash = "/", mustWork = TRUE)

  ## Get the list of files in the output directory -- we diff against it
  ## to figure out what the top-level directory name is
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(where)

  whereFiles <- list.files()
  message("- Untarring '", basename(bundle), "' in directory '", where, "'...")
  untar(bundle, exdir = where, ...)
  dirName <- normalizePath(setdiff(list.files(), whereFiles), winslash = "/", mustWork = TRUE)

  if (restore) {
    setwd(dirName)
    if (length(dirName) != 1) {
      stop("Couldn't infer top-level directory name; cannot perform automatic restore")
    }
    ## Ensure the (empty) library directory is present before restoring
    dir.create(libDir(getwd()), recursive = TRUE, showWarnings = FALSE)
    message("- Restoring project library...")
    restore(project = getwd(), restart = FALSE)
    message("Done! The project has been unbundled and restored at:\n- \"", dirName, "\"")
  } else {
    message("Done! The packrat project has been unbundled at:\n- \"", dirName, "\"")
  }

  invisible(dirName)

}
