#' Bundle a Packrat Project
#'
#' Bundle a packrat project, for easy sharing.
#'
#' The project is bundled as a gzipped tarball (\code{.tar.gz}), which can
#' be unbundled either with \code{packrat::\link{unbundle}} (which
#' restores the project as well), \R's own \code{utils::\link{untar}}, or
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
#' @param omit.cran.src Boolean; when \code{TRUE}, packages whose sources can
#'  be retrieved from CRAN are excluded from the bundle.
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
                   omit.cran.src = FALSE,
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
  bundlePath <- file.path(tempdir(), "packrat-bundles")
  from <- getwd()
  to <- file.path(bundlePath, basename(project))
  dir_copy(
    from = from,
    to = to,
    pattern = pattern,
    overwrite = TRUE
  )

  ## Clean up after ourselves
  on.exit(unlink(to, recursive = TRUE), add = TRUE)

  ## Remove any CRAN packages if 'omit.cran.src' was specified.
  if (omit.cran.src) {
    lockfile <- readLockFilePackages(lockFilePath(project))
    pkgs <- vapply(lockfile, `[[`, FUN.VALUE = character(1), "name", USE.NAMES = FALSE)
    isCRAN <- vapply(lockfile, FUN.VALUE = logical(1), function(x) {
      x[["source"]] == "CRAN"
    })
    cranPkgs <- pkgs[isCRAN]
    srcPkgs <- list.files(
      srcDir(project = to),
      full.names = TRUE,
      recursive = TRUE
    )

    for (srcPkg in srcPkgs) {

      isPathToCranPkg <- any(unlist(lapply(cranPkgs, function(cranPkg) {
        grepl(cranPkg, basename(srcPkg))
      })))

      if (isPathToCranPkg) {
        unlink(srcPkg)
      }

    }

  }

  ## Now bundle up that copied directory, from the tempdir path
  setwd(bundlePath)
  result <- tar(
    tarfile = file,
    files = basename(project),
    compression = "gzip",
    tar = "internal",
    ...
  )

  if (result != 0) {
    stop("Failed to bundle the packrat project.")
  }
  message("The packrat project has been bundled at:\n- \"", file, "\"")
  invisible(file)
}

extractProjectNameFromBundlePath <- function(bundlePath) {
  bundleBasename <- basename(bundlePath)
  reDate <- "^(.*?)-\\d{4}-\\d{2}-\\d{2}\\.tar\\.gz$"
  if (grepl(reDate, bundleBasename, perl = TRUE))
    gsub(reDate, "\\1", bundleBasename, perl = TRUE)
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

  # Ensure that the directory we'll be creating doesn't already exist.
  # It's possible that people will have renamed the bundles, so make
  # this a no-op on error.
  projectName <- extractProjectNameFromBundlePath(bundle)
  if (!is.null(projectName) && file.exists(file.path(where, projectName)))
    stop("Path '", file.path(where, projectName), "' already exists!")

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
