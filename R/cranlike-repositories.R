#' Create a Local, CRAN-like Repository
#'
#' Generate a local CRAN-like repository which can be
#' used to store and distribute \R packages.
#'
#' @param path Path to a local CRAN-like repository.
#' @param name The name to assign to the repository. Defaults to the
#'   directory name in which the reopsitory is created.
#' @param add Add this new repository to the current set of repositories?
#'
#' @export
repos_create <- function(path, name = basename(path), add = TRUE) {

  if (file.exists(path))
    stop("Path '", path, "' is not empty; cannot create ",
         "repository at this location", call. = FALSE)

  if (name %in% names(getOption("repos")))
    stop("A repository named '", name, "' is already registered!")

  dir.create(path, recursive = TRUE)
  root <- normalize.path(path)

  ## Create the 'contrib' dirs

  # Create the 'src' dir and write PACKAGES
  srcContribeDir <- file.path(root, "src", "contrib")
  dir.create(srcContribeDir, recursive = TRUE)
  tools::write_PACKAGES(srcContribeDir, type = "source")

  # Create the 'Archive' directory
  createArchiveDir(srcContribeDir)

  # Create the 'bin' dirs and write PACKAGES
  binContribDirs <- binContribDirs(root)
  lapply(binContribDirs, function(dirs) {
    lapply(dirs, function(dir) {
      dir.create(dir, recursive = TRUE)
      type <- if (grepl("/bin/windows/", dir))
        "win.binary"
      else if (grepl("/bin/macosx/", dir))
        "mac.binary"
      else
        "source"
      tools::write_PACKAGES(dir, type = type)
    })
  })

  message("Local CRAN repository '", name, "' created at: ",
          "\n- ", shQuote(normalize.path(path)))

  URI <- paste(filePrefix(), root, sep = "")
  names(URI) <- name

  if (add)
    options(repos = c(getOption("repos"), URI))

  URI
}

## createArchiveDir
## @description Create the archive dir to store old package version
## @param srcContribeDir Path to the src/contrib directory
## @param packageName Name of the package to add to archive (if null only create the "Archive" directory)
## @return string The path of the archive directory create
createArchiveDir <- function(srcContribeDir, packageName=NULL){
  name <- file.path(srcContribeDir, if(is.null(packageName)) "Archive" else file.path("Archive", packageName))
  dir.create(name, recursive = TRUE, showWarnings = FALSE)
  return(name)
}

## @name getPackageName
## @description Get the package name by its path
## @param packagePath Path to the package
## @return string The name of the package, without version
getPackageName <- function(packagePath){
  return(sub("_.*", "", basename(packagePath)))
}

## @name getReposrcContribeDir
## @description Get the path to the srcContribeDir
## @param repoPath Path to the repository
## @return string Path to the srcContribeDir of the repository
getReposrcContribeDir <- function(repoPath){
  return(normalize.path(sub(reFilePrefix(), "", file.path(repoPath, "src", "contrib"))))
}

## @name pushOldPackagesToArchive
## @description If a version of the package is already in the repository, move it to the archive
## @param packageName The name of the package
## @param srcContribeDir Path to the srcContribeDir
## @param quiet Be quiet
pushOldPackagesToArchive <- function(package, srcContribeDir, quiet = FALSE){
  # Get the name of the package
  packageName <- getPackageName(package)

  # Search old version of package in srcContribeDir
  oldPackages <- list.files(srcContribeDir, pattern=paste0(packageName, "_"))

  # Create the package directory in Archive
  packageArchiveDir <- createArchiveDir(srcContribeDir = srcContribeDir, packageName = packageName)

  # Move old package to Archive directory
  for(pkg in oldPackages){
    # Don't move the new version of the package
    if(pkg != basename(package)){
      if(!quiet)
        message("Move package '", pkg, "' to archive '", packageArchiveDir, "'")
      file.rename(file.path(srcContribeDir, pkg), file.path(packageArchiveDir, pkg))
    }
  }
}

binContribDirs <- function(root, rVersions = NULL) {
  # Add a number of empty R-version folders by default, just
  # so that these versions of R don't fail when attempting to query
  # the PACKAGES file in the binary directory
  if (is.null(rVersions))
    rVersions <- c("2.15", "2.16", "3.0", "3.1", "3.2", "3.3")

  list(
    win.binary = file.path(root, "bin/windows/contrib", rVersions),
    mac.binary = file.path(root, "bin/macosx/contrib", rVersions),
    mac.binary.mavericks = file.path(root, "bin/macosx/mavericks/contrib", rVersions),
    mac.binary.leopard = file.path(root, "bin/macosx/leopard/contrib", rVersions)
  )
}

#' Upload a Package to a Local CRAN-like Repository
#'
#' @param package Path to a package tarball. The tarball should be
#'   created by \code{R CMD build}; alternatively, it can be the path
#'   to a folder containing the source code for a package (which
#'   will then be built with \code{R CMD build}) and then uploaded
#'   to the local repository.
#' @param to The name of the CRAN-like repository. It (currently) must
#'   be a local (on-disk) CRAN repository.
#' @param archive Keep older version of the package in Archive directory like CRAN does
#' @param ... Optional arguments passed to \code{R CMD build}.
#' @export
repos_upload <- function(package, to, archive = FALSE, ...) {

  # validation
  if (!file.exists(package))
    stop("no package named '", package, "'", call. = FALSE)

  if (is.directory(package) && !file.exists(file.path(package, "DESCRIPTION")))
    stop("directory '", package, "' exists but contains no DESCRIPTION file",
         call. = FALSE)

  if (!(is.directory(package)) && !(grepl("\\.tar\\.gz$", package)))
    stop("file '", package, "' exists but is not appropriately named; ",
         "uploadable package tarballs are generated by `R CMD build`",
         call. = FALSE)

  if (!is.string(to))
    stop("'to' should be a length-one character vector, naming ",
         "a repository available in 'getOption(\"repos\")'",
         call. = FALSE)

  repos <- getOption("repos")
  isNameOfRepo <- to %in% names(repos)
  isRepo <- to %in% repos

  if (!(isNameOfRepo || isRepo))
    stop("no repository '", to, "' available; ",
         "try adding a repository with 'packrat::repos_create()'",
         call. = FALSE)

  if (isNameOfRepo) {
    repoName <- to
    repoPath <- repos[[repoName]]
  } else {
    repoName <- names(repos)[which(repos == to)]
    repoPath <- to
  }

  if (!grepl(reFilePrefix(), repoPath))
    stop("packages can only be uploaded to local CRAN-like repositories with ",
         "this version of packrat",
         call. = FALSE)

  # Path to the upload package
  uploadPkgPath <- ""

  # perform upload
  if (is.directory(package))
    uploadPkgPath <- uploadPackageSourceDir(package, repoName, repoPath, ...)
  else
    uploadPkgPath <- uploadPackageTarball(package, repoName, repoPath)

  # Move old package version to archive
  if(archive)
    pushOldPackagesToArchive(uploadPkgPath, getReposrcContribeDir(repoPath))

  uploadPkgPath
}

uploadPackageSourceDir <- function(package, repoName, repoPath, ...) {

  # create temporary directory for package
  randomString <- paste(sample(c(0:9, letters, LETTERS), 16, TRUE), collapse = "")
  dir <- file.path(tempdir(), paste(basename(package), randomString, sep = "-"))
  on.exit(unlink(dir, recursive = TRUE))

  success <- dir_copy(package, dir, pattern = "^[^\\.]")
  if (!all(success))
    stop("failed to copy package files to temporary directory")

  # Annotate the DESCRIPTION with the name of the repository we're
  # going to be uploading to
  descPath <- file.path(dir, "DESCRIPTION")
  setRepositoryField(descPath, repoName)

  path <- build(dir, ...)
  if (!file.exists(path))
    stop("failed to build source package")

  contribUrl <- getReposrcContribeDir(repoPath)
  success <- file.copy(
    path,
    contribUrl
  )

  if (!success)
    stop("failed to copy built package to CRAN repo '", repoName, "'")

  tools::write_PACKAGES(contribUrl, type = "source")
  message("Package '", basename(path), "' successfully uploaded.")
  file.path(contribUrl, basename(path))
}

uploadPackageTarball <- function(package, repoName, repoPath, ...) {

  # Annotate the package DESCRIPTION with the repository
  tmpTarballPath <- file.path(tempdir(), "packrat-tarball-upload")
  untar(package, exdir = tmpTarballPath)
  pkgName <- getPackageName(package)
  untarredPath <- file.path(tmpTarballPath, pkgName)
  setRepositoryField(
    file.path(untarredPath, "DESCRIPTION"),
    repoName
  )

  owd <- getwd()
  setwd(tmpTarballPath)
  on.exit(setwd(owd), add = TRUE)

  success <- tar(basename(package), files = pkgName)
  if (success != 0)
    stop("Failed to re-tar package tarball")

  path <- normalize.path(basename(package))

  contribUrl <- getReposrcContribeDir(repoPath)
  if (!file.copy(path, contribUrl, overwrite = TRUE))
    stop("failed to upload package '", basename(package), "' to '", contribUrl, "'")

  tools::write_PACKAGES(contribUrl, type = "source")
  message("Package '", basename(path), "' successfully uploaded.")
  file.path(contribUrl, basename(path))
}

addRepos <- function(repos, overwrite = FALSE, local = FALSE) {

  dots <- repos
  dotNames <- names(dots)
  if (!length(dotNames) || any(!nzchar(dotNames)))
    stop("all arguments should be named")

  # For local (on-disk) repositories, ensure that the paths
  # supplied do exist
  if (local) {
    missing <- unlist(lapply(dots, function(x) {
      !file.exists(x)
    }))

    if (any(missing))
      stop("The following paths do not exist: \n- ",
           paste(shQuote(dots[missing]), collapse = "\n- "))
  }

  oldRepos <- getOption("repos")
  if (!overwrite) {
    conflicts <- intersect(names(dots), names(oldRepos))
    if (length(conflicts)) {
      quoted <- paste(shQuote(conflicts), " (", oldRepos[conflicts], ")", sep = "")
      stop("The following repositories have already been set.\n",
           "Use 'overwrite = TRUE' to override these repository paths.\n- ",
           paste(quoted, collapse = "\n- "))
    }
  }

  URIs <- if (local) {
    paths <- normalizePath(unlist(dots), winslash = "/", mustWork = TRUE)
    paste(filePrefix(), paths, sep = "")
  } else {
    unlist(dots)
  }

  newRepos <- URIs
  names(newRepos) <- names(dots)

  repos <- c(oldRepos, newRepos)
  repos <- repos[!duplicated(repos)]
  options(repos = repos)
  invisible(repos)
}

#' Add a Repository
#'
#' Add a repository to the set of currently available repositories. This is
#' effectively an easier-to-use wrapper over interacting with the
#' \code{"repos"} option, which is otherwise set with \code{options(repos = ...)}.
#'
#' \code{repos_add_local} is used for adding file-based repositories; that is,
#' CRAN repositories that live locally on disk and not on the internet / local network.
#'
#' @param ... Named arguments of the form \code{<repoName> = <pathToRepo>}.
#' @param overwrite Boolean; overwrite if a repository with the given name
#'   already exists?
#'
#' @rdname repository-management
#' @name repository-management
#'
#' @export
repos_add <- function(..., overwrite = FALSE) {
  addRepos(list(...), overwrite = overwrite, local = FALSE)
}

#' @rdname repository-management
#' @name repository-management
#' @export
repos_add_local <- function(..., overwrite = FALSE) {
  addRepos(list(...), overwrite = overwrite, local = TRUE)
}

#' @rdname repository-management
#' @name repository-management
#' @export
repos_set <- function(...) {
  addRepos(list(...), overwrite = TRUE, local = FALSE)
}

#' @rdname repository-management
#' @name repository-management
#' @export
repos_set_local <- function(...) {
  addRepos(list(...), overwrite = TRUE, local = TRUE)
}

#' @param names The names of repositories (as exist in e.g.
#'   \code{names(getOption("repos"))}).
#' @rdname repository-management
#' @name repository-management
#' @export
repos_remove <- function(names) {
  oldRepos <- getOption("repos")
  repos <- oldRepos[setdiff(names(oldRepos), names)]
  options(repos = repos)
  invisible(repos)
}

#' @rdname repository-management
#' @name repository-management
#' @export
repos_list <- function() getOption("repos")

setRepositoryField <- function(descPath, repoName) {
  contents <- readLines(descPath)
  repoIdx <- grep("^Repository:", contents)
  repoLine <- paste("Repository:", repoName)
  if (length(repoIdx))
    contents[[repoIdx]] <- repoLine
  else
    contents <- c(contents, repoLine)
  cat(contents, file = descPath, sep = "\n")
}
