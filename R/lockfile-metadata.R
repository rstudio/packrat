#' Get / Set packrat lockfile metadata
#'
#' Get and set metadata in the current packrat-managed project lockfile \code{packrat.lock}
#'
#' Project's \code{packrat.lock} contains some metadata before packages
#' dependencies informations. The project's lockfile is created and updated
#' programmatically by \code{\link{snapshot}}. However it could be necessary sometimes to
#' modify manually some of those values. For example, it could be useful to set another repository
#' CRAN url when deploying to a offline environnement.
#'
#' @section available metadata :
#'
#' \itemize{
#' \item \code{r_version}: R version the project depends on
#' \item \code{repos}: Name of repos and their url recorded packages can be
#' retrieve from. Only url is recommended to change if need. Name of repos is
#' used in package records and must be identical
#' }
#'
#' @param repos A named character vector of the form \code{c(<repoName> = "<pathToRepo>")}.
#' @param r_version A length-one character vector with suitable numeric version
#'   string. See \code{\link[base]{package_version}}.
#' @param project The project directory. When in packrat mode, defaults to the current project;
#'   otherwise, defaults to the current working directory.
#' @export
#' @rdname lockfile-metadata
#' @name lockfile-metadata
#' @examples \dontrun{
#' # changes repos url
#' repos <- old_repos <- get_lockfile_metadata("repos")
#' repos
#' repos["CRAN"] <- "https://cran.r-project.org/"
#' set_lockfile_metadata(repos = repos)
#' get_lockfile_metadata("repos")
#' # setting back old state
#' # set_lockfile_metadata(repos = old_repos)
#'
#' # changes R version
#' rver <- old_rver <- get_lockfile_metadata("r_version")
#' rver
#' rver <- "3.4.1"
#' set_lockfile_metadata(r_version = rver)
#' get_lockfile_metadata("r_version")
#' # Setting back old state
#' # set_lockfile_metadata(r_version = old_rver)
#' }
set_lockfile_metadata <- function(repos = NULL, r_version = NULL, project = NULL) {
  project <- getProjectDir(project)
  lf_filepath <- lockFilePath(project)
  if (!file.exists(lf_filepath)) {
    stop(paste(lockFilePath, " is missing. Run packrat::init('",
                 project, "') to generate it.", sep = ""))
  }
  lf <- as.data.frame(readDcf(lf_filepath), stringsAsFactors = FALSE)

  # update repos
  if (!is.null(repos)) {
    # Windows automatically transforms \n to \r\n on write through write.dcf
    separator <- ",\n"
    reposString <- paste(names(repos), unname(repos), sep = "=", collapse = separator)
    lf[1, "Repos"] <- reposString
  }

  # update rversion
  if (!is.null(r_version)) {
    if (length(r_version) > 1) {
      stop("RVersion metadata must contains one element only", call. = FALSE)
    }
    lf[1, "RVersion"] <- as.character(package_version(r_version))
  }
  # write back the lockfile
  write_dcf(lf, lf_filepath)
  invisible()
}

#' @param metadata The lockfile field name(s) to draw from.
#' @param simplify Boolean; if \code{TRUE} the returned metadata will be un-listed.
#'
#' @rdname lockfile-metadata
#' @name lockfile-metadata
#' @export
get_lockfile_metadata <- function(metadata = NULL, simplify = TRUE, project = NULL) {
  project <- getProjectDir(project)
  # Get and parse the lockfile
  lockFilePath <- lockFilePath(project)
  if (!file.exists(lockFilePath)) {
      stop(paste(lockFilePath, " is missing. Run packrat::init('",
                 project, "') to generate it.", sep = ""))
  }
  lf_metadata <- readLockFile(lockFilePath)[names(available_metadata)]
  if (is.null(metadata)) {
    lf_metadata
  } else {
    result <- lf_metadata[names(lf_metadata) %in% metadata]
    if (simplify) unlist(unname(result))
    else result
  }
}

# lockfile metadata available for modification and r_aliases
available_metadata <- c(
  r_version = "RVersion",
  repos = "Repos"
)


