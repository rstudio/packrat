#' change packrat lockfile metadata manually
#'
#' @param repos A named character vector of the form \code{c(<repoName> = "<pathToRepo>")}
#' @param rversion A one-length character vector with suitable numeric version
#'   string. See \code{\link[base]{package_version}}
#' @param project  The project directory. When in packrat mode, defaults to the current project;
#'   otherwise, defaults to the current working directory.
#' @export
#' @rdname lockfile-metadata
#' @name lockfile-metadata
#' @examples
set_lockfile_metadata <- function(repos = NULL, r_version = NULL, project = NULL) {
  project <- getProjectDir(project)
  lf_filepath <- lockFilePath(project)
  if (!file.exists(lockFilePath)) {
    stop(paste(lockFilePath, " is missing. Run packrat::init('",
                 project, "') to generate it.", sep = ""))
  }
  lf <- as.data.frame(readDcf(lf_filepath), stringsAsFactors = F)

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
      stop("RVersion metadata must contains one element only", call. = F)
    }
    lf[1, "RVersion"] <- as.character(package_version(r_version))
  }
  # write back the lockfile
  write_dcf(lf, lf_filepath)
  invisible()
}

#' @rdname lockfile-metadata
#' @name lockfile-metadata
get_lockfile_metadata <- function(metadata = c("repos", "r_version"), project = NULL) {
    metadata <- match.arg(metadata)
    lockInfo(project = project, property = metadata, fatal = F)
}


