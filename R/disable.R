#' Disable the use of Packrat in a Project
#'
#' Disable packrat within a project, reverting to the use of standard user
#' package libraries.
#'
#' @param project The directory in which packrat will be disabled (defaults to
#'   the current working directory)
#' @param restart If \code{TRUE}, restart the R session after disabling packrat.
#'
#' @note Disabling packrat for a project removes the packrat initialization code
#' from the .Rprofile file, resulting in the use of standard user package
#' libraries. Note that the \code{packrat} directory is not deleted, but remains
#' unused.
#'
#' To re-enable the use of packrat for a project you can call the
#' \code{\link{init}} function.
#'
#' The \code{restart} parameter will only result in a restart of R when the R
#' environment packrat is running within makes available a restart function via
#' \code{getOption("restart")}.
#'
#' @export
disable <- function(project = NULL, restart = TRUE) {

  # get the project
  project <- getProjectDir(project)
  stopIfNotPackified(project)

  # remove packrat from the .Rprofile
  editRprofileAutoloader(project, "remove")

  # turn packrat mode off
  if (isPackratModeOn())
    off(project)

  # restart if requested
  if (restart)
    attemptRestart(restore.packrat.mode = FALSE)

  invisible()
}
