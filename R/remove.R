
#' Remove Packrat from a Project
#'
#' Remove packrat from the specified project, reverting to the use of standard
#' user package libraries.
#'
#' @param project The directory which packrat will be removed from (defaults
#'  to the current working directory)
#' @param restart If \code{TRUE}, restart the R session after restoring.
#'
#' @note
#' The \code{restart} parmaeter will only result in a restart of R when the
#' R environment packrat is running within makes available a restart function
#' via \code{getOption("restart")}.
#'
#' @export
remove <- function(project = NULL, restart = TRUE) {

  # get the project
  project <- getProjectDir(project)
  stopIfNotPackified(project)

  # remove packrat from the .Rprofile
  editRprofileAutoloader(project, "remove")

  # restart if requested
  if (restart)
    attemptRestart()

  invisible()
}
