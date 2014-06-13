#' Install a package from GitHub
#'
#' Use the \code{\link[devtools:install_github]{devtools::install_github}}
#' function to install packages into a project private library. Using this
#' function rather than calling devtools directly enables you to install GitHub
#' packages without adding devtools and it's dependencies to your project
#' private library.
#'
#' @param repo Repository address in the format
#'   \code{[username/]repo[/subdir][@@ref|#pull]}. Alternatively, you can
#'   specify \code{username}, \code{subdir}, \code{ref} or \code{pull} using the
#'   respective parameters.
#' @param ... Other arguments passed on to
#'   \code{\link[devtools:install_github]{devtools::install_github}}.
#' @param build_vignettes If \code{TRUE}, will build the package's vignettes
#' (default to \code{FALSE} for faster in)
#' @param dependencies \code{NA} (the default) has the same behavior as
#'   \code{install.packages} (installs "Depends", "Imports", and "LinkingTo").
#'   See the documentation for
#'   \code{\link[utils:install.packages]{install.packages}} for details on other
#'   valid arguments.
#' @param project The project directory. Defaults to current working directory.
#'
#' @note This function requires the \pkg{devtools} package and will prompt to
#' to install it if it's not already available on the search path.
#' In this case, devtools will be installed into the standard user package
#' library rather than the project private library.
#'
#' @export
install_github <-function(repo,
                          ...,
                          build_vignettes = FALSE,
                          dependencies = NA,
                          project = NULL) {

  # get the default libpath (behave correctly even if we aren't packified)
  libPaths <- NULL
  project <- getProjectDir(project)
  packified <- checkPackified(project, quiet = TRUE)
  if (packified)
    libPaths <- .packrat_mutables$get("origLibPaths")
  if (is.null(libPaths))
    libPaths <- .libPaths()

  # prompt to install devtools if necessary
  if (length(find.package("devtools", lib.loc = libPaths, quiet = TRUE)) == 0) {
    if (interactive()) {
      message("Installing packages from GitHub requires the devtools package.")
      response <- readline("Do you want to install devtools now? [Y/n]: ")
      if (substr(tolower(response), 1, 1) != "n")
        utils::install.packages("devtools", lib = libPaths)
    } else {
      stop("packrat::install_github requires the devtools package.")
    }
  }

  # if we are packfied then pre-pend our private library before installing
  if (packified)
    libPaths <- c(libDir(project), libPaths)

  # execute devtools::install_github with our project library path as
  # well as the system standard library paths
  doInstall <- function() {
    with_libpaths(libPaths,
      devtools::install_github(repo, ..., dependencies = dependencies,
                               build_vignettes = build_vignettes)
    )
  }

  # if devtools is already on the search path then execute the install,
  # otherwise load devtools just for the duration of the call
  if ("package:devtools" %in% search()) {
    doInstall()
  } else {
    suppressMessages(require("devtools", quietly = TRUE, character.only = TRUE))
    on.exit(forceUnload("devtools"), add = TRUE)
    doInstall()
  }
}


