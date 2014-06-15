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
#'
#' @note This function requires the \pkg{devtools} package and will prompt to
#' to install it if it's not already available in the standard library paths.
#' In this case, devtools will be installed into the standard user package
#' library rather than the project private library.
#'
#' @export
install_github <-function(repo,
                          ...,
                          build_vignettes = FALSE,
                          dependencies = NA) {

  # look for devtools in the original libs and prompt to install if necessary
  origLibPaths <- .packrat_mutables$get("origLibPaths")
  if (length(find.package("devtools", lib.loc = origLibPaths, quiet = TRUE)) == 0) {
    if (interactive()) {
      message("Installing packages from GitHub requires the devtools package.")
      response <- readline("Do you want to install devtools now? [Y/n]: ")
      if (substr(tolower(response), 1, 1) != "n")
        utils::install.packages("devtools", lib = origLibPaths)
    } else {
      stop("packrat::install_github requires the devtools package.")
    }
  }

  # execute devtools::install_github with version of devtools (and dependencies)
  # installed in original lib paths
  args <- list(...)
  args$repo <- repo
  args$build_vignettes <- build_vignettes
  args$dependencies <- dependencies
  with_extlib(c("httr", "devtools"), envir = environment(), {
    f <- get("install_github", envir = as.environment("package:devtools"))
    do.call(f, args)
  })
  invisible()
}


