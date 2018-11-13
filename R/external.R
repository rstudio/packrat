##' Managing External Libraries
##'
##' These functions provide a mechanism for (temporarily) using packages outside
##' of the packrat private library. The packages are searched within the 'default'
##' libraries; that is, the libraries that would be available upon launching a new
##' \R session.
##'
##' @param packages An optional set of package names (as a character
##'   vector) to load for the duration of evaluation of \code{expr}.
##'   Whether \code{packages} is provided or \code{NULL} (the
##'   default), \code{expr} is evaluated in an environment where the
##'   external library path is in place, not the local (packrat)
##'   library path.
##' @param expr An \R expression.
##' @param envir An environment in which the expression is evaluated.
##' @name packrat-external
##' @rdname packrat-external
##' @examples \dontrun{
##' with_extlib("lattice", xyplot(1 ~ 1))
##' with_extlib(expr = packageVersion("lattice"))
##' # since devtools requires roxygen2 >= 5.0.0 for this step, this
##' # should fail unless roxygen2 is available in the packrat lib.loc
##' with_extlib("devtools", load_all("path/to/project"))
##' # this method will work given roxygen2 is installed in the
##' # non-packrat lib.loc with devtools
##' with_extlib(expr = devtools::load_all("path/to/project"))
##' }
##' @export
with_extlib <- function(packages = NULL, expr, envir = parent.frame()) {

  # need to force this promise now otherwise it will get evaluated
  # in the wrong context later on
  force(envir)

  if (!is.null(packages) && !is.character(packages)) {
    stop("'packages' should be a character vector of libraries", call. = FALSE)
  }

  call <- match.call()

  local({

    tryCatch({
      ## Record the search path, then load the libraries
      oldSearch <- search()

      libPaths <- .packrat_mutables$get("origLibPaths")
      oldLibPaths <- .libPaths()
      if (!length(libPaths))
        libPaths <- getDefaultLibPaths()
      .libPaths(libPaths)

      for (package in packages) {
        library(package, character.only = TRUE, warn.conflicts = FALSE)
      }

      ## Evaluate the call
      error <- try(res <- eval(call$expr, envir = envir), silent = TRUE)

      ## Now, propagate the error / result
      if (exists("res", envir = environment(), inherits = FALSE)) {
        res
      } else {
        stop(attr(error, "condition")$message, call. = FALSE)
      }
    },

    finally = {
      newSearch <- search()
      for (path in setdiff(newSearch, oldSearch)) {
        try(forceUnload(path))
      }
      .libPaths(oldLibPaths)
    })

  })

}

##' @name packrat-external
##' @rdname packrat-external
##' @export
extlib <- function(packages) {

  # place user library at front of library paths (we want to
  # include both the user library and the packrat library just
  # so that external packaegs can still load and depend on
  # packages only installed within the private library as well)
  oldLibPaths <- .libPaths()
  newLibPaths <- c(getDefaultLibPaths(), .libPaths())

  .libPaths(newLibPaths)
  on.exit(.libPaths(oldLibPaths), add = TRUE)

  for (package in packages)
    library(package, character.only = TRUE)

}

loadExternalPackages <- function() {
  pkgs <- get_opts("external.packages")
  if (length(pkgs)) {
    pkgs <- pkgs[ !is.null(pkgs) & !is.na(pkgs) & nchar(pkgs) ]
    failures <- dropNull(lapply(pkgs, function(pkg) {
      tryCatch(
        expr = extlib(pkg),
        error = function(e) {
          pkg
        }
      )
    }))
    if (length(failures)) {
      failures <- as.character(unlist(failures))
      message("Warning: failed to load the following external packages:\n- ",
              paste(shQuote(failures), collapse = ", "))
    }
    return(length(failures) > 0)
  }
  return(TRUE)
}
