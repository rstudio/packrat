##' Managing External Libraries
##'
##' These functions provide a mechanism for (temporarily) using packages outside
##' of the packrat private library. The packages are searched within the 'default'
##' libraries; that is, the libraries that would be available upon launching a new
##' \R session.
##'
##' @param packages A set of package names (as a character vector) to load for
##'   the duration of evaluation of \code{expr}.
##' @param expr An \R expression.
##' @param envir An environment in which the expression is evaluated.
##' @name packrat-external
##' @rdname packrat-external
##' @examples \dontrun{
##' with_extlib("lattice", xyplot(1 ~ 1))
##' }
##' @export
with_extlib <- function(packages, expr, envir = parent.frame()) {

  # need to force this promise now otherwise it will get evaluated
  # in the wrong context later on
  force(envir)

  if (!is.character(packages)) {
    stop("'packages' should be a character vector of libraries", call. = FALSE)
  }

  call <- match.call()

  local({

    tryCatch({
      ## Record the search path, then load the libraries
      oldSearch <- search()

      libPaths <- .packrat_mutables$get("origLibPaths")
      if (!length(libPaths))
        libPaths <- getDefaultLibPaths()

      for (package in packages) {
        library(package, character.only = TRUE, lib.loc = libPaths, warn.conflicts = FALSE)
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
    })

  })

}

##' @name packrat-external
##' @rdname packrat-external
##' @export
extlib <- function(packages) {
  lib.loc <- getDefaultLibPaths()
  for (package in packages) {
    library(package, character.only = TRUE, lib.loc = lib.loc)
  }
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
