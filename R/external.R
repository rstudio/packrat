##' Managing External Libraries
##'
##' These functions provide a mechanism for (temporarily) using packages outside of the
##' packrat private library.
##'
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

  if (!is.character(packages)) {
    stop("'packages' should be a character vector of libraries", call. = FALSE)
  }

  call <- match.call()

  local({

    tryCatch({
      ## Record the search path, then load the libraries
      oldSearch <- search()
      ## This will be NULL when not in packrat mode -- but then this implies
      ## we should use the user library anyway, so this is fine
      origLibPaths <- .packrat_mutables$get("origLibPaths")

      for (package in packages) {
        library(package, character.only = TRUE, lib.loc = origLibPaths, warn.conflicts = FALSE)
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
  for (package in packages) {
    lib.loc <- .packrat_mutables$get("origLibPaths")
    if (is.null(lib.loc)) {
      lib.loc <- getLibPaths()
    }
    library(package, character.only = TRUE, lib.loc = lib.loc)
  }
}

loadExternalPackages <- function() {
  pkgs <- get_opts("external.packages")
  if (length(pkgs)) {
    pkgs <- pkgs[ !is.null(pkgs) & !is.na(pkgs) & nchar(pkgs) ]
    for (pkg in pkgs) extlib(pkg)
  }
}
