##' Temporarily load External Libraries for Evaluation of an Expression
##'
##' This can be useful if you need to call a function from a library
##' not managed by \code{packrat}, e.g. \code{devtools}.
##'
##' @param libs A set of library names (as a character vector) to load for
##'   the duration of evaluation of \code{expr}.
##' @param expr An \R expression.
withExtLibs <- function(libs, expr) {

  if (!is.character(libs)) {
    stop("'libs' should be a character vector of libraries", call. = FALSE)
  }

  call <- match.call()

  local({

    tryCatch({
      ## Record the search path, then load the libraries
      oldSearch <- search()

      ## This will be NULL when not in packrat mode -- but then this implies
      ## we should use the user library anyway, so this is fine
      origLibPaths <- .packrat_mutables$get("origLibPaths")

      for (lib in libs) {
        library(lib, character.only = TRUE, lib.loc = origLibPaths, warn.conflicts = FALSE)
      }

      ## Evaluate the call
      error <- try(res <- eval(call$expr), silent = TRUE)

      ## Now, propagate the error / result
      if (exists("res")) {
        res
      } else {
        stop(attr(error, "condition")$message, call. = FALSE)
      }
    },

    finally = {
      newSearch <- search()
      for (path in setdiff(newSearch, oldSearch)) {
        try(detach(path, character.only = TRUE, unload = TRUE))
      }
    })

  })

}
