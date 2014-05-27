##' Load an External Package
##'
##' This function loads a non-packrat package that might be useful for development
##' while within a packrat project, but otherwise unused for a current project.
##'
##' Unlike \code{\link{library}}, this function requires you to supply the package
##' name as a character vector, not a symbol.
##' @export
extlib <- function(packages) {
  for (package in packages) {
    lib.loc <- .packrat_mutables$get("origLibPaths")
    if (is.null(lib.loc)) {
      lib.loc <- .libPaths()
    }
    library(package, character.only = TRUE, lib.loc = lib.loc)
  }
}
