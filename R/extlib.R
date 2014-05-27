##' Load an External Package
##'
##' Load a package from the user (non-private) library.
##'
##' Unlike \code{\link{library}}, this function requires you to supply the package
##' name as a character vector, not a symbol.
##' @param packages A list of \R packages to load from the default user library.
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
