##' Get Packages on the Search Path
##'
##' Retrieve the packages on the search path, as well as the
##' associated library location.
##'
##' @export
loadedPackages <- function() {

  ## Start by getting everything on the search path + the library location
  pkgs <- data.frame(
    package = search(),
    lib.loc = searchpaths(),
    stringsAsFactors = FALSE
  )

  ## Filter to only actual packages
  pkgs <- pkgs[ grep("^package:", pkgs$package), ]

  ## Clean up the package name by removing the initial 'package:'
  pkgs$package <- sub("^package:", "", pkgs$package)

  ## Unset the rownames
  rownames(pkgs) <- NULL

  ## Get just the directory containing the library, not the library path itself
  pkgs$lib.dir <- dirname(pkgs$lib.loc)

  ## Arrange by lib.dir
  pkgs <- pkgs[ order(pkgs$lib.dir), ]

  pkgs

}
