##' Get Packages on the Search Path
##'
##' Retrieve the packages on the search path, as well as the
##' associated library location.
##'
##' @export
search_path <- function() {

  ## NOTE: We cannot use searchpaths() here because it follows symlinks --
  ## for consistency, we want to use the symlink, not the actual path

  ## Start by getting everything on the search path + the library location
  pkgs <- data.frame(
    package = search(),
    stringsAsFactors = FALSE
  )

  ## Filter to only actual packages
  pkgs <- pkgs[ grep("^package:", pkgs$package), , drop = FALSE]

  ## Clean up the package name by removing the initial 'package:'
  pkgs$package <- sub("^package:", "", pkgs$package)

  ## Get the library locations
  ## NOTE: find.package is special-cased when the first argument is
  ## of length 1 -- we rely on that behaviour here
  ## normalizePath used on Windows because .Library can be stored as a
  ## truncated path
  ## ...but only do it on Windows, because normalizePath will resolve symlinks,
  ## which we want to avoid here
  pkgs$lib.loc <- unlist(lapply(pkgs$package, find.package))
  if (is.windows()) {
    pkgs$lib.loc <- normalizePath(pkgs$lib.loc, winslash = "/")
  }

  ## Get just the directory containing the library, not the library path itself
  pkgs$lib.dir <- dirname(pkgs$lib.loc)

  ## Arrange by lib.dir
  pkgs <- pkgs[ order(pkgs$lib.dir), ]

  ## Unset the rownames
  rownames(pkgs) <- NULL

  pkgs

}
