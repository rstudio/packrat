findPackageDirectoriesAndTarballs <- function(dir) {
  dirs <- list.dirs(dir, recursive = FALSE)
  hasDesc <- unlist(lapply(dirs, function(dir) {
    file.exists(file.path(dir, "DESCRIPTION"))
  }))
  dirs[hasDesc]
}

##' Install a Package from a Local Repository
##'
##' This function can be used to install a package from a local 'repository'; i.e.,
##' a directory containing package tarballs and sources.
##'
##' @param pkgs A character vector of package names.
##' @param lib The library in which the package should be installed.
##' @param repos The local repositories to search for the package names specified.
##' @param ... Optional arguments passed to \code{\link[packrat]{install}}.
##' @export
install_local <- function(pkgs,
                          ...,
                          lib = .libPaths()[1],
                          repos = get_opts("local.repos")) {
  for (pkg in pkgs) {
    install_local_single(pkg, lib = lib, repos = repos, ...)
  }
}

findLocalRepoForPkg <- function(pkg,
                                repos = get_opts("local.repos"),
                                fatal = TRUE) {
  if (!length(repos) || identical(repos, "")) return(character())
  # Search through the local repositories for a suitable package
  hasPackage <- unlist(lapply(repos, function(repo) {
    file.exists(file.path(repo, pkg))
  }))
  names(hasPackage) <- repos
  numFound <- sum(hasPackage)
  if (numFound == 0) {
    if (fatal) {
      stop("No package '", pkg, "' found in local repositories specified")
    } else {
      return(NULL)
    }
  }

  if (numFound > 1)
    warning("Package '", pkg, "' found in multiple local repositories:\n- ",
            paste(shQuote(file.path(repos[hasPackage], pkg)), collapse = ", "))

  repos[hasPackage][1]

}

install_local_single <- function(pkg,
                                 lib = .libPaths()[1],
                                 repos = get_opts("local.repos"),
                                 fatal = TRUE,
                                 ...) {

  if (!length(repos) || identical(repos, ""))
    stop("No local repositories have been defined. ",
         "Use 'packrat::set_opts(local.repos = ...)' to add local repositories.",
         call. = FALSE)
  repoToUse <- findLocalRepoForPkg(pkg, repos, fatal = fatal)
  path <- file.path(repoToUse, pkg)
  with_libpaths(lib, install_local_path(path = path, ...))

}
