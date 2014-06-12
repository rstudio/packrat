getPackageDependencies <- function(pkgs, available.packages = available.packages()) {

  deps <- unlist(lapply(pkgs, function(pkg) {

    if (!(pkg %in% row.names(available.packages))) {
      warning("Package '", pkg, "' not available in repository")
      return(NULL)
    }

    theseDeps <- as.list(available.packages[pkg, c("Depends", "Imports", "LinkingTo")])

    ## Split fields, remove white space, get versioning
    splitDeps <- lapply(theseDeps, function(x) {
      if (is.na(x)) return(NULL)
      splat <- unlist(strsplit(x, ",[[:space:]]*"))
      gsub("[[:space:]].*", "", splat)
    })
    unlist(splitDeps, use.names = FALSE)

  }))

  ## Don't worry about R, base, recommended packages
  ip <- installed.packages()
  basePkgs <- rownames(ip)[!is.na(ip[, "Priority"])]
  deps <- setdiff(deps, c("R", basePkgs))
  if (is.null(deps)) NULL
  else sort(unique(deps))
}

recursivePackageDependencies <- function(pkgs, available.packages = available.packages()) {

  if (!length(pkgs)) return(NULL)
  deps <- getPackageDependencies(pkgs, available.packages)
  depsToCheck <- setdiff(deps, pkgs)
  while (length(depsToCheck)) {
    newDeps <- getPackageDependencies(depsToCheck, available.packages)
    deps <- sort(unique(c(deps, newDeps)))
    depsToCheck <- setdiff(newDeps, deps)
  }
  if (is.null(deps)) NULL
  else sort(unique(deps))

}
