getPackageDependencies <- function(pkgs,
                                   lib.loc,
                                   available.packages = available.packages()) {

  deps <- unlist(lapply(pkgs, function(pkg) {
    pkgDescFile <- system.file('DESCRIPTION', package = pkg,
                               lib.loc = lib.loc)
    if (file.exists(pkgDescFile)) {
      # try to read dependency information from the locally installed package
      # if it's available (dependency information in available.packages may not
      # be accurate if there's a locally installed version with a different
      # dependency list)
      theseDeps <- combineDcfFields(as.data.frame(readDcf(pkgDescFile)),
        c("Depends", "Imports", "LinkingTo"))
    } else if (pkg %in% row.names(available.packages)) {
      # no locally installed version but we can check dependencies in the
      # package database
      theseDeps <- as.list(
        available.packages[pkg, c("Depends", "Imports", "LinkingTo")])
    } else {
      warning("Package '", pkg, "' not available in repository or locally")
      return(NULL)
    }

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

recursivePackageDependencies <- function(pkgs, lib.loc,
                                         available.packages = available.packages()) {

  if (!length(pkgs)) return(NULL)
  deps <- getPackageDependencies(pkgs, lib.loc, available.packages)
  depsToCheck <- setdiff(deps, pkgs)
  while (length(depsToCheck)) {
    newDeps <- getPackageDependencies(depsToCheck, lib.loc, available.packages)
    deps <- sort(unique(c(deps, newDeps)))
    depsToCheck <- setdiff(newDeps, deps)
  }
  if (is.null(deps)) NULL
  else sort(unique(deps))

}
