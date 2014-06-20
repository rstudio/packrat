getPackageDependencies <- function(pkgs,
                                   lib.loc,
                                   available.packages = available.packages()) {

  if (isPackratModeOn()) {
    lockPkgs <- readLockFilePackages(file = lockFilePath())
  }

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
    } else if (isPackratModeOn() && pkg %in% names(lockPkgs)) {
      # if packrat mode is on, we'll also try reading dependencies from the lock file
      theseDeps <- lockPkgs[[pkg]]$requires

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

  ## Exclude base packages
  installedPkgsSystemLib <- as.data.frame(installed.packages(lib.loc = .Library), stringsAsFactors = FALSE)

  # The first library path is implicitly the user library
  installedPkgsLocalLib <- as.data.frame(installed.packages(lib.loc = .libPaths()[1]), stringsAsFactors = FALSE)

  basePkgs <- with(installedPkgsSystemLib, Package[Priority %in% "base"])
  deps <- setdiff(deps, c("R", basePkgs))

  ## Exclude recommended packages if there is no package installed locally
  ## this places an implicit dependency on the system-installed version of a package
  recommendedPkgsInSystemLib <- with(installedPkgsSystemLib, Package[Priority %in% "recommended"])
  recommendedPkgsInLocalLib <- with(installedPkgsLocalLib, Package[Priority %in% "recommended"])
  toExclude <- setdiff(recommendedPkgsInSystemLib, recommendedPkgsInLocalLib)
  deps <- setdiff(deps, toExclude)

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
    depsToCheck <- setdiff(newDeps, deps)
    deps <- sort(unique(c(deps, newDeps)))
  }
  if (is.null(deps)) NULL
  else sort(unique(deps))

}
