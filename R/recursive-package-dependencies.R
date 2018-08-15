getPackageDependencies <- function(pkgs,
                                   lib.loc,
                                   available.packages = availablePackages(),
                                   fields = c("Depends", "Imports", "LinkingTo")) {

  if (isPackratModeOn()) {
    lockPkgs <- readLockFilePackages(file = lockFilePath())
  }

  deps <- unlist(lapply(pkgs, function(pkg) {

    # Read the package DESCRIPTION file
    pkgDescFile <- system.file('DESCRIPTION', package = pkg,
                               lib.loc = lib.loc)

    # Get any packages available in local repositories
    localReposPkgPaths <- as.character(unlist(lapply(opts$local.repos(), function(x) {
      fullPaths <- list.files(x, full.names = TRUE)
      fullPaths[file.exists(file.path(fullPaths, "DESCRIPTION"))]
    })))
    localReposPkgs <- basename(localReposPkgPaths)

    if (file.exists(pkgDescFile)) {
      # try to read dependency information from the locally installed package
      # if it's available (dependency information in available.packages may not
      # be accurate if there's a locally installed version with a different
      # dependency list)
      theseDeps <- combineDcfFields(as.data.frame(readDcf(pkgDescFile)), fields)
    } else if (isPackratModeOn() && pkg %in% names(lockPkgs)) {
      # if packrat mode is on, we'll also try reading dependencies from the lock file
      theseDeps <- lockPkgs[[pkg]]$requires

    } else if (pkg %in% row.names(available.packages)) {
      # no locally installed version but we can check dependencies in the
      # package database
      theseDeps <- as.list(
        available.packages[pkg, fields])
    } else if (pkg %in% localReposPkgs) {
      # use the version in the local repository
      allIdx <- which(localReposPkgs == pkg)
      path <- localReposPkgPaths[allIdx[1]]
      if (length(allIdx) > 1) {
        warning("Package '", pkg, "' found in multiple local repositories; ",
                "inferring dependencies from package at path:\n- ", shQuote(path))
      }
      theseDeps <- combineDcfFields(as.data.frame(readDcf(path)), fields)
    } else {
      warning("Package '", pkg, "' not available in repository or locally")
      return(NULL)
    }

    ## Split fields, remove white space
    splitDeps <- lapply(theseDeps, function(x) {
      if (is.na(x)) return(NULL)
      splat <- unlist(strsplit(x, ",[[:space:]]*"))
      ## Remove versioning information as this function only returns package names
      splat <- gsub("\\(.*", "", splat, perl = TRUE)
      gsub("[[:space:]].*", "", splat, perl = TRUE)
    })
    unlist(splitDeps, use.names = FALSE)

  }))

  deps <- dropSystemPackages(deps)

  if (is.null(deps)) NULL
  else sort_c(unique(deps))
}

discoverBaseRecommendedPackages <- function() {

  # First, attempt to ask 'tools' what the standard package
  # names are. Since this function is unexported we are
  # careful on how we query + use it.
  tools <- asNamespace("tools")
  pkgs <- tryCatch(tools$.get_standard_package_names(), error = identity)
  ok <- is.list(pkgs) &&
    all(c("base", "recommended") %in% names(pkgs)) &&
    length(pkgs$base) &&
    length(pkgs$recommended)
  if (ok)
    return(pkgs)

  # Otherwise, fall back to installed.packages().
  ip <- utils::installed.packages()
  list(
    base        = rownames(ip)[ip[, "Priority"] %in% "base"],
    recommended = rownames(ip)[ip[, "Priority"] %in% "recommended"]
  )

}

excludeBasePackages <- function(packages) {
  pkgs <- discoverBaseRecommendedPackages()
  setdiff(packages, c("R", pkgs$base))
}

excludeRecommendedPackages <- function(packages) {

  # NOTE: becase utils::installed.packages() can fail in some
  # scenarios, e.g. when libraries live on networked drives,
  # we fall back to a simple listing of files in the associated
  # library paths
  installedPkgsSystemLib <- list.files(.Library)
  installedPkgsLocalLib  <- list.files(.libPaths()[1])

  ## Exclude recommended packages if there is no package installed locally
  ## this places an implicit dependency on the system-installed version of a package
  pkgs <- discoverBaseRecommendedPackages()
  rcmd <- pkgs$recommended
  recommendedPkgsInSystemLib <- intersect(installedPkgsSystemLib, rcmd)
  recommendedPkgsInLocalLib <- intersect(installedPkgsLocalLib, rcmd)
  toExclude <- setdiff(recommendedPkgsInSystemLib, recommendedPkgsInLocalLib)
  setdiff(packages, toExclude)

}

dropSystemPackages <- function(packages) {

  # always exclude base packages
  packages <- excludeBasePackages(packages)

  # exclude recommended packages if desired by user
  if (!isTRUE(packrat::opts$snapshot.recommended.packages()))
    packages <- excludeRecommendedPackages(packages)

  packages
}

recursivePackageDependencies <- function(pkgs, lib.loc,
                                         available.packages = availablePackages(),
                                         fields = c("Depends", "Imports", "LinkingTo")) {

  if (!length(pkgs)) return(NULL)
  deps <- getPackageDependencies(pkgs, lib.loc, available.packages, fields)
  depsToCheck <- setdiff(deps, pkgs)
  while (length(depsToCheck)) {
    newDeps <- getPackageDependencies(depsToCheck, lib.loc, available.packages, fields)
    depsToCheck <- setdiff(newDeps, deps)
    deps <- sort_c(unique(c(deps, newDeps)))
  }
  if (is.null(deps)) NULL
  else sort_c(unique(deps))

}
