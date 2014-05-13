#' Show differences between the last snapshot and the library
#'
#' Shows the differences between the project's packrat dependencies, its private
#' package library, and its R scripts.
#'
#' These differences are created when you use the normal R package management
#' commands like \code{\link{install.packages}}, \code{\link{update.packages}},
#' and \code{\link{remove.packages}}. To bring these differences into packrat, you
#' can use \code{\link{snapshot}}.
#'
#' Differences can also arise if one of your collaborators adds or removes
#' packages from the packrat dependencies. In this case, you simply need to tell
#' packrat to update your private package library using \code{\link{restore}}.
#'
#' @param projDir The directory that contains the R project.
#' @param lib.loc The library to examine. Defaults to the private library
#' associated with the project directory.
#' @param quiet \code{TRUE} to suppress output, \code{FALSE} (the default) to
#' show output.
#'
#' @return Either \code{NULL} if a \code{packrat} project has not yet been
#' initialized, or a (invisibly) a \code{data.frame} with components:
#' \item{package}{The package name.}
#' \item{packrat.version}{The package version used in the last snapshot.}
#' \item{packrat.source}{The location from which the package was obtained.}
#' \item{library.version}{The package version available in the local library.}
#' \item{currently.used}{Whether the package is used in any of the R code in the current project.}
#'
#' @export
status <- function(projDir = '.', lib.loc = libdir(projDir), quiet = FALSE) {

  projDirDefault <- identical(projDir, '.')
  projDir <- normalizePath(projDir, winslash='/', mustWork=TRUE)

  ### Step 1: Collect packages from three sources: packrat.lock, code inspection
  ### (using lib.loc and .libPaths() to find packages/dependencies), and by
  ### enumerating the packages in lib.loc.

  ## Packages from the lockfile (with their version)
  packratPackages <- lockInfo(projDir, fatal=FALSE)

  if (length(packratPackages) == 0) {
    bootstrapArg <- if (projDirDefault) '' else deparse(projDir)
    cat('This directory does not appear to be using packrat.\n',
        'Call packrat::bootstrap(', bootstrapArg, ') to initialize packrat.',
        sep = '')
    return(invisible())
  }

  # Get the names, alongside the versions, of packages recorded in the lockfile
  packratNames <- unlist(lapply(packratPackages, "[[", "name"))
  packratVersions <- unlist(lapply(packratPackages, "[[", "version"))
  packratSources <- unlist(lapply(packratPackages, "[[", "source"))

  ## Packages in the library (with their version)
  installedPkgs <- installed.packages(
    lib.loc = lib.loc,
    noCache = TRUE
  )[, "Package"]

  installedPkgRecords <- getPackageRecords(
    installedPkgs
  )
  installedPkgNames <- names(installedPkgRecords)
  installedPkgVersions <- unlist(lapply(installedPkgRecords, "[[", "version"))

  # Packages inferred from the code
  inferredPkgNames <- appDependencies(projDir)

  # All packages mentioned in one of the three above
  allPkgNames <- sort(unique(c(
    packratNames, installedPkgNames, inferredPkgNames
  )))

  # Match the above with the set of all package names
  packrat.version <- packratVersions[allPkgNames]
  packrat.source  <- packratSources[allPkgNames]
  library.version <- installedPkgVersions[allPkgNames]
  currently.used <- allPkgNames %in% inferredPkgNames

  # Generate a table that holds the current overall state
  statusTbl <- data.frame( stringsAsFactors = FALSE,
                           row.names = 1:length(allPkgNames),
                           package = allPkgNames,
                           packrat.version = packrat.version,
                           packrat.source  = packrat.source,
                           library.version = library.version,
                           currently.used  = currently.used
  )

  # Fill the state, according to the different kinds of mismatches there might
  # be between packrat.version, library.version, currently.used

  # Packages that are only tracked within packrat, but are no longer present
  # in the local library nor found in the user's code
  onlyPackrat <- with(statusTbl,
                      !is.na(packrat.version) &
                        is.na(library.version) &
                        !currently.used
  )

  if (any(onlyPackrat)) {
    prettyPrint(
      searchPackages(packratPackages, statusTbl$package[onlyPackrat]),
      header = c("The following packages are tracked by packrat, but are no longer ",
                 "available in the local library nor present in your code:"),
      footer = c("You should call packrat::snapshot() to update the packrat lockfile.")
    )
  }

  # Packages that are missing from the library, or out of sync
  whichOutOfSync <- with(statusTbl,
                         currently.used &
                           (packrat.version != library.version)
  )
  whichOutOfSync[is.na(whichOutOfSync)] <- TRUE

  pkgNamesOutOfSync <- statusTbl$package[whichOutOfSync]
  if (length(pkgNamesOutOfSync)) {
    prettyPrintPair(
      searchPackages(packratPackages, pkgNamesOutOfSync),
      searchPackages(libPackages, pkgNamesOutOfSync),
      "The following packages are missing from your library, or out of sync with packrat:",
      "Use packrat::restore() to install/remove the appropriate packages.",
      "packrat",
      "library"
    )
  }

  # Packages that are in the library, currently used, but not tracked by packrat
  whichMissingFromPackrat <- with(statusTbl,
                                  currently.used &
                                    is.na(packrat.version) &
                                    (!is.na(library.version))
  )

  missingFromPackrat <- statusTbl$package[whichMissingFromPackrat]

  if (length(missingFromPackrat)) {
    prettyPrintPair(
      searchPackages(packratPackages, missingFromPackrat),
      searchPackages(libPackages, missingFromPackrat),
      "The following packages have been updated in your library, but have not been recorded in packrat:",
      "Use packrat::snapshot() to record these packages in packrat.",
      "packrat",
      "library"
    )
  }

  whichPkgsNotNeeded <- with(statusTbl,
                        !currently.used &
                          !is.na(library.version)
                        )
  pkgsNotNeeded <- statusTbl$package[whichPkgsNotNeeded]
  if (length(pkgsNotNeeded)) {
    prettyPrint(
      searchPackages(libPackages, pkgsNotNeeded),
      "The following packages are installed but not needed:",
      c("Use packrat::clean() to remove them. Or, if they are actually needed\n",
        "by your project, add `library(packagename)` calls to a .R file\n",
        "somewhere in your project.")
    )
  }

  # If everything is in order, let the user know
  if (!(any(onlyPackrat) || length(pkgNamesOutOfSync) || length(missingFromPackrat) || length(pkgsNotNeeded))) {
    cat("Up to date.\n")
  }

  invisible(statusTbl)

}
