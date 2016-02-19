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
#' @param project The directory that contains the R project.
#' @param lib.loc The library to examine. Defaults to the private library
#' associated with the project directory.
#' @param quiet Print detailed information about the packrat status to the console?
#'
#' @return Either \code{NULL} if a \code{packrat} project has not yet been
#' initialized, or a (invisibly) a \code{data.frame} with components:
#' \item{package}{The package name,}
#' \item{packrat.version}{The package version used in the last snapshot,}
#' \item{packrat.source}{The location from which the package was obtained,}
#' \item{library.version}{The package version available in the local library,}
#' \item{currently.used}{Whether the package is used in any of the R code in the current project.}
#'
#' @export
status <- function(project = NULL, lib.loc = libDir(project), quiet = FALSE) {

  project <- getProjectDir(project)
  stopIfNotPackified(project)

  projectDefault <- identical(project, '.')
  project <- normalizePath(project, winslash = '/', mustWork = TRUE)

  ### Step 1: Collect packages from three sources: packrat.lock, code inspection
  ### (using lib.loc and getLibPaths() to find packages/dependencies), and by
  ### enumerating the packages in lib.loc.

  ## Packages from the lockfile (with their version)
  packratPackages <- lockInfo(project, fatal = FALSE)

  if (length(packratPackages) == 0) {
    initArg <- if (projectDefault) '' else deparse(project)
    cat('This directory does not appear to be using packrat.\n',
        'Call packrat::init(', initArg, ') to initialize packrat.',
        sep = '')
    return(invisible())
  }

  # Get the names, alongside the versions, of packages recorded in the lockfile
  packratNames <- getPackageElement(packratPackages, "name")
  packratVersions <- getPackageElement(packratPackages, "version")
  packratSources <- getPackageElement(packratPackages, "source")

  ## Packages in the library (with their version)
  installedPkgFolders <- list.files(lib.loc, full.names = TRUE)

  installedPkgRecords <- lapply(installedPkgFolders, function(path) {

    descPath <- file.path(path, "DESCRIPTION")
    if (!file.exists(descPath)) {
      warning("No DESCRIPTION file for installed package '", basename(path), "'")
      return(NULL)
    }

    DESCRIPTION <- readDcf(descPath, all = TRUE)
    list(
      name = DESCRIPTION$Package,
      source = DESCRIPTION$InstallSource,
      version = DESCRIPTION$Version
    )
  })

  installedPkgNames <- unlist(lapply(installedPkgRecords, `[[`, "name"))
  names(installedPkgNames) <- installedPkgNames
  installedPkgVersions <- unlist(lapply(installedPkgRecords, `[[`, "version"))
  names(installedPkgVersions) <- installedPkgNames

  # Manually construct package records suitable for later reporting

  # Packages inferred from the code
  # Don't stop execution if package missing from library; just propagate later
  # as information to user
  #
  # NOTE: We avoid explicitly calling `available.packages()`, just in case we haven't
  # yet cached the set of available packages. However, to infer broken dependency chains
  # it is in general necessary to have the set of `available.packages()` to fill in
  # broken links.
  availablePkgs <- if (hasCachedAvailablePackages())
    available.packages()
  else
    availablePackagesSkeleton()

  inferredPkgNames <- appDependencies(
    project,
    available.packages = availablePkgs
  )


  # Suppress warnings on 'Suggests', since they may be from non-CRAN repos (e.g. OmegaHat)
  suggestedPkgNames <- suppressWarnings(
    appDependencies(project,
                    available.packages = availablePkgs,
                    fields = "Suggests")
  )

  # All packages mentioned in one of the three above
  allPkgNames <- sort_c(unique(c(
    packratNames, installedPkgNames, inferredPkgNames
  )))

  # Match the above with the set of all package names
  .match <- function(what, from = allPkgNames) {
    if (is.null(what)) NA
    else what[from]
  }

  packrat.version <- .match(packratVersions)
  packrat.source  <- .match(packratSources)
  library.version <- .match(installedPkgVersions)
  currently.used <- allPkgNames %in% c(inferredPkgNames, suggestedPkgNames)

  # Generate a table that holds the current overall state
  external.packages <- opts$external.packages()
  statusTbl <- data.frame(stringsAsFactors = FALSE,
                          row.names = 1:length(allPkgNames),
                          package = allPkgNames,
                          packrat.version = packrat.version,
                          packrat.source  = packrat.source,
                          library.version = library.version,
                          currently.used  = currently.used,
                          external.package = allPkgNames %in% external.packages
  )

  # Only give information on packages not included in external.packages
  statusTbl <- statusTbl[!statusTbl$external.package, ]

  # Fill the state, according to the different kinds of mismatches there might
  # be between packrat.version, library.version, currently.used

  if (!quiet) {

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
        footer = c("You can call packrat::snapshot() to remove these packages from the lockfile, ",
                   "or if you intend to use these packages, use packrat::restore() to restore them ",
                   "to your private library.")
      )
    }

    # Packages that are used in the code, but are not mentioned in either packrat
    # or the library
    whichUntrackedPackages <- with(statusTbl,
                                   currently.used &
                                     is.na(packrat.version) &
                                     is.na(library.version)
    )
    pkgNamesUntracked <- statusTbl$package[whichUntrackedPackages]

    if (length(pkgNamesUntracked)) {
      prettyPrintNames(
        pkgNamesUntracked,
        c("The following packages are referenced in your code, but are not present\n",
          "in your library nor in packrat:\n"),
        c("\nYou will need to install these packages manually, then use\n",
          "packrat::snapshot() to record these packages in packrat.")
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
        searchPackages(installedPkgRecords, missingFromPackrat),
        searchPackages(packratPackages, missingFromPackrat),
        "The following packages have been updated in your library, but have not been recorded in packrat:",
        "Use packrat::snapshot() to record these packages in packrat.",
        "library",
        "packrat"
      )
    }

    # Packages that are tracked by packrat, currently used, but out of sync in the library
    whichOutOfSync <- with(statusTbl,
                           currently.used &
                             !is.na(packrat.version) &
                             !is.na(library.version) &
                             packrat.version != library.version)
    pkgNamesOutOfSync <- statusTbl$package[whichOutOfSync]

    if (length(pkgNamesOutOfSync)) {
      prettyPrintPair(
        searchPackages(packratPackages, pkgNamesOutOfSync),
        searchPackages(installedPkgRecords, pkgNamesOutOfSync),
        "The following packages are out of sync between packrat and your current library:",
        c("Use packrat::snapshot() to set packrat to use the current library, or use\n",
          "packrat::restore() to reset the library to the last snapshot."),
        "packrat",
        "library"
      )
    }

    # Packages which have been deleted from the library, but are still tracked by packrat,
    # and still in use
    whichDeletedButStillTracked <- with(statusTbl,
                                        currently.used &
                                          !is.na(packrat.version) &
                                          is.na(library.version))
    deletedButStillTracked <- statusTbl$package[whichDeletedButStillTracked]

    if (length(deletedButStillTracked)) {
      prettyPrintPair(
        searchPackages(packratPackages, deletedButStillTracked),
        searchPackages(installedPkgRecords, deletedButStillTracked),
        "The following packages are used in your code, tracked by packrat, but no longer present in your library:",
        c("Use packrat::restore() to restore these libraries.")
      )
    }

    # If everything is in order, let the user know
    if (!(any(onlyPackrat) ||
            length(missingFromPackrat) ||
            length(pkgNamesUntracked) ||
            length(pkgNamesOutOfSync) ||
            length(deletedButStillTracked) ||
            length(missingFromPackrat))) {
      message("Up to date.")
    }

  }

  invisible(statusTbl)

}

getPackageElement <- function(package, element) {

  setNames(
    unlist(lapply(package, "[[", element)),
    unlist(lapply(package, "[[", "name"))
  )

}

hasCachedAvailablePackages <- function() {
  contrib.url <- contrib.url(getOption('repos'))
  tempFiles <- list.files(tempdir())
  repoNames <- paste("repos_", URLencode(contrib.url, TRUE), ".rds", sep = "")
  all(repoNames %in% tempFiles)
}
