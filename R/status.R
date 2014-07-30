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
  project <- normalizePath(project, winslash='/', mustWork=TRUE)

  ### Step 1: Collect packages from three sources: packrat.lock, code inspection
  ### (using lib.loc and getLibPaths() to find packages/dependencies), and by
  ### enumerating the packages in lib.loc.

  ## Packages from the lockfile (with their version)
  packratPackages <- lockInfo(project, fatal=FALSE)

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
  installedPkgs <- installed.packages(
    lib.loc = lib.loc,
    noCache = TRUE
  )[, "Package"]

  ## Ignore 'rstudio', 'manipulate'
  installedPkgs <- setdiff(installedPkgs, c('rstudio', 'manipulate'))

  ## If we are using packrat alongside an R package, then we should
  ## ignore the package itself
  if (file.exists(file.path(project, "DESCRIPTION"))) {
    pkgName <- unname(readDcf(file.path(project, "DESCRIPTION"))[, "Package"])
    installedPkgs <- installedPkgs[installedPkgs != pkgName]
  }

  # Recursive should be false here -- we collect records _only_ for packages which are installed
  installedPkgRecords <- flattenPackageRecords(
    getPackageRecords(installedPkgs,
                      project = project,
                      recursive = FALSE,
                      lib.loc = lib.loc,
                      missing.package = function(...) NULL
    )
  )
  installedPkgNames <- getPackageElement(installedPkgRecords, "name")
  installedPkgVersions <- getPackageElement(installedPkgRecords, "version")

  # Packages inferred from the code
  # Don't stop execution if package missing from library; just propogate later
  # as information to user
  inferredPkgNames <- appDependencies(project)
  inferredPkgRecords <- getPackageRecords(
    inferredPkgNames,
    project = project,
    lib.loc = lib.loc,
    available = available.packages(contrib.url(activeRepos(project))),
    missing.package = function(package, lib.loc) NULL
  )

  # All packages mentioned in one of the three above
  allPkgNames <- sort(unique(c(
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
  currently.used <- allPkgNames %in% inferredPkgNames

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

    # Packages that are no longer used, but still seen in the library
    whichPkgsNotNeeded <- with(statusTbl,
                               !currently.used &
                                 !is.na(library.version)
    )
    pkgsNotNeeded <- statusTbl$package[whichPkgsNotNeeded]
    if (length(pkgsNotNeeded)) {
      prettyPrint(
        searchPackages(installedPkgRecords, pkgsNotNeeded),
        "The following packages are installed but not needed:",
        c("Use packrat::clean() to remove them. Or, if they are actually needed\n",
          "by your project, add `library(packagename)` calls to a .R file\n",
          "somewhere in your project.")
      )
    }

    # If everything is in order, let the user know
    if (!(any(onlyPackrat) ||
            length(missingFromPackrat) ||
            length(pkgNamesUntracked) ||
            length(pkgNamesOutOfSync) ||
            length(deletedButStillTracked) ||
            length(missingFromPackrat) ||
            length(pkgsNotNeeded))) {
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
