#' Packrat: Reproducible dependency management
#' 
#' Packrat is a tool for managing the R packages your project depends on in 
#' an isolated, portable, and reproducible way.
#' 
#' Use packrat to make your R projects more:
#' 
#' \itemize{
#' \item \strong{Isolated}: Installing a new or updated package for one project
#' won't break your other projects, and vice versa. That's because packrat gives
#' each project its own private package library.
#' \item \strong{Portable}: Easily transport your projects from one computer to
#' another, even across different platforms. Packrat makes it easy to install the
#' packages your project depends on.
#' \item \strong{Reproducible}: Packrat records the exact package versions you
#' depend on, and ensures those exact versions are the ones that get installed
#' wherever you go.}
#' 
#' Use \code{\link{bootstrap}} to create a new packrat project,
#' \code{\link{snapshot}} to record changes to your project's library, and
#' \code{\link{restore}} to recreate your library the way it was the last time you
#' (or anyone!) took a snapshot. 
#' 
#' Using these simple functions and sharing packrat's files lets you collaborate
#' in a shared, consistent environment with others as your project grows and
#' changes, and provides an easy way to share your results when you're done. 
#' 
#' @section Anatomy of a packrat project:
#' 
#' A packrat project contains a few extra files and directories. The
#' \code{\link{bootstrap}} function creates these files for you, if they don't
#' already exist.
#' 
#' \describe{
#'   \item{\code{library/}}{Private package library for this project.}
#' 
#'   \item{\code{packrat.lock}}{Lists the precise package versions that were used
#' to satisfy dependencies, including dependencies of dependencies. (This file
#' should never be edited by hand!)}
#' 
#'   \item{\code{packrat.sources/}}{Source packages of all the dependencies that
#' packrat has been made aware of.}
#' 
#'   \item{\code{.Rprofile and .Renviron}}{Directs R to use the private package
#' library (when it is started from the project directory).}
#' }
#' 
#' @section Using packrat with version control:
#' 
#' Packrat is designed to work hand in hand with Git, Subversion, or any other
#' version control system. Be sure to check in the \code{.Rprofile},
#' \code{.Renviron}, and \code{packrat.lock} files, as well as everything under
#' \code{packrat.sources/}. You can tell your VCS to ignore \code{library} (or
#' feel free to check it in if you don't mind taking up some extra space in your
#' repository).
#' 
#' @examples
#' \dontrun{
#' # Create a new packrat project from an existing directory of R code
#' bootstrap()
#' 
#' # Install a package and take a snapshot of the new state of the library
#' install.packages("TTR")
#' snapshot()
#' 
#' # Accidentally remove a package and restore to add it back
#' remove.packages("TTR")
#' restore()
#' }
#' 
#' @docType package
#' @name packrat
#' @author RStudio, Inc.
NULL

#' Initialize Packrat on a new or existing R project
#' 
#' Given an application directory, makes a new packrat project in the directory.
#' 
#' \code{bootstrap} works as follows: \enumerate{ \item Application dependencies
#' are computed by examining the R code as described in
#' \code{\link{appDependencies}}.
#' 
#' \item A snapshot is taken of the version of each package currently used by 
#' the application as described in \code{\link{snapshot}}, and each package's 
#' sources are downloaded.
#' 
#' \item A private library is created in the directory.
#' 
#' \item The snapshot is applied to the directory as described in 
#' \code{\link{restore}}. } When \code{bootstrap} is finished, all the packages
#' on which the project depends are installed in a new, private library located
#' inside the project directory.
#' 
#' You must restart your R session in the given application directory after 
#' running \code{bootstrap} in order for the changes to take effect.
#' 
#' When R is started in the directory, it will use the new, private library by
#' default whenever you make library changes (using 
#' \code{\link{install.packages}}, etc.). You can sync this private library with
#' packrat using \code{\link{snapshot}} and \code{\link{restore}}.
#' 
#' @param appDir The directory that contains the R project.
#' @param sourcePackagePaths List of paths to unpacked R package source 
#'   directories.  Use this argument only if your project depends on packages
#'   that are not available on CRAN or Github.
#'   
#' @seealso \link{packrat} for a description of the files created by
#'   \code{bootstrap}.
#'   
#' @export
bootstrap <- function(appDir = '.', sourcePackagePaths = character()) {
  appDir <- normalizePath(appDir, winslash='/', mustWork=TRUE)
  
  if (nzchar(Sys.getenv("R_PACKRAT"))) {
    stop("This project is already running under packrat!")
  }
  
  descriptionFile <- file.path(appDir, 'DESCRIPTION')
  
  if (file.exists(descriptionFile)) {
    description <- as.data.frame(read.dcf(descriptionFile))
    package <- description$Package
    if (!is.null(package)) {
      stop("This project appears to be an R package. Packrat doesn't work on ",
           "packages.")
    }
  }
  
  # Get the inferred set of dependencies and take a snapshot
  inferredDependencies <- appDependencies(appDir)
  sourcePackages <- getSourcePackageInfo(sourcePackagePaths)
  snapshotImpl(appDir, available.packages(contrib.url(activeRepos())),
               sourcePackages=sourcePackages, lib.loc = NULL, ignore.stale=TRUE)
  
  # Use the lockfile to copy sources and install packages to the library
  restore(appDir, overwriteDirty=TRUE)
  
  # Write the .Rprofile and .Renviron files
  packify(appDir)
  
  invisible()
}

#' Apply the most recent snapshot to the library
#'
#' Applies the most recent snapshot to the project's private library. 
#'
#' \code{restore} works by adding, removing, and changing packages so that the
#' set of installed packages and their versions matches the snapshot exactly.
#' 
#' There are three common use cases for \code{restore}: 
#' \itemize{
#'   \item \strong{Hydrate}: Use \code{restore} after copying a project to a new
#' machine to populate the library on that machine. 
#' 
#'   \item \strong{Sync}: Use \code{restore} to apply library changes made by a
#' collaborator to your own library. (In general, you want to run \code{restore}
#' whenever you pick up a change to \code{packrat.lock}) 
#' 
#'   \item \strong{Rollback}: Use \code{restore} to undo accidental changes made
#' to the library since the last snapshot.
#' }
#' 
#' \code{restore} cannot make changes to packages that are currently loaded. If 
#' changes are necessary to currently loaded packages, you will need to restart
#' R to apply the changes (\code{restore} will let you know when this is
#' necessary). It is recommended that you do this as soon as possible, because
#' any library changes made between running \code{restore} and restarting R will
#' be lost.
#' 
#' @note
#' \code{restore} can be destructive; it will remove packages that were not in
#' the snapshot, and it will replace newer packages with older versions if
#' that's what the snapshot indicates. \code{restore} will warn you before
#' attempting to remove or downgrade a package (if \code{prompt} is
#' \code{TRUE}), but will always perform upgrades and new installations without
#' prompting.
#' 
#' \code{restore} works only on the private package library created by packrat;
#' if you have other libraries on your path, they will be unaffected.
#' 
#' @param appDir The directory that contains the packrat project.
#' @param overwriteDirty A dirty package is one that has been changed since the
#' last snapshot or restore. Packrat will leave these alone by default. If you
#' want to guarantee that \code{restore} will put you in the exact state
#' represented by the snapshot being applied, use \code{overwriteDirty = TRUE}.
#' @param prompt \code{TRUE} to prompt before performing potentially destructive
#' changes (package removals or downgrades); \code{FALSE} to perform these
#' operations without confirmation.
#'
#' @seealso
#' \code{\link{snapshot}}, the command that creates the snapshots applied with
#' \code{restore}.
#' 
#' \code{\link{status}} to view the differences between the most recent snapshot
#' and the library.
#' 
#' @export
restore <- function(appDir = '.', overwriteDirty = FALSE, 
                    prompt = interactive()) {
  appDir <- normalizePath(appDir, winslash='/', mustWork=TRUE)
  
  # RTools cp.exe (invoked during installation) can warn on Windows since we
  # use paths of the format c:/foo/bar and it prefers /cygwin/c/foo/bar. 
  # Unfortunately, R's implementation of tar treats this warning output as
  # though it were part of the list of files in the archive. 
  cygwin <- Sys.getenv("CYGWIN")
  if (length(grep("nodosfilewarning", cygwin)) == 0) {
    Sys.setenv("CYGWIN" = paste(cygwin, "nodosfilewarning"))
    on.exit(Sys.setenv("CYGWIN" = cygwin), add = TRUE)
  }
  
  packages <- lockInfo(appDir)
  r_version <- lockInfo(appDir, 'r_version')
  if (!identical(as.character(getRversion()), r_version)) {
    warning('The most recent snapshot was generated using R version ',
            r_version)
  }
  
  # Make sure the library directory exists 
  libDir <- libdir(appDir)
  if (!file.exists(libDir)) {
    dir.create(libDir, recursive=TRUE)
  }
  
  # See if any of the packages that are currently in the library are dirty. 
  # Dirty packages that are represented in the snapshot will be either ignored 
  # (with a message) or overwritten, depending on the value of the
  # overwriteDirty flag. Dirty packages that are not represented in the snapshot
  # (like git untracked) will be silently ignored in all cases.
  
  libPkgNames <- rownames(installed.packages(libDir, noCache=TRUE))
  dirty <- !installedByPackrat(libPkgNames, libDir, TRUE)
  dirtyPackageNames <- libPkgNames[dirty]

  if (!isTRUE(overwriteDirty)) {
    prettyPrint(
      packages[pkgNames(packages) %in% dirtyPackageNames],
      'The following packages were not installed by packrat and will be ignored:',
      'If you would like to overwrite them, call restore again with\noverwriteDirty = TRUE.'
    )
    # Keep all dirty packages
    pkgsToIgnore <- dirtyPackageNames
  } else {
    # Even if overwriteDirty is TRUE, we still want to keep packages that are
    # dirty and NOT represented in the list of packages to install (this is akin 
    # to "untracked" files in git).
    pkgsToIgnore <- dirtyPackageNames[!dirtyPackageNames %in% pkgNames(packages)]
  }

  # Install each package from CRAN or github, from binaries when available and 
  # then from sources.
  repos <- lockInfo(appDir, 'repos')
  restoreImpl(appDir, repos, packages, libDir,
              pkgsToIgnore = pkgsToIgnore, prompt = prompt)
}

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
#' @param appDir The directory that contains the R project.
#' @param lib.loc The library to examine. Defaults to the private library
#' associated with the application directory.
#' @param quiet \code{TRUE} to suppress output, \code{FALSE} (the default) to
#' show output.
#' 
#' @export
status <- function(appDir = '.', lib.loc = libdir(appDir), quiet = FALSE) {
  appDirDefault <- identical(appDir, '.')
  appDir <- normalizePath(appDir, winslash='/', mustWork=TRUE)

  ### Step 1: Collect packages from three sources: packrat.lock, code inspection
  ### (using lib.loc and .libPaths() to find packages/dependencies), and by
  ### enumerating the packages in lib.loc.
  
  # Packages from the lockfile
  packratPackages <- lockInfo(appDir, fatal=FALSE)
  if (length(packratPackages) == 0) {
    bootstrapArg <- if (appDirDefault) '' else deparse(appDir)
    message('This directory does not appear to be using packrat.\n',
            'Call packrat::bootstrap(', bootstrapArg, ') to initialize ',
            'packrat.')
    return(invisible())
  }
  packratNames <- pkgNames(flattenPackageRecords(packratPackages))
  
  # We're about to go sniffing through the code, looking for package
  # dependencies. As we do so, we may encounter missing packages--either direct
  # or indirect dependencies may be missing. Collect these dependencies in this
  # vector, and later we'll let the user know they are missing.
  missingPackageNames <- character(0)
  
  # Packages that are inferred from the code
  allAppPackageNames <- pkgNames(flattenPackageRecords(getPackageRecords(
    appDependencies(appDir), NULL, lib.loc = c(lib.loc, .libPaths()),
    missing.package = function(pkgName, lib.loc) {
      missingPackageNames <<- c(missingPackageNames, pkgName)
      return(NULL)
    })))
  # Don't include names of packages that are represented in packrat; those are
  # displayed separately.
  missingPackageNames <- sort(setdiff(unique(missingPackageNames), packratNames))
  
  # (Non-recursive) packages from the library--i.e., installed packages
  libPackages <- getPackageRecords(
    as.vector(installed.packages(lib.loc, noCache=TRUE)[,'Package']), NULL,
    lib.loc=lib.loc, recursive=FALSE)
  libPackageNames <- pkgNames(libPackages)
  
  ### Step 2: Now that we have all the packages, we'll figure out which of them
  ### are dirty, are orphans, and/or don't have consistent versions between the
  ### lockfile and the library.

  # Orphan: Exists in the library, not in the code.
  orphanNames <- setdiff(pkgNames(libPackages), allAppPackageNames)
  # Dirty: Not installed by packrat.
  # TODO: Should non-lib.loc (e.g. non-installed recommended) be included in dirtyNames?
  dirtyNames <- libPackageNames[!installedByPackrat(libPackageNames, lib.loc, FALSE)]
  # Changed: Different versions represented in lockfile vs. library (including
  # added/removed packages)
  diffs <- diff(packratPackages, libPackages)
  changedNames <- names(diffs[!is.na(diffs)])
  
  ### Step 3: Create logical vectors indicating set membership, then use those
  ### vectors to classify each package.
  
  allNames <- sort(unique(c(packratNames, allAppPackageNames, libPackageNames)))
  isChanged <- allNames %in% changedNames
  #isInstalled <- allNames %in% libPackageNames
  isDirty <- allNames %in% dirtyNames
  isOrphan <- allNames %in% orphanNames
  
  # classify will have NA, restore, snapshot, and clean as possible values; the
  # rownames will be package names
  classify <- as.character(rep.int(NA, length(allNames)))
  names(classify) <- allNames
  
  # It's intentional that each assignment may overwrite previously written
  # values (e.g. in the case that something is both dirty and an orphan, it will
  # be classified as "clean").
  classify[isChanged] <- 'restore'
  classify[isDirty] <- 'snapshot'
  classify[isOrphan] <- 'clean'
  
  ### Step 4: Print the results
  
  if (!quiet) {
    fetch <- function(classification) {
      names(subset(classify, classify == classification))
    }
    
    prettyPrintPair(
      searchPackages(packratPackages, fetch('restore')),
      searchPackages(libPackages, fetch('restore')),
      "The following packages are missing from your library, or are out of date:",
      "Use packrat::restore() to install/remove the appropriate packages.",
      "packrat",
      "library"
    )
    
    prettyPrintNames(
      missingPackageNames,
      c("The following packages are referenced in your code, but are not present\n",
        "in your library nor in packrat:"),
      c("You will need to install these packages manually, then use\n",
        "packrat::snapshot() to record these packages in packrat.")
    )

    prettyPrintPair(
      searchPackages(libPackages, fetch('snapshot')),
      searchPackages(packratPackages, fetch('snapshot')),
      c("The following packages have been updated in your library, but have not\n",
        "been recorded in packrat:"),
      c("Use packrat::snapshot() to record these packages in packrat."),
      "library",
      "packrat"
    )
    
    prettyPrint(
      searchPackages(libPackages, fetch('clean')),
      "The following packages are installed but not needed:",
      c("Use packrat::clean() to remove them. Or, if they are actually needed\n",
        "by your project, add `library(packagename)` calls to a .R file\n",
        "somewhere in your project.")
    )
  }
  
  if (!any(isChanged) && !any(isDirty) && !any(isOrphan)) {
    message('Up to date.')
    return(invisible(TRUE))
  }

  return(invisible(FALSE))
}

extractVersions <- function(packages, packageNames) {
  as.character(lapply(
    searchPackages(packages, packageNames),
    function(pkg) {
      if (is.null(pkg))
        return(NA)
      else
        return(pkg$version)
    }
  ))
}

prettyPrint <- function(packages, header, footer = NULL) {
  if (length(packages) > 0) {
    cat('\n')
    if (!is.null(header)) {
      cat(paste(header, collapse=''))
      cat('\n')
    }
    print.simple.list(lapply(packages, function(pkg) {
      result <- ifelse(is.na(pkg$version), '', pkg$version)
      result <- paste(" ", result)
      names(result) <- paste("   ", pkg$name)
      result
    }))
    if (!is.null(footer)) {
      cat(paste(footer, collapse=''))
    }
    cat('\n')
  }
}

summarizeDiffs <- function(diffs, pkgsA, pkgsB, addMessage, 
                           removeMessage, upgradeMessage, downgradeMessage, 
                           crossgradeMessage)
{
  prettyPrint(
    searchPackages(pkgsB, names(diffs)[!is.na(diffs) & diffs == 'add']),
    addMessage
  )
  prettyPrint(
    searchPackages(pkgsA, names(diffs)[!is.na(diffs) & diffs == 'remove']),
    removeMessage
  )
  prettyPrintPair(
    searchPackages(pkgsA, names(diffs)[!is.na(diffs) & diffs == 'upgrade']),
    searchPackages(pkgsB, names(diffs)[!is.na(diffs) & diffs == 'upgrade']),
    upgradeMessage
  )
  prettyPrintPair(
    searchPackages(pkgsA, names(diffs)[!is.na(diffs) & diffs == 'downgrade']),
    searchPackages(pkgsB, names(diffs)[!is.na(diffs) & diffs == 'downgrade']),
    downgradeMessage
  )
  prettyPrintPair(
    searchPackages(pkgsA, names(diffs)[!is.na(diffs) & diffs == 'crossgrade']),
    searchPackages(pkgsB, names(diffs)[!is.na(diffs) & diffs == 'crossgrade']),
    crossgradeMessage
  )
}

prettyPrintPair <- function(packagesFrom, packagesTo, header, footer = NULL,
                            fromLabel = 'from', toLabel = 'to') {
  
  if (length(packagesFrom) != length(packagesTo)) {
    stop('Invalid arguments--package record lengths mismatch')
  }
  
  if (length(packagesFrom) > 0) {
    if (any(sapply(packagesFrom, is.null) & sapply(packagesTo, is.null))) {
      stop('Invalid arguments--NULL packages')
    }
    for (i in seq_along(packagesFrom)) {
      if (!is.null(packagesFrom[[i]]) && !is.null(packagesTo[[i]])) {
        if (!identical(packagesFrom[[i]]$name , packagesTo[[i]]$name)) {
          stop('Invalid arguments--package names did not match')
        }
      }
    }

    cat('\n')
    if (!is.null(header)) {
      cat(paste(header, collapse=''))
      cat('\n')
    }
    
    pickVersion <- pick("version", defaultValue="NA")
    df <- data.frame(paste(" ", sapply(packagesFrom, pickVersion)),
                     paste(" ", sapply(packagesTo, pickVersion)))
    names(df) <- c(paste(" ", fromLabel), paste(" ", toLabel))
    row.names(df) <- paste("   ", pkgNames(packagesFrom))
    print(df)

    if (!is.null(footer)) {
      cat(paste(footer, collapse=''))
    }
    cat('\n')
  }
}

prettyPrintNames <- function(packageNames, header, footer = NULL) {
  if (length(packageNames) > 0) {
    cat('\n')
    if (!is.null(header)) {
      cat(paste(header, collapse=''))
      cat('\n')
    }
    cat(paste("    ", packageNames, sep = '', collapse = '\n'))
    cat('\n')
    if (!is.null(footer)) {
      cat(paste(footer, collapse=''))
    }
    cat('\n')
  }
}

#' Remove unused packages
#' 
#' Remove unused packages from the given library.
#' 
#' Detects and removes orphaned packages. Orphaned packages are those that meet
#' the following criteria: 
#' \itemize{
#'   \item Installed in the library
#'   \item Not directly used by any R code in the application
#'   \item Not a dependency of any non-orphaned package
#' }
#' If \code{clean} wants to remove a package but your project actually needs the
#' package, add a statement such as \code{\link{require}(package-name)} to any .R
#' file in your project's directory. 
#'
#' @param appDir Directory containing application. Defaults to current working
#' directory.
#' @param lib.loc The library to clean. Defaults to the private package library
#' associated with the application directory.
#' @param prompt \code{TRUE} to prompt before removing packages, \code{FALSE} to
#' remove packages immediately.
#' 
#' @seealso \code{\link{appDependencies}} for an explanation of how dependencies are detected.
#'
#' @export
clean <- function(appDir = ".", lib.loc = libdir(appDir),
                  prompt = interactive()) {

  appDir <- normalizePath(appDir, winslash='/', mustWork=TRUE)

  rootDeps <- appDependencies(appDir)
  missingPackageNames <- character(0)
  packagesInUse <- getPackageRecords(
    rootDeps, available=NULL, sourcePackages=NULL, recursive=TRUE,
    lib.loc=c(lib.loc, .libPaths()),
    missing.package = function(pkgName, lib.loc) {
      missingPackageNames <<- c(missingPackageNames, pkgName)
      return(NULL)
    }
  )
  missingPackageNames <- sort(unique(missingPackageNames))
  
  prettyPrintNames(
    missingPackageNames,
    c("Can't detect orphaned packages because these package(s) are not installed:")
  )
  if (length(missingPackageNames) > 0) {
    return(invisible())
  }
  
  installedPkgNames <- row.names(installed.packages(
    lib.loc=lib.loc, priority=c('NA', 'recommended'), noCache=TRUE))
  
  orphans <- setdiff(installedPkgNames, 
                     pkgNames(flattenPackageRecords(packagesInUse)))

  if (length(orphans) > 0) {
    orphanRecs <- getPackageRecords(orphans, available=NULL,
                                    sourcePackages=NULL,
                                    recursive=FALSE,
                                    lib.loc=lib.loc)
    
    prettyPrint(orphanRecs,
                'The following packages will be removed:')
    
    if (prompt) {
      answer <- readline('Do you want to continue? [Y/n] ')
      answer <- gsub('^\\s*(.*?)\\s*$', '\\1', answer)
      if (nzchar(answer) && tolower(answer) != 'y') {
        return(invisible())
      }
    }
    
    removePkgs(appDir, orphans, lib.loc)
    return(invisible(orphans))
  }
}

wipe <- function(appDir = getwd()) {

  deaugmentFile(file.path(appDir, ".Rprofile"))
  deaugmentFile(file.path(appDir, ".Renviron"))
  
  files <- "packrat.lock"
  dirs <- c("library", "library.old", "library.new", "packrat.sources")
  
  # Clean up dependency information
  unlink(file.path(appDir, files))
  # Clean up downloaded sources and library directories
  unlink(file.path(appDir, dirs), recursive = TRUE)
  
  return(invisible())
}

#' Install packrat startup directives
#' 
#' Install .Rprofile and .Renviron files in the given directory to make it
#' use a private package library.
#' 
#' Packrat uses entries in the \code{.Rprofile} and \code{.Renviron} files to keep
#' package library operations (such as \code{\link{install.packages}}) local to
#' the project's local library. 
#' 
#' It is not normally necessary to call \code{packify} directly; these files are
#' normally installed by \code{\link{bootstrap}}. \code{packify} can be used to
#' restore the files if they are missing (for instance, if they were not added to
#' source control, or were accidentally removed). 
#' 
#' You'll need to restart R in the specified directory after running
#' \code{packify} in order to start using the private package library.
#' 
#' @param dir The directory in which to install .Rprofile and .Renviron files.
#' 
#' @export
packify <- function(dir = '.') {
  dir <- normalizePath(dir, winslash='/', mustWork = TRUE)
  rprofile <- file.path(dir, '.Rprofile')
  renviron <- file.path(dir, '.Renviron')
  
  augmentFile(system.file('Rprofile', package='packrat'), rprofile, TRUE)
  augmentFile(system.file('Renviron', package='packrat'), renviron, FALSE)
  
  message('Packrat startup directives installed. Please quit and restart your R session.')
  
  invisible()
}

# Add the contents of srcFile into targetFile, with "magic" comments bracketing
# the contents. Should be safe to run multiple times--each subsequent call will
# replace the contents between magic comments.
augmentFile <- function(srcFile, targetFile, preferTop) {
  header <- '# -- BEGIN PACKRAT --\n'
  footer <- '# -- END PACKRAT --'
  headerFooterRegex <- '# -- BEGIN PACKRAT --\\s*\n.*?# -- END PACKRAT --'
  emptyHeaderFooter <- paste(header, footer, sep='')
  
  src <- paste(readLines(srcFile, warn=FALSE), collapse='\n')
  target <- if (file.exists(targetFile)) {
    paste(readLines(targetFile, warn=FALSE), collapse='\n')
  } else {
    ''
  }
  
  target <- gsub(headerFooterRegex,
                 emptyHeaderFooter,
                 target)
  
  if (!isTRUE(grepl(paste(header, footer, sep=''), target, fixed = TRUE))) {
    if (preferTop) {
      if (nzchar(target) > 0)
        target <- paste(emptyHeaderFooter, target, sep='\n')
      else
        target <- emptyHeaderFooter
    } else {
      if (nzchar(target) > 0)
        target <- paste(target, emptyHeaderFooter, sep='\n')
      else
        target <- emptyHeaderFooter
    }
  }
  
  target <- gsub(headerFooterRegex,
                 paste(header, src, '\n', footer, sep=''),
                 target)
  
  writeLines(target[[1]], targetFile)
  
  invisible()
}

deaugmentFile <- function(file, delete.if.empty=TRUE) {
  if (!file.exists(file))
    return()
  headerFooterRegex <- '# -- BEGIN PACKRAT --\\s*\n.*?# -- END PACKRAT --\\s*?(\n|$)'
  contents <- paste(readLines(file, warn=FALSE), collapse='\n')
  contents <- gsub(headerFooterRegex, '', contents)
  if (delete.if.empty && isTRUE(grepl('^[\r\n]*$', contents))) {
    unlink(file)
  } else {
    writeLines(contents, file)
  }
  return(invisible())
}

#' Show the path to the current private library
#'
#' Returns the path to the private package library used by packrat.
#'
#' @param appDir Directory containing application. Defaults to current working
#' directory.
#' @return A character vector containing the path to the private package library. The path
#' is not guaranteed to exist on disk.
#'
#' @note
#' The private package library is normally created by \code{\link{bootstrap}}.
#' @examples
#' # Show the library directory for the current working directory
#' libdir()
#' 
#' @export
libdir <- function(appDir = ".") {
  file.path(normalizePath(appDir, winslash='/', mustWork=TRUE), 'library', 
            R.version$platform, getRversion())
}

lockInfo <- function(appDir, property='packages', fatal=TRUE) {
  # Get and parse the lockfile
  lockFilePath <- file.path(appDir, "packrat.lock")
  if (!file.exists(lockFilePath)) {
    if (fatal) {
      stop(paste(lockFilePath, " is missing. Run packrat::bootstrap('",
                 appDir, "') to generate it.", sep = ""))
    } else {
      return(list())
    }
  }
  return(readLockFile(lockFilePath)[[property]])
}

