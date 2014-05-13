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
#' Given a project directory, makes a new packrat project in the directory.
#'
#' \code{bootstrap} works as follows: \enumerate{ \item Application dependencies
#' are computed by examining the R code as described in
#' \code{\link{appDependencies}}.
#'
#' \item A snapshot is taken of the version of each package currently used by
#' the project as described in \code{\link{snapshot}}, and each package's
#' sources are downloaded.
#'
#' \item A private library is created in the directory.
#'
#' \item The snapshot is applied to the directory as described in
#' \code{\link{restore}}. } When \code{bootstrap} is finished, all the packages
#' on which the project depends are installed in a new, private library located
#' inside the project directory.
#'
#' You must restart your R session in the given project directory after
#' running \code{bootstrap} in order for the changes to take effect.
#'
#' When R is started in the directory, it will use the new, private library by
#' default whenever you make library changes (using
#' \code{\link{install.packages}}, etc.). You can sync this private library with
#' packrat using \code{\link{snapshot}} and \code{\link{restore}}.
#'
#' @param projDir The directory that contains the R project.
#' @param sourcePackagePaths List of paths to unpacked R package source
#'   directories.  Use this argument only if your project depends on packages
#'   that are not available on CRAN or Github.
#'
#' @seealso \link{packrat} for a description of the files created by
#'   \code{bootstrap}.
#'
#' @export
bootstrap <- function(projDir = '.', sourcePackagePaths = character()) {
  projDir <- normalizePath(projDir, winslash='/', mustWork=TRUE)

  if (nzchar(Sys.getenv("R_PACKRAT"))) {
    stop("This project is already running under packrat!")
  }

  descriptionFile <- file.path(projDir, 'DESCRIPTION')

  if (file.exists(descriptionFile)) {
    description <- as.data.frame(read.dcf(descriptionFile))
    package <- description$Package
    if (!is.null(package)) {
      stop("This project appears to be an R package. Packrat doesn't work on ",
           "packages.")
    }
  }

  # Take a snapshot
  sourcePackages <- getSourcePackageInfo(sourcePackagePaths)
  snapshotImpl(projDir, available.packages(contrib.url(activeRepos(projDir))),
               sourcePackages=sourcePackages, lib.loc = NULL, ignore.stale=TRUE)

  # Use the lockfile to copy sources and install packages to the library
  restore(projDir, overwriteDirty=TRUE)

  # Write the .Rprofile and .Renviron files
  packify(projDir)

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
#' @param projDir The directory that contains the packrat project.
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
restore <- function(projDir = '.', overwriteDirty = FALSE,
                    prompt = interactive()) {
  projDir <- normalizePath(projDir, winslash='/', mustWork=TRUE)

  # RTools cp.exe (invoked during installation) can warn on Windows since we
  # use paths of the format c:/foo/bar and it prefers /cygwin/c/foo/bar.
  # Unfortunately, R's implementation of tar treats this warning output as
  # though it were part of the list of files in the archive.
  cygwin <- Sys.getenv("CYGWIN", unset = NA)
  if (!is.na(cygwin) && length(grep("nodosfilewarning", cygwin)) == 0) {
    Sys.setenv("CYGWIN" = paste(cygwin, "nodosfilewarning"))
    on.exit(Sys.setenv("CYGWIN" = cygwin), add = TRUE)
  }

  packages <- lockInfo(projDir)
  r_version <- lockInfo(projDir, 'r_version')
  if (!identical(as.character(getRversion()), r_version)) {
    warning('The most recent snapshot was generated using R version ',
            r_version)
  }

  # Make sure the library directory exists
  libDir <- libdir(projDir)
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
  repos <- lockInfo(projDir, 'repos')
  repos <- strsplit(repos, "\\s*,\\s*")[[1]]
  restoreImpl(projDir, repos, packages, libDir,
              pkgsToIgnore = pkgsToIgnore, prompt = prompt)
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
#'   \item Not directly used by any R code in the project
#'   \item Not a dependency of any non-orphaned package
#' }
#' If \code{clean} wants to remove a package but your project actually needs the
#' package, add a statement such as \code{\link{require}(package-name)} to any .R
#' file in your project's directory.
#'
#' @param projDir The project directory. Defaults to current working
#' directory.
#' @param lib.loc The library to clean. Defaults to the private package library
#' associated with the project directory.
#' @param prompt \code{TRUE} to prompt before removing packages, \code{FALSE} to
#' remove packages immediately.
#'
#' @seealso \code{\link{appDependencies}} for an explanation of how dependencies are detected.
#'
#' @export
clean <- function(projDir = ".", lib.loc = libdir(projDir),
                  prompt = interactive()) {

  projDir <- normalizePath(projDir, winslash='/', mustWork=TRUE)

  rootDeps <- appDependencies(projDir)
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

    removePkgs(projDir, orphans, lib.loc)
    return(invisible(orphans))
  }
}

wipe <- function(projDir = getwd()) {

  deaugmentFile(file.path(projDir, ".Rprofile"))
  deaugmentFile(file.path(projDir, ".Renviron"))

  files <- "packrat.lock"
  dirs <- c("library", "library.old", "library.new", "packrat.sources")

  # Clean up dependency information
  unlink(file.path(projDir, files))
  # Clean up downloaded sources and library directories
  unlink(file.path(projDir, dirs), recursive = TRUE)

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

  cat('Packrat startup directives installed.',
      'Please quit and restart your R session.',
      sep = '\n')

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
#' @param projDir The project directory. Defaults to current working
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
libdir <- function(projDir = ".") {
  file.path(normalizePath(projDir, winslash='/', mustWork=TRUE), 'library',
            R.version$platform, getRversion())
}

lockInfo <- function(projDir, property='packages', fatal=TRUE) {
  # Get and parse the lockfile
  lockFilePath <- file.path(projDir, "packrat.lock")
  if (!file.exists(lockFilePath)) {
    if (fatal) {
      stop(paste(lockFilePath, " is missing. Run packrat::bootstrap('",
                 projDir, "') to generate it.", sep = ""))
    } else {
      return(list())
    }
  }
  return(readLockFile(lockFilePath)[[property]])
}

