# Package dependency:
# list(
#   name = 'ggplot2',
#   source = 'CRAN',
#   version = '0.9.3.1', # or: '>= 3.0', 'github:hadley/ggplot2/fix/axis', ''
# )

# Package record:
# list(
#   name = 'ggplot2',
#   source = 'github',
#   version = '0.9.3.1',
#   gh_repo = 'ggplot2',
#   gh_username = 'hadley',
#   gh_ref = 'master',
#   gh_sha1 = '66b81e9307793029f6083fc6108592786a564b09'
# )

# Returns a package records for the given packages
getPackageRecords <- function(pkgNames, available, sourcePackages=NULL, 
                              recursive=TRUE, lib.loc=NULL, fatal=TRUE) {
  records <- lapply(pkgNames, function(pkgName) {
    if (!is.null(sourcePackages) &&
        pkgName %in% rownames(sourcePackages)) {
      # This package was a manually specified source package; use the 
      # description file from there
      record <- structure(list(
        name = pkgName,
        source = 'source',
        version = as.character(sourcePackages[pkgName,"version"]),
        source_path = as.character(sourcePackages[pkgName,"path"])
      ), class=c('packageRecord', 'source'))
      db <- installed.packages(priority='NA')
    } else {
      # This package is from an external source (CRAN-like repo or github); 
      # attempt to get its description from the installed package database.
      pkgDescFile <- system.file('DESCRIPTION', package=pkgName, 
                                 lib.loc = lib.loc)
      if (nchar(pkgDescFile) == 0) {
        if (pkgName %in% rownames(available)) {
          # The package's DESCRIPTION doesn't exist locally--get its version and
          # dependency information from the repo, and use the database of 
          # available packages to compute its dependencies
          pkg <- available[pkgName,]
          df <- data.frame(
            Package = pkg[["Package"]],
            Version = pkg[["Version"]],
            Repository = "CRAN")
          db <- available
        } else if (fatal) {
          where <- ifelse(is.null(lib.loc), 'the current libpath', lib.loc)
          stop('The package "', pkgName, '" is not installed in ', where)
        } else {
          return(list(
            name = pkgName,
            version = NA,
            source = NA
          ))
        }
      } else {
        # This package's DESCRIPTION exists locally--read it, and use the database
        # of installed packages to compute its dependencies.
        df <- as.data.frame(read.dcf(pkgDescFile))
        db <- installed.packages(priority='NA')
      }
      record <- inferPackageRecord(df)
    }
    if (isTRUE(recursive && !is.null(record))) {
      deps <- tools::package_dependencies(
        record$name, db,
        c("Depends", "Imports", "LinkingTo"),
        recursive=FALSE
      )[[record$name]]
      record$depends <- getPackageRecords(
        deps, available, sourcePackages, TRUE, lib.loc=lib.loc, fatal=fatal)
    }
    return(record)
  })
  return(records[!sapply(records, is.null)])
}

# Reads a description file and attempts to infer where the package came from.
# Currently works only for packages installed from CRAN or from GitHub using
# devtools 1.4 or later.
inferPackageRecord <- function(df) {
  name <- as.character(df$Package)
  ver <- as.character(df$Version)
  
  if (!is.null(df$Repository) &&
      identical(as.character(df$Repository), 'CRAN')) {
    # It's CRAN!
    return(structure(list(
      name = name,
      source = 'CRAN',
      version = ver
    ), class=c('packageRecord', 'CRAN')))
  } else if (!is.null(df$GithubRepo)) {
    # It's GitHub!
    return(structure(list(
      name = name,
      source = 'github',
      version = ver,
      gh_repo = as.character(df$GithubRepo),
      gh_username = as.character(df$GithubUsername),
      gh_ref = as.character(df$GithubRef),
      gh_sha1 = as.character(df$GithubSHA1)
    ), class=c('packageRecord', 'github')))
  } else if (identical(as.character(df$Priority), 'base')) {
    # It's a base package!
    return(NULL)
  } else if (!is.null(df$biocViews)) {
    # It's Bioconductor!
    return(structure(list(
      name = name,
      source = 'Bioconductor',
      version = ver
      ), class=c('packageRecord', 'Bioconductor')))
  } else {
    warning("Couldn't figure out the origin of package ", name)
    return(structure(list(
      name = name,
      version = ver
    ), class='packageRecord'))
  }
}

# Given a list of source package paths, parses the DESCRIPTION for each and
# returns a data frame containing each (with row names given by package names)
getSourcePackageInfo <- function(sourcePackagePaths) {
  results <- data.frame()
  names <- lapply(sourcePackagePaths, function(path) {
    descPath <- file.path(path, "DESCRIPTION") 
    if (!file.exists(descPath)) {
      stop("Cannot treat ", path, " as a source package directory; ", descPath, 
           " is missing.")
    }
    desc <- as.data.frame(read.dcf(descPath))
    results <<- rbind(results, data.frame(
      name = as.character(desc$Package), 
      version = as.character(desc$Version), 
      path = normalizePath(path)))
    as.character(desc$Package)
  })
  row.names(results) <- names
  results
}

pick <- function(property, package) {
  func <- function(packageRecord) {
    packageRecord[[property]]
  }
  if (!missing(package)) {
    return(func(package))
  } else {
    return(func)
  }
}

# If called without a second argument, returns a curried function. If called
# with a second argument then it returns the package without the indicated
# properties.
strip <- function(properties, package) {
  func <- function(packageRecord) {
    packageRecord[!names(packageRecord) %in% properties]
  }
  if (!missing(package)) {
    return(func(package))
  } else {
    return(func)
  }
}

#' Returns a character vector of package names. Depends are ignored.
#' 
#' @keyword internal
pkgNames <- function(packageRecords) {
  sapply(packageRecords, pick("name"))
}

#' Filters out all record properties except name and version. Dependencies
#' are dropped.
#' 
#' @keyword internal
pkgNamesAndVersions <- function(packageRecords) {
  lapply(packageRecords, function(pkg) {
    pkg[names(pkg) %in% c('name', 'version')]
  })
}

#' Recursively filters out all record properties except name, version, and
#' depends.
#' 
#' @keyword internal
pkgNamesVersDeps <- function(packageRecords) {
  lapply(packageRecords, function(pkg) {
    pkg <- pkg[names(pkg) %in% c('name', 'version', 'depends')]
    pkg$depends <- pkgNamesVersDeps(pkg$depends)
    return(pkg)
  })
}

# Searches package records recursively looking for packages
searchPackages <- function(packages, packageNames) {
  lapply(packageNames, function(pkgName) {
    for (pkg in packages) {
      if (pkg$name == pkgName)
        return(pkg)
      if (!is.null(pkg$depends)) {
        found <- searchPackages(pkg$depends, pkgName)[[1]]
        if (!is.null(found))
          return(found)
      }
    }  
    return(NULL)
  })
}

#' Returns a linear list of package records, sorted by name, with all
#' dependency information removed
#' 
#' @keyword internal
flattenPackageRecords <- function(packageRecords) {
  visited <- new.env(parent=emptyenv())
  visit <- function(pkgRecs) {
    for (rec in pkgRecs) {
      visit(rec$depends)
      rec['depends'] <- NULL
      visited[[rec$name]] <- rec
    }
  }
  visit(packageRecords)
  lapply(sort(ls(visited)), function(name) {
    visited[[name]]
  })
}

# States: NA (unchanged), remove, add, upgrade, downgrade, crossgrade
# (crossgrade means name and version was the same but something else was
# different, i.e. different source or GitHub SHA1 hash or something)

diff <- function(packageRecordsA, packageRecordsB) {
  removed <- pkgNameDiff(packageRecordsA, packageRecordsB)
  removed <- structure(rep.int('remove', length(removed)),
                       names = removed)
  
  added <- pkgNameDiff(packageRecordsB, packageRecordsA)
  added <- structure(rep.int('add', length(added)),
                     names = added)
  
  both <- pkgNameIntersect(packageRecordsA, packageRecordsB)
  both <- structure(
    sapply(both, function(pkgName) {
      pkgA <- searchPackages(packageRecordsA, pkgName)[[1]]
      pkgB <- searchPackages(packageRecordsB, pkgName)[[1]]
      
      if (identical(strip('depends', pkgA), strip('depends', pkgB)))
        return(NA)
      verComp <- compareVersion(pkgA$version, pkgB$version)
      if (verComp < 0)
        return('upgrade')
      else if (verComp > 0)
        return('downgrade')
      else
        return('crossgrade')
    }),
    names = both
  )
  
  return(c(removed, added, both))
}

pkgNameIntersect <- function(packageRecordsA, packageRecordsB) {
  a <- pkgNames(flattenPackageRecords(packageRecordsA))
  b <- pkgNames(flattenPackageRecords(packageRecordsB))
  intersect(a, b)
}

pkgNameDiff <- function(packageRecordsA, packageRecordsB) {
  a <- pkgNames(flattenPackageRecords(packageRecordsA))
  b <- pkgNames(flattenPackageRecords(packageRecordsB))
  setdiff(a, b)
}
