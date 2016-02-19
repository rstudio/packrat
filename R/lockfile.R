# Given a list of named lists, return a list of all the names used
collectFieldNames <- function(lists) {
  allFieldNames <- character(0)
  for (lst in lists)
    allFieldNames <- union(allFieldNames, unique(names(lst)))
  return(allFieldNames)
}

# Create a single-row data frame with the given column names and all NA values
naRow <- function(fieldNames) {
  structure(
    do.call(data.frame, as.list(rep.int(NA, length(fieldNames)))),
    names = fieldNames
  )
}

# Like rbind.data.frame but tolerates heterogeneous columns, filling in any
# missing values with NA
rbind2 <- function(df1, df2) {
  allNames <- union(names(df1), names(df2))

  missing1 <- setdiff(allNames, names(df1))
  missing2 <- setdiff(allNames, names(df2))

  return(rbind(
    cbind(df1, naRow(missing1), row.names = NULL),
    cbind(naRow(missing2), df2, row.names = NULL)
  ))
}

writeLockFile <- function(file, lockinfo) {

  rver <- as.character(getRversion())

  # Construct Repos as a key-value pair to write into the lock file
  repos <- activeRepos(dirname(file))

  # Windows automatically transforms \n to \r\n on write through write.dcf
  separator <- ",\n"
  reposString <- paste(names(repos), unname(repos), sep = "=", collapse = separator)

  # The first record contains metadata about the project and lockfile
  preamble <- data.frame(
    PackratFormat = .packrat$packratFormat,
    PackratVersion = as.character(packageVersion("packrat")),
    RVersion = rver,
    Repos = reposString
  )

  stopifnot(nrow(preamble) == 1)

  # Remaining records are about the packages
  if (length(lockinfo)) {
    packages <- flattenPackageRecords(lockinfo, depInfo = TRUE, sourcePath = TRUE)
    fieldNames <- collectFieldNames(packages)
    packageInfo <- lapply(fieldNames, function(fieldName) {
      values <- data.frame(vapply(packages, function(pkg) {
        if (!is.null(pkg[[fieldName]]))
          pkg[[fieldName]]
        else
          NA_character_
      }, character(1), USE.NAMES = FALSE))
      names(values) <- fieldName
      return(values)
    })
    packageInfoDf <- do.call(data.frame, packageInfo)
    df <- rbind2(preamble, packageInfoDf)
  } else {
    df <- as.data.frame(preamble, stringsAsFactors = FALSE)
  }

  names(df) <- translate(names(df), r_aliases)
  write_dcf(df, file)
  invisible()
}

readLockFile <- function(file) {

  df <- as.data.frame(readDcf(file), stringsAsFactors = FALSE)
  df <- cleanupWhitespace(df)

  # Used 'GitHub' instead of 'Github' for a brief period -- translate those
  names(df) <- gsub("^GitHub", "Github", names(df))

  # Translate the names according to the aliases we maintain
  names(df) <- translate(names(df), aliases)

  # Split the repos
  repos <- gsub("[\r\n]", " ", df[1, 'Repos'])
  repos <- strsplit(unlist(strsplit(repos, "\\s*,\\s*", perl = TRUE)), "=", fixed = TRUE)

  # Support older-style lockfiles containing unnamed repositories
  repoLens <- vapply(repos, length, numeric(1))
  if (all(repoLens == 1)) {
    # Support for old (unnamed) repositories

    if (length(repoLens) > 1) {
      # We warn if there were multiple repositories (if there was only one, we
      # can safely assume it was CRAN)
      warning("Old-style repository format detected; bumped to new version\n",
              "Please re-set the repositories with options(repos = ...)\n",
              "and call packrat::snapshot() to update the lock file.")
    }
    repos <- c(CRAN = repos[[1]])
  } else if (all(repoLens == 2)) {
    repos <- setNames(
      sapply(repos, "[[", 2),
      sapply(repos, "[[", 1)
    )
  }

  list(
    packrat_format = df[1, 'PackratFormat'],
    packrat_version = df[1, 'PackratVersion'],
    r_version = df[1, 'RVersion'],
    repos = repos,
    packages = deserializePackages(utils::tail(df, -1))
  )
}

# Remove leading and trailing whitespace from character vectors
# in the dataframe, and return the modified dataframe
cleanupWhitespace <- function(df) {
  for (i in seq_along(df)) {
    if (is.character(df[[i]]))
      df[[i]] <- sub('^\\s*(.*?)\\s*$', '\\1', df[[i]])
  }
  return(df)
}

# @param graph Named list where the names are the packages and the values
#   are the names of the packages that they depend on. Packages with no
#   dependencies should have character(0) or NULL.
# @return Sorted character vector of package names
topoSort <- function(graph) {
  packageNames <- names(graph)

  # Key: dependency, Value: dependent
  # Use this to answer: What things depend on this key?
  dependents <- new.env(parent = emptyenv(), size = as.integer(length(packageNames) * 1.3))
  # Key: dependent, Value: Number of dependencies
  # Use this to answer: How many things does this key depend on?
  dependencyCount <- new.env(parent = emptyenv(), size = as.integer(length(packageNames) * 1.3))
  for (packageName in packageNames)
    dependencyCount[[packageName]] <- 0

  # Initialize dependents and dependencyCount
  for (pkgName in packageNames) {
    for (r in graph[[pkgName]]) {
      dependents[[r]] <- c(dependents[[r]], pkgName)
      dependencyCount[[pkgName]] <- dependencyCount[[pkgName]] + 1
    }
  }

  if (length(setdiff(ls(dependents), packageNames)) > 0)
    stop("Corrupted lockfile: missing dependencies") # TODO: better message

  # Do topo sort
  sortedNames <- character(0)
  leaves <- packageNames[vapply(packageNames, function(pkgName) {
    identical(dependencyCount[[pkgName]], 0)
  }, logical(1))]

  while (length(leaves) > 0) {
    leaf <- leaves[[1]]
    leaves <- utils::tail(leaves, -1)

    sortedNames <- c(sortedNames, leaf)

    # See who depends on the leaf
    for (dependent in dependents[[leaf]]) {
      # Decrease the dependency count for this dependent
      dependencyCount[[dependent]] <- dependencyCount[[dependent]] - 1
      # Is this dependent now a leaf?
      if (identical(dependencyCount[[dependent]], 0)) {
        leaves <- c(leaves, dependent)
        do.call(rm, list(dependent, envir = dependencyCount))
      }
    }
    if (exists(leaf, where = dependents))
      do.call(rm, list(leaf, envir = dependents))
  }

  if (!setequal(sortedNames, packageNames))
    stop("Corrupt lockfile: circular package dependencies detected")

  sortedNames
}

deserializePackages <- function(df) {
  packageNames <- df[, 'name']

  ## Begin validation

  # Test for package records without names
  if (any(is.na(packageNames)))
    stop("Invalid lockfile format: missing package name detected")

  dupNames <- packageNames[duplicated(packageNames)]
  if (length(dupNames) > 0) {
    stop("The following package(s) appear in the lockfile more than once: ",
         paste(dupNames, collapse = ", "))
  }

  # TODO: Test that package names are valid (what are the rules?)

  ## End validation

  graph <- lapply(seq.int(nrow(df)), function(i) {
    req <- df[i, 'requires']
    if (is.null(req) || is.na(req))
      return(character(0))
    reqs <- unique(strsplit(req, '\\s*,\\s*')[[1]])
    if (identical(reqs, ''))
      return(character(0))
    return(reqs)
  })
  names(graph) <- packageNames

  # Validate graph
  undeclaredDeps <- setdiff(unique(unlist(graph)), packageNames)
  if (length(undeclaredDeps) > 0) {
    stop("The following dependencies are missing lockfile entries: ",
         paste(undeclaredDeps, collapse = ", "))
  }

  topoSorted <- topoSort(graph)

  # It's now safe to drop the requires info since it's encoded in the graph
  df <- df[, names(df) != 'requires', drop = FALSE]

  sortedPackages <- lapply(topoSorted, function(pkgName) {
    pkg <- as.list(df[df$name == pkgName,])
    pkg <- pkg[!is.na(pkg)]
    return(pkg)
  })
  names(sortedPackages) <- topoSorted

  for (i in seq_along(sortedPackages)) {
    pkg <- sortedPackages[[i]]
    pkg$depends <- lapply(graph[[pkg$name]], function(depName) {
      sortedPackages[[depName]]
    })
    sortedPackages[[i]] <- pkg
  }

  names(sortedPackages) <- NULL

  return(sortedPackages)
}

translate <- function(x, dict) {
  vapply(x, function(val) {
    if (!(val %in% names(dict)))
      val
    else
      as.vector(dict[[val]])
  }, character(1))
}

# Translates persistent names with in-memory names (i.e. the names are what the
# fields are called in the lockfile, and the values are what the fields are
# called after they've been deserialized into package records).
#
# NB: This list must be maintained if additional fields are added to package
# records!
aliases <- c(
  Package = "name",
  Source = "source",
  Version = "version",
  Requires = "requires",
  GithubRepo = "gh_repo",
  GithubUsername = "gh_username",
  GithubRef = "gh_ref",
  GithubSha1 = "gh_sha1",
  GithubSubdir = "gh_subdir",
  SourcePath = "source_path",
  Hash = "hash"
)
r_aliases <- structure(names(aliases), names = aliases)
