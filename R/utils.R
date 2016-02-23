silent <- function(expr) {
  suppressWarnings(suppressMessages(
    capture.output(result <- eval(expr, envir = parent.frame()))
  ))
  result
}

forceUnload <- function(pkg) {

  if (!startswith(pkg, "package:"))
    pkg <- paste("package", pkg, sep = ":")

  # force detach from search path
  if (pkg %in% search())
    detach(pkg, character.only = TRUE, unload = TRUE, force = TRUE)

  # unload DLL if there is one
  pkgName <- gsub("package:", "", pkg, fixed = TRUE)
  pkgDLL <- getLoadedDLLs()[[pkgName]]
  if (!is.null(pkgDLL)) {
    suppressWarnings({
      pkgDir <- system.file(package = pkgName)
      if (nzchar(pkgDir))
        try(library.dynam.unload(pkgName, pkgDir), silent = TRUE)
    })
  }

  # unload the namespace if it's still loaded
  if (pkgName %in% loadedNamespaces()) {
    unloadNamespace(pkgName)
  }

}

list_files <- function(path = ".", pattern = NULL, all.files = FALSE,
                       full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
                       include.dirs = FALSE, no.. = TRUE) {

  files <- list.files(path = path, pattern = pattern, all.files = all.files,
                      full.names = full.names, recursive = recursive,
                      ignore.case = ignore.case, include.dirs = include.dirs, no.. = no..)

  dirs <- list.dirs(path = path, full.names = full.names, recursive = recursive)
  setdiff(files, dirs)

}

# wrapper around read.dcf to workaround LC_CTYPE bug
# (see: http://r.789695.n4.nabble.com/Bug-in-read-dcf-all-TRUE-td4690578.html)
readDcf <- function(...) {
  loc <- Sys.getlocale('LC_CTYPE')
  on.exit(Sys.setlocale('LC_CTYPE', loc))
  read.dcf(...)
}

is_dir <- function(file) {
  isTRUE(file.info(file)$isdir) ## isTRUE guards against NA (ie, missing file)
}

# Copy a directory at file location 'from' to location 'to' -- this is kludgey,
# but file.copy does not handle copying of directories cleanly
dir_copy <- function(from, to, overwrite = FALSE, all.files = TRUE,
                     pattern = NULL, ignore.case = TRUE) {

  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)

  # Make sure we're doing sane things
  if (!is_dir(from)) stop("'", from, "' is not a directory.")

  if (file.exists(to)) {
    if (overwrite) {
      unlink(to, recursive = TRUE)
    } else {
      stop(paste( sep = "",
                  if (is_dir(to)) "Directory" else "File",
                  " already exists at path '", to, "'."
      ))
    }
  }

  success <- dir.create(to, recursive = TRUE)
  if (!success) stop("Couldn't create directory '", to, "'.")

  # Get relative file paths
  files.relative <- list.files(from, all.files = all.files, full.names = FALSE,
                               recursive = TRUE, no.. = TRUE)

  # Apply the pattern to the files
  if (!is.null(pattern)) {
    files.relative <- Reduce(intersect, lapply(pattern, function(p) {
      grep(
        pattern = p,
        x = files.relative,
        ignore.case = ignore.case,
        perl = TRUE,
        value = TRUE
      )
    }))
  }

  # Get paths from and to
  files.from <- file.path(from, files.relative)
  files.to <- file.path(to, files.relative)

  # Create the directory structure
  dirnames <- unique(dirname(files.to))
  sapply(dirnames, function(x) dir.create(x, recursive = TRUE, showWarnings = FALSE))

  # Copy the files
  res <- file.copy(files.from, files.to)
  if (!all(res)) {
    # The copy failed; we should clean up after ourselves and return an error
    unlink(to, recursive = TRUE)
    stop("Could not copy all files from directory '", from, "' to directory '", to, "'.")
  }
  setNames(res, files.relative)

}

wrap <- function(x, width = 78, ...) {
  paste(strwrap(x = paste(x, collapse = " "), width = width, ...), collapse = "\n")
}

pkgDescriptionDependencies <- function(file) {

  fields <- c("Depends", "Imports", "Suggests", "LinkingTo")

  if (!file.exists(file)) stop("no file '", file, "'")
  DESCRIPTION <- readDcf(file)

  # ignore empty description
  if (nrow(DESCRIPTION) < 1)
    return(list())

  requirements <- DESCRIPTION[1, fields[fields %in% colnames(DESCRIPTION)]]

  ## Remove whitespace
  requirements <- gsub("[[:space:]]*", "", requirements)

  ## Parse packages + their version
  parsed <- vector("list", length(requirements))
  for (i in seq_along(requirements)) {
    x <- requirements[[i]]
    splat <- unlist(strsplit(x, ",", fixed = TRUE))
    res <- lapply(splat, function(y) {
      if (grepl("(", y, fixed = TRUE)) {
        list(
          Package = gsub("\\(.*", "", y),
          Version = gsub(".*\\((.*?)\\)", "\\1", y, perl = TRUE),
          Field = names(requirements)[i]
        )
      } else {
        list(
          Package = y,
          Version = NA,
          Field = names(requirements)[i]
        )
      }
    })
    parsed[[i]] <- list(
      Package = sapply(res, "[[", "Package"),
      Version = sapply(res, "[[", "Version"),
      Field = sapply(res, "[[", "Field")
    )
  }

  result <- do.call(rbind, lapply(parsed, function(x) {
    as.data.frame(x, stringsAsFactors = FALSE)
  }))

  ## Don't include 'base' packages
  ip <- installed.packages()
  basePkgs <- ip[Vectorize(isTRUE)(ip[, "Priority"] == "base"), "Package"]
  result <- result[!(result$Package %in% basePkgs), ]

  ## Don't include R
  result <- result[!result$Package == "R", ]

  result

}

# does str1 start with str2?
startswith <- function(str1, str2) {
  if (!length(str2) == 1) stop("expecting a length 1 string for 'str2'")
  sapply(str1, function(x) {
    identical(
      substr(x, 1, min(nchar(x), nchar(str2))),
      str2
    )
  })
}

# does str1 end with str2?
endswith <- function(str1, str2) {
  if (!length(str2) == 1) stop("expecting a length 1 string for 'str2'")
  n2 <- nchar(str2)
  sapply(str1, function(x) {
    nx <- nchar(x)
    identical(
      substr(x, nx - n2 + 1, nx),
      str2
    )
  })
}

stopIfNotPackified <- function(project) {

  if (!checkPackified(project, quiet = TRUE)) {
    if (identical(project, getwd())) {
      stop("This project has not yet been packified.\nRun 'packrat::init()' to init packrat.",
           call. = FALSE)
    } else {
      stop("The project at '", project, "' has not yet been packified.\nRun 'packrat::init('", project, "') to init packrat.",
           call. = FALSE)
    }
  }
}

## Expected to be used with .Rbuildignore, .Rinstignore
## .gitignore + SVN ignore have their own (similar) logical, but
## need to handle options specially
updateIgnoreFile <- function(project = NULL, file, add = NULL, remove = NULL) {

  project <- getProjectDir(project)

  ## If the file doesn't exist and we have content, create and fill it
  path <- file.path(project, file)
  if (!file.exists(path)) {
    if (length(add) > 0) {
      cat(add, file = path, sep = "\n")
    }
    return(invisible())
  }

  ## If it already exists, add and remove as necessary
  content <- readLines(path)
  content <- union(content, add)
  content <- setdiff(content, remove)
  cat(content, file = path, sep = "\n")
  return(invisible())

}

updateRBuildIgnore <- function(project = NULL) {

  add <- c(
    "^packrat/",
    "^\\.Rprofile$"
  )

  updateIgnoreFile(project = project, file = ".Rbuildignore", add = add)
}

updateGitIgnore <- function(project = NULL, options) {
  git.options <- options[grepl("^vcs", names(options))]

  names(git.options) <- swap(
    names(git.options),
    c(
      "vcs.ignore.lib" = paste0(relLibraryRootDir(), "*/"),
      "vcs.ignore.src" = paste0(relSrcDir(), "/")
    )
  )

  add <- names(git.options)[sapply(git.options, isTRUE)]
  remove <- names(git.options)[sapply(git.options, isFALSE)]

  updateIgnoreFile(project = project,
                   file = ".gitignore",
                   add = add,
                   remove = remove)
}

# A packrat project is managed by git if any one of its parent directories
# contains a '.git' folder.
isGitProject <- function(project) {
  path <- project
  while (dirname(path) != path) {
    .git <- file.path(path, ".git")
    if (file.exists(.git) && is_dir(.git))
      return(TRUE)
    path <- dirname(path)
  }
  return(FALSE)
}

isSvnProject <- function(project) {
  .svn <- file.path(project, ".svn")
  file.exists(.svn) && is_dir(.svn)
}

getSvnIgnore <- function(svn, dir) {
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dir)
  result <- system(paste(svn, "propget", "svn:ignore"), intern = TRUE)
  result[result != ""]
}

setSvnIgnore <- function(svn, dir, ignores) {
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dir)
  ignores <- paste(ignores, collapse = "\n")
  system(paste(svn, "propset", "svn:ignore", shQuote(ignores), "."), intern = TRUE)
}

updateSvnIgnore <- function(project, options) {

  svn.options <- options[grepl("^vcs", names(options))]
  names(svn.options) <- swap(
    names(svn.options),
    c(
      "vcs.ignore.lib" = relLibraryRootDir(),
      "vcs.ignore.src" = relSrcDir()
    )
  )
  add <- names(svn.options)[sapply(svn.options, isTRUE)]
  remove <- names(svn.options)[sapply(svn.options, isFALSE)]

  ## We need to explicitly exclude library.new, library.old
  add <- unique(c(add,
                  relNewLibraryDir(),
                  relOldLibraryDir()
  ))

  add <- c(add, "packrat/lib-R")

  svn <- Sys.which("svn")
  if (svn == "") {
    stop("Could not locate an 'svn' executable on your PATH")
  }
  ignores <- getSvnIgnore(svn, project)
  ignores <- union(ignores, add)
  ignores <- setdiff(ignores, remove)

  setSvnIgnore(svn, project, ignores)

}

## Wrappers over setLibPaths that do some better error reporting
setLibPaths <- function(paths) {
  for (path in paths) {
    if (!file.exists(path)) {
      stop("No directory exists at path '", path, "'")
    }
  }
  .libPaths(paths)
}

normalize_paths <- function(paths, winslash = "/", mustWork = FALSE) {
  paths[paths == ""] <- getwd()
  unlist(lapply(paths, function(path) {
    normalizePath(path, winslash = "/", mustWork = FALSE)
  }))
}

getLibPaths <- function() {
  normalize_paths(.libPaths())
}

getUserLibPaths <- function() {
  allPaths <- getLibPaths()
  sysPaths <- normalize_paths(c(.Library, .Library.site))
  setdiff(allPaths, sysPaths)
}

## Get the default library paths (those that would be used upon
## starting a new R session)
getDefaultLibPaths <- function() {
  getenv(.packrat.env$R_PACKRAT_DEFAULT_LIBPATHS)
}

getInstalledPkgInfo <- function(packages, installed.packages, ...) {
  ip <- installed.packages
  missingFromLib <- packages[!(packages %in% rownames(ip))]
  if (length(missingFromLib)) {
    warning("The following packages are not installed in the current library:\n- ",
            paste(missingFromLib, sep = ", "))
  }
  packages <- setdiff(packages, missingFromLib)
  getPkgInfo(packages, ip)
}

getPkgInfo <- function(packages, installed.packages) {

  records <- installed.packages[packages, , drop = FALSE]

  ## Convert from matrix to list
  records <- apply(records, 1, as.list)

  ## Parse the package dependency fields -- we split up the depends, imports, etc.
  for (i in seq_along(records)) {
    for (field in c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")) {
      item <- records[[i]][[field]]
      if (is.na(item)) next
      item <- gsub("[[:space:]]*(.*?)[[:space:]]*", "\\1", item, perl = TRUE)
      item <- unlist(strsplit(item, ",[[:space:]]*", perl = TRUE))

      ## Remove version info
      item <- gsub("\\(.*", "", item)
      records[[i]][[field]] <- item
    }
  }
  records
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

`%nin%` <- function(x, y) {
  !(x %in% y)
}

isFALSE <- function(x) identical(x, FALSE)

swap <- function(vec, from, to = NULL) {

  if (is.null(to)) {
    to <- unname(unlist(from))
    from <- names(from)
  }

  tmp <- to[match(vec, from)]
  tmp[is.na(tmp)] <- vec[is.na(tmp)]
  return(tmp)
}


attemptRestart <- function(..., restore.packrat.mode = TRUE) {
  restart <- getOption("restart")
  if (!is.null(restart)) {
    # set packrat mode environment variable here so that
    # the host environment knows to return to packrat
    # mode after the restart (affects how .libPaths are
    # handled during the restart)
    if (restore.packrat.mode) {
      setPackratModeEnvironmentVar()
    }
    restart(...)
    TRUE
  } else {
    FALSE
  }
}

loadedNamespacePaths <- function() {
  loadedNamespaceNames <- loadedNamespaces()
  paths <- unlist(lapply(loadedNamespaceNames, function(nm) {
    if (nm == "base") return(NA)
    ns <- asNamespace(nm)
    if (!exists(".__NAMESPACE__.", envir = ns)) return(NA)
    getNamespaceInfo(ns, "path")
  }))
  result <- data.frame(
    namespace = loadedNamespaceNames,
    dir = dirname(paths),
    path = paths
  )
  result <- result[order(result$dir), ]
  rownames(result) <- NULL
  result
}

# Work around namespace:stats potentially not being loaded
setNames <- function(object = nm, nm) {
  names(object) <- nm
  object
}

# Drop null values in a list
dropNull <- function(x) {
  Filter(Negate(is.null), x)
}

surround <- function(x, with = "'") {
  if (!length(x)) return(character())
  paste0(with, as.character(x), with)
}

write_dcf <- function(x, file = "", append = FALSE, indent = 4,
                      width = 72, keep.white = NULL, ...) {
  write.dcf(x = x, file = file, append = append, indent = indent,
            width = width, keep.white = keep.white, ...)
}

symlink <- function(from, to) {
  if (is.windows()) Sys.junction(from, to)
  else file.symlink(from, to)
}

with_dir <- function(dir, expr) {
  owd <- getwd()
  setwd(dir)
  on.exit(setwd(owd))
  eval(expr, envir = parent.frame())
}

set_collate <- function(locale) {
  cur <- Sys.getlocale(category = "LC_COLLATE")
  Sys.setlocale(category = "LC_COLLATE", locale = locale)
  cur
}

with_collate <- function(locale, code) {
  old <- set_collate(locale)
  on.exit(set_collate(old))

  force(code)
}

sort_c <- function(x) with_collate("C", sort(x))

is.string <- function(x) {
  is.character(x) && length(x) == 1
}

is.directory <- function(x) {
  file.exists(x) && isTRUE(file.info(x)[["isdir"]]) # guard against NA
}

getBinaryPkgType <- function() {
  .Platform$pkgType
}

normalize.path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

filePrefix <- function() {
  if (is.windows())
    "file:///"
  else
    "file://"
}

reFilePrefix <- function() {
  paste("^", filePrefix(), sep = "")
}

# Call 'available.packages()' with an invalid URL to get the
# 'skeleton' output of 'available.packages()' (ie, an empty matrix
# with all appropriate fields populated)
availablePackagesSkeleton <- function() {

  # Use internal download file method just to ensure no errors leak.
  download.file.method <- getOption("download.file.method")
  on.exit(options(download.file.method = download.file.method), add = TRUE)
  options(download.file.method = "internal")

  # Use 'available.packages()' to query a URL that doesn't exist
  result <- withCallingHandlers(
    available.packages("/no/such/path/here/i/hope/"),
    warning = function(w) invokeRestart("muffleWarning"),
    message = function(m) invokeRestart("muffleMessage")
  )

  result
}

isProgramOnPath <- function(program) {
  nzchar(Sys.which(program)[[1]])
}

isPathToSameFile <- function(lhs, rhs) {

  if (!(is.string(lhs) && is.string(rhs)))
    return(FALSE)

  lhsNorm <- normalizePath(lhs, winslash = "/", mustWork = FALSE)
  rhsNorm <- normalizePath(rhs, winslash = "/", mustWork = FALSE)

  lhsNorm == rhsNorm

}

isTestingPackrat <- function() {
  !is.na(Sys.getenv("R_PACKRAT_TESTING", unset = NA))
}

defer <- function(expr, envir = parent.frame()) {

  # Create a call that must be evaluated in the parent frame (as
  # that's where functions and symbols need to be resolved)
  call <- substitute(
    evalq(expr, envir = envir),
    list(expr = substitute(expr), envir = parent.frame())
  )

  # Use 'do.call' with 'on.exit' to attach the evaluation to
  # the exit handlrs of the selected frame
  do.call(base::on.exit, list(substitute(call), add = TRUE), envir = envir)
}

isUsingExternalTar <- function() {

  TAR <- Sys.getenv("TAR")

  if (!nzchar(TAR))
    return(FALSE)

  if (!nzchar(Sys.which(TAR)))
    return(FALSE)

  if (identical(TAR, "internal"))
    return(FALSE)

  TRUE
}

join <- function(..., sep = "", collapse = NULL) {
  paste(..., sep = sep, collapse = collapse)
}

# sneakily get a function
yoink <- function(package, symbol) {
  eval(call(":::", package, symbol))
}

enumerate <- function(list, fn) {
  keys <- names(list)
  values <- list
  sapply(seq_along(keys), function(i) {
    fn(keys[[i]], values[[i]])
  })
}

packageVersionInstalled <- function(...) {
  enumerate(list(...), function(package, version) {
    result <- try(packageVersion(package), silent = TRUE)
    !inherits(result, "try-error") && result >= version
  })
}
