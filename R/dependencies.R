# Detect Application Dependencies
#
# Recursively detect all package dependencies for an application. This function
# parses all .R files in the application directory to determine what packages
# the application depends directly.
#
# Only direct dependencies are detected (i.e. no recursion is done to find the
# dependencies of the dependencies).
#
# @param project Directory containing application. Defaults to current working
#   directory.
# @return Returns a list of the names of the packages on which R code in the
#   application depends.
# @details Dependencies are determined by parsing application source code and
#   looking for calls to \code{library}, \code{require}, \code{::}, and
#   \code{:::}.
#
# @examples
#
# \dontrun{
#
# # dependencies for the app in the current working dir
# appDependencies()
#
# # dependencies for an app in another directory
# appDependencies("~/projects/shiny/app1")
# }
# @keywords internal
appDependencies <- function(project = NULL, available.packages = NULL) {

  if (is.null(available.packages)) available.packages <- available.packages()

  project <- getProjectDir(project)

  ## For R packages, we only use the DESCRIPTION file
  if (file.exists(file.path(project, "DESCRIPTION"))) {

    ## Make sure we get records recursively from the packages in DESCRIPTION
    parentDeps <-
      pkgDescriptionDependencies(file.path(project, "DESCRIPTION"))$Package

    ## For downstream dependencies, we don't grab their Suggests:
    ## Presumedly, we can build child dependencies without vignettes, and hence
    ## do not need suggests -- for the package itself, we should make sure
    ## we grab suggests, however
    childDeps <- recursivePackageDependencies(parentDeps, libDir(project),
                                              available.packages)
  } else {
    parentDeps <- setdiff(unique(c(dirDependencies(project))), "packrat")
    childDeps <- recursivePackageDependencies(parentDeps, libDir(project),
                                              available.packages)
  }

  sort(unique(c(parentDeps, childDeps, "packrat")))

}

# detect all package dependencies for a directory of files
dirDependencies <- function(dir) {
  dir <- normalizePath(dir, winslash='/')

  # first get the packages referred to in source code
  pattern <- "\\.[rR]$|\\.[rR]md$|\\.[rR]nw$|\\.[rR]pres$"
  pkgs <- character()
  R_files <- list.files(dir,
                        pattern = pattern,
                        ignore.case = TRUE,
                        recursive = TRUE
  )

  ## Avoid anything within the packrat directory itself -- all inference
  ## should be done on user code
  packratDirRegex <- paste("^", .packrat$packratFolderName, sep = "")
  R_files <- grep(packratDirRegex, R_files, invert = TRUE, value = TRUE)


  sapply(R_files, function(file) {
    filePath <- file.path(dir, file)
    pkgs <<- append(pkgs, fileDependencies(file.path(dir, file)))

  })

  unique(pkgs)
}

# detect all package dependencies for a source file (parses the file and then
# recursively examines all expressions in the file)

# ad-hoc dispatch based on the file extension
fileDependencies <- function(file) {
  fileext <- tolower(gsub(".*\\.", "", file))
  switch (fileext,
          r = fileDependencies.R(file),
          rmd = fileDependencies.Rmd(file),
          rnw = fileDependencies.Rnw(file),
          rpres = fileDependencies.Rpres(file),
          stop("Unrecognized file type '", file, "'")
  )
}

fileDependencies.knitr <- fileDependencies.Rmd <- fileDependencies.Rpres <- function(file) {
  if (require("knitr", quietly = TRUE)) {
    tempfile <- tempfile()
    on.exit(unlink(tempfile))
    tryCatch(silent(
      knitr::knit(file, output = tempfile, tangle = TRUE)
    ), error = function(e) {
      message("Unable to tangle file '", file, "'; cannot parse dependencies")
      character()
    })
    fileDependencies.R(tempfile)
  } else {
    warning("knitr is required to parse dependencies but is not available")
    character()
  }
}

fileDependencies.Rnw <- function(file) {
  tempfile <- tempfile()
  on.exit(unlink(tempfile))
  tryCatch(silent({
    utils::Stangle(file, output = tempfile)
    fileDependencies.R(tempfile)
  }), error = function(e) {
    fileDependencies.knitr(file)
  })
}

fileDependencies.R <- function(file) {

  if (!file.exists(file)) {
    warning("No file at path '", file, "'.")
    return(character())
  }

  # build a list of package dependencies to return
  pkgs <- character()

  # parse file and examine expressions
  tryCatch({
    # parse() generates a warning when the file has an incomplete last line, but
    # it still parses the file correctly; ignore this and other warnings.
    # We'll still halt when parsing fails.
    exprs <- suppressWarnings(parse(file, n = -1L))
    for (i in seq_along(exprs))
      pkgs <- append(pkgs, expressionDependencies(exprs[[i]]))
  }, error = function(e) {
    warning(paste("Failed to parse", file, "; dependencies in this file will",
                  "not be discovered."))
  })

  # return packages
  unique(pkgs)
}

# detect the package dependencies of an expression (adapted from
# tools:::.check_packages_used)
#
# expressionDependencies(quote(library("h")))
# expressionDependencies(quote(library(10, package = "h")))
# expressionDependencies(quote(library(h)))
# expressionDependencies(quote({library(h); library(g)}))
# expressionDependencies(quote(h::f))
expressionDependencies <- function(e) {
  # base case
  if (is.atomic(e) || is.name(e)) return()

  # recursive case: expression (= list of calls)
  if (is.expression(e)) {
    return(unlist(lapply(e, expressionDependencies)))
  }

  # base case: a call
  fname <- as.character(e[[1L]])
  # a refclass method call, so return
  if (length(fname) > 1) return()

  if (length(fname) == 1) {

    # base case: call to library/require
    if (fname %in% c("library", "require")) {
      mc <- match.call(get(fname, baseenv()), e)
      if (is.null(mc$package)) return(NULL)
      if (isTRUE(mc$character.only)) return(NULL)

      return(as.character(mc$package))
    }

    # base case: call to :: or :::
    if (fname %in% c("::", ":::")) (
      return(as.character(e[[2L]]))
    )

    # base case: methods functions
    if (fname %in% c("setClass", "setRefClass", "setMethod", "setGeneric")) {
      return("methods")
    }

  }

  # recursive case: all other calls
  children <- lapply(as.list(e[-1]), expressionDependencies)
  unique(unlist(children))
}

