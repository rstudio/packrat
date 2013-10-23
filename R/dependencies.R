#' Detect Application Dependencies
#' 
#' Recursively detect all package dependencies for an application. This function
#' parses all .R files in the application directory to determine what packages 
#' the application depends directly.
#' 
#' Only direct dependencies are detected (i.e. no recursion is done to find the 
#' dependencies of the dependencies).
#' 
#' @param appDir Directory containing application. Defaults to current working 
#'   directory.
#' @return Returns a list of the names of the packages on which R code in the
#'   application depends.
#' @details Dependencies are determined by parsing application source code and 
#'   looking for calls to \code{library}, \code{require}, \code{::}, and 
#'   \code{:::}.
#'   
#' @examples
#' \dontrun{
#' 
#' # dependencies for the app in the current working dir
#' appDependencies()
#' 
#' # dependencies for an app in another directory
#' appDependencies("~/projects/shiny/app1")
#' }
appDependencies <- function(appDir = ".") {
  unique(c(dirDependencies(appDir), 'packrat'))
}

# does str1 start with str2?
startswith <- function(str1, str2) {
  identical(substr(str1, 1, min(nchar(str1), nchar(str2))), str2)
}
  
# detect all package dependencies for a directory of files
dirDependencies <- function(dir) {
  libdir <- libdir(dir)
  dir <- normalizePath(dir, winslash='/')

  # first get the packages referred to in source code
  pkgs <- character()
  sapply(list.files(dir, pattern=glob2rx("*.R"), 
                    ignore.case=TRUE, recursive=TRUE),
         function(file) {
           # ignore files in the library directory 
           if (!startswith(file.path(dir, file), libdir)) 
             pkgs <<- append(pkgs, fileDependencies(file.path(dir, file)))
         })
  unique(pkgs)  
}

# detect all package dependencies for a source file (parses the file and then
# recursively examines all expressions in the file)
fileDependencies <- function(file) {
  # build a list of package dependencies to return
  pkgs <- character()
  
  # parse file and examine expressions
  tryCatch({
    exprs <- parse(file, n = -1L) 
    for (i in seq_along(exprs))
      pkgs <- append(pkgs, expressionDependencies(exprs[[i]]))
  }, error = function(e) {
    warning(paste("Failed to parse", file, "; dependencies in this file will",
                  "not be discovered."))
  })
  
  # return packages
  unique(pkgs)
}

# detect the pacakge dependencies of an expression (adapted from 
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
    unlist(lapply(e, expressionDependencies))
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

# Given a subtree of package dependencies and an environment containing the 
# names of packages to be considered already installed, return an ordered list
# of packages from the subtree to be installed.
makePkgInstallList <- function(packageDepSubtree, installed) {
  installList <- list()
  for (package in packageDepSubtree) {
    if (!is.null(package)) {
      if (length(package$depends) > 0) {
        # This package has dependencies; recursively process them.
        installList <- c(installList, makePkgInstallList(package$depends, 
                                                         installed))
      } 
      if (exists(package$name, envir = installed)) {
        # This package was already installed to fulfill another dependency; 
        # make sure that the version installed satisfies this version
        if (!identical(installed[[package$name]], package$version)) {
          stop(paste("Conflicting version requirements for ", package$name, 
                     ": ", package$version, ", ", installed[[package$name]]))
        }
      }
      else {
        # This package is not already installed; install it
        package$depends <- NULL
        installed[[package$name]] <- package$version
        installList[[length(installList) + 1]] <- package
      }
    }
  }
  return(installList)
}

# Given a package dependency tree, return a list of the packages to be installed
# to fulfill the tree, in the order of installation (i.e. each package's 
# dependencies must be installed before the package itself). 
makeInstallList <- function(packageDepTree) {
  installed <- new.env(hash = TRUE, parent = emptyenv())
  makePkgInstallList(packageDepTree, installed)
}


