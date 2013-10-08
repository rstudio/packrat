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
appDependencies <- function(appDir = getwd()) {
  dirDependencies(appDir)
}

# detect all package dependencies for a directory of files
dirDependencies <- function(dir) {
  
  # first get the packages referred to in source code
  pkgs <- character()
  sapply(list.files(dir, pattern=glob2rx("*.R"), 
                    ignore.case=TRUE, recursive=TRUE),
         function(file) {
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
  exprs <- parse(file, n = -1L) 
  for (i in seq_along(exprs))
    pkgs <- append(pkgs, expressionDependencies(exprs[[i]]))
  
  # return packages
  unique(pkgs)
}

# detect the pacakge dependencies of an expression (adapted from 
# tools:::.check_packages_used)
expressionDependencies <- function(e) {
  
  # build a list of packages to return
  pkgs <- character()
  
  # examine calls
  if (is.call(e) || is.expression(e)) {
    
    # extract call
    call <- deparse(e[[1L]])[1L]
    
    # check for library or require and extract package argument
    if ((call %in% c("library", "require")) && (length(e) >= 2L)) {
      keep <- sapply(e, function(x) deparse(x)[1L] != "...")
      mc <- match.call(get(call, baseenv()), e[keep])
      if (!is.null(pkg <- mc$package)) {
        # ensure that types are rational
        if (!identical(mc$character.only, TRUE) || 
              identical(class(pkg), "character")) {
          pkg <- sub("^\"(.*)\"$", "\\1", deparse(pkg))
          pkgs <- append(pkgs, pkg)
        }
      }
    }
    
    # check for :: or :::
    else if (call %in% c("::", ":::")) {
      pkg <- deparse(e[[2L]])
      pkgs <- append(pkgs, pkg)
    }
    
    # check for uses of methods
    else if (call %in% c("setClass", "setMethod")) {
      pkgs <- append(pkgs, "methods")
    }
    
    # process subexpressions
    for (i in seq_along(e)) 
      pkgs <- append(pkgs, Recall(e[[i]]))
  }
  
  # return packages
  unique(pkgs)
}

