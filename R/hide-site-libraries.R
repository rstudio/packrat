hideLibrary <- function(lib) {
  do.call("unlockBinding", list(lib, .BaseNamespaceEnv))
  assign(lib, character(), envir = .BaseNamespaceEnv)
  lockBinding(lib, .BaseNamespaceEnv)
}

restoreLibrary <- function(lib) {
  oldLibs <- .packrat_mutables$get(lib)
  if (!is.null(oldLibs)) {
    do.call("unlockBinding", list(lib, .BaseNamespaceEnv))
    assign(lib, oldLibs, envir = .BaseNamespaceEnv)
    lockBinding(lib, .BaseNamespaceEnv)
  }
}

replaceLibrary <- function(lib, value) {
  do.call("unlockBinding", list(lib, .BaseNamespaceEnv))
  assign(lib, value, envir = .BaseNamespaceEnv)
  lockBinding(lib, .BaseNamespaceEnv)
}

## Remove the site-library libraries from unix-alikes
hideSiteLibraries <- function() {
  hideLibrary(".Library.site")
}

## Restore the site-library libraries
restoreSiteLibraries <- function() {
  restoreLibrary(".Library.site")
}
