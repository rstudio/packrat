## Remove the site-library libraries from unix-alikes
hideSiteLibraries <- function() {

  .packrat_mutables$set(.Library.site = .Library.site)

  ## This is necessary for functionality in packrat, and we want to hide the NOTE
  do.call("unlockBinding", list(".Library.site", .BaseNamespaceEnv))
  assign(".Library.site", character(), envir = .BaseNamespaceEnv)
  lockBinding(".Library.site", .BaseNamespaceEnv)

  ## Reset the getLibPaths()
  setLibPaths(character())
}

## Restore the site-library libraries
restoreSiteLibraries <- function() {

  oldSiteLibs <- .packrat_mutables$get(".Library.site")
  if (!is.null(oldSiteLibs)) {
    do.call("unlockBinding", list(".Library.site", .BaseNamespaceEnv))
    assign(".Library.site", oldSiteLibs, envir = .BaseNamespaceEnv)
    lockBinding(".Library.site", .BaseNamespaceEnv)
  }

}
