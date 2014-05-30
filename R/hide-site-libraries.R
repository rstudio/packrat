## Remove the site-library libraries from unix-alikes
hideSiteLibraries <- function() {

  ## This is necessary for functionality in packrat, and we want to hide the NOTE
  do.call("unlockBinding", list(".Library.site", .BaseNamespaceEnv))
  assign(".Library.site", character(), envir = .BaseNamespaceEnv)
  lockBinding(".Library.site", .BaseNamespaceEnv)

  ## Reset the .libPaths()
  .libPaths(character())
}
