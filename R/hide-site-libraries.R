replaceLibrary <- function(lib, value) {

  ## Need to clobber in package:base, namespace:base
  envs <- c(
    as.environment("package:base"),
    .BaseNamespaceEnv
  )

  for (env in envs) {
    do.call("unlockBinding", list(lib, env))
    assign(lib, value, envir = env)
    lockBinding(lib, env)
  }

}


hideLibrary <- function(lib) {
  replaceLibrary(lib, character())
}

restoreLibrary <- function(lib) {

  cachedLib <- if (lib == ".Library")
    getenv(.packrat.env$R_PACKRAT_SYSTEM_LIBRARY)
  else if (lib == ".Library.site")
    getenv(.packrat.env$R_PACKRAT_SITE_LIBRARY)

  if (is.null(cachedLib)) {
    warning("packrat did not properly save the library state; cannot restore")
    return(invisible(NULL))
  }

  replaceLibrary(lib, cachedLib)

}

## Remove the site-library libraries from unix-alikes
hideSiteLibraries <- function() {
  hideLibrary(".Library.site")
}

## Restore the site-library libraries
restoreSiteLibraries <- function() {
  restoreLibrary(".Library.site")
}
