.onLoad <- function(libname, pkgname) {

  mappings <- list(
    "R_PACKRAT_DEFAULT_LIBPATHS" = .libPaths(),
    "R_PACKRAT_SYSTEM_LIBRARY"   = .Library,
    "R_PACKRAT_SITE_LIBRARY"     = .Library.site
  )

  enumerate(mappings, function(key, val) {
    if (is.na(Sys.getenv(key, unset = NA)))
      setenv(key, val)
  })

}
