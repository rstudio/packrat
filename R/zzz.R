.onLoad <- function(libname, pkgname) {
  setenv(.packrat.env$R_PACKRAT_DEFAULT_LIBPATHS, .libPaths())
  setenv(.packrat.env$R_PACKRAT_SYSTEM_LIBRARY, .Library)
  setenv(.packrat.env$R_PACKRAT_SITE_LIBRARY, .Library.site)
}
