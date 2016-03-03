.onLoad <- function(libname, pkgname) {
  setenv(.packrat.env$R_PACKRAT_DEFAULT_LIBPATHS, .libPaths(), FALSE)
  setenv(.packrat.env$R_PACKRAT_LIBRARY, .Library, FALSE)
  setenv(.packrat.env$R_PACKRAT_LIBRARY_SITE, .Library.site, FALSE)
}

.onUnload <- function(libname, pkgname) {
  unsetenv(.packrat.env$R_PACKRAT_DEFAULT_LIBPATHS)
  unsetenv(.packrat.env$R_PACKRAT_LIBRARY)
  unsetenv(.packrat.env$R_PACKRAT_LIBRARY_SITE)
}
