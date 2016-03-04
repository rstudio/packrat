.onLoad <- function(libname, pkgname) {
  setenv(.packrat.env$R_PACKRAT_DEFAULT_LIBPATHS, .libPaths())
}
