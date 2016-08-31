.onLoad <- function(libname, pkgname) {
  if (!length(getenv(.packrat.env$R_PACKRAT_DEFAULT_LIBPATHS)))
    setenv(.packrat.env$R_PACKRAT_DEFAULT_LIBPATHS, .libPaths())

  if (!length(getenv(.packrat.env$R_PACKRAT_SYSTEM_LIBRARY)))
    setenv(.packrat.env$R_PACKRAT_SYSTEM_LIBRARY, .Library)

  if (!length(getenv(.packrat.env$R_PACKRAT_SITE_LIBRARY)))
    setenv(.packrat.env$R_PACKRAT_SITE_LIBRARY, .Library.site)
}
