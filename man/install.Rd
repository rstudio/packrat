% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/install.R
\name{install}
\alias{install}
\title{Install a local development package.}
\usage{
install(
  pkg = ".",
  reload = TRUE,
  quick = FALSE,
  local = TRUE,
  args = getOption("devtools.install.args"),
  quiet = FALSE,
  dependencies = NA,
  build_vignettes = !quick,
  keep_source = getOption("keep.source.pkgs")
)
}
\arguments{
\item{pkg}{package description, can be path or package name.}

\item{reload}{if \code{TRUE} (the default), will automatically reload the
package after installing.}

\item{quick}{if \code{TRUE} skips docs, multiple-architectures,
demos, and vignettes, to make installation as fast as possible.}

\item{local}{if \code{FALSE} \code{\link{build}}s the package first:
this ensures that the installation is completely clean, and prevents any
binary artefacts (like \file{.o}, \code{.so}) from appearing in your local
package directory, but is considerably slower, because every compile has
to start from scratch.}

\item{args}{An optional character vector of additional command line
arguments to be passed to \code{R CMD install}. This defaults to the
value of the option \code{"devtools.install.args"}.}

\item{quiet}{if \code{TRUE} suppresses output from this function.}

\item{dependencies}{\code{logical} indicating to also install uninstalled
packages which this \code{pkg} depends on/links to/suggests. See
argument \code{dependencies} of \code{\link{install.packages}}.}

\item{build_vignettes}{if \code{TRUE}, will build vignettes. Normally it is
\code{build} that's responsible for creating vignettes; this argument makes
sure vignettes are built even if a build never happens (i.e. because
\code{local = TRUE}.}

\item{keep_source}{If \code{TRUE} will keep the srcrefs from an installed
package. This is useful for debugging (especially inside of RStudio).
It defaults to the option \code{"keep.source.pkgs"}.}
}
\description{
Uses \code{R CMD INSTALL} to install the package. Will also try to install
dependencies of the package from CRAN, if they're not already installed.
}
\details{
By default, installation takes place using the current package directory.
If you have compiled code, this means that artefacts of compilation will be
created in the \code{src/} directory. If you want to avoid this, you can
use \code{local = FALSE} to first build a package bundle and then install
it from a temporary directory. This is slower, but keeps the source
directory pristine.

If the package is loaded, it will be reloaded after installation.
}
