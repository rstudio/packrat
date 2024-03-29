% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bundle.R
\name{bundle}
\alias{bundle}
\title{Bundle a Packrat Project}
\usage{
bundle(
  project = NULL,
  file = NULL,
  include.src = TRUE,
  include.lib = FALSE,
  include.bundles = TRUE,
  include.vcs.history = FALSE,
  overwrite = FALSE,
  omit.cran.src = FALSE,
  ...
)
}
\arguments{
\item{project}{The project directory. Defaults to the currently activate
project. By default, the current project active under \code{packratMode}
is checked.}

\item{file}{The path to write the bundle. By default, we write
the bundle to \code{packrat/bundles/<project>-<date>.tar.gz}, with
\code{<date>} as returned by \code{Sys.date()}.}

\item{include.src}{Include the packrat sources?}

\item{include.lib}{Include the packrat private library?}

\item{include.bundles}{Include other packrat bundle tarballs
(as in \code{packrat/bundles/})?}

\item{include.vcs.history}{Include version control history (ie, \code{.git/}
or \code{.svn/} folders)?}

\item{overwrite}{Boolean; overwrite the file at \code{file} if it already exists?}

\item{omit.cran.src}{Boolean; when \code{TRUE}, packages whose sources can
be retrieved from CRAN are excluded from the bundle.}

\item{...}{Optional arguments passed to \code{\link{tar}}.}
}
\value{
The path (invisibly) to the bundled project.
}
\description{
Bundle a packrat project, for easy sharing.
}
\details{
The project is bundled as a gzipped tarball (\code{.tar.gz}), which can
be unbundled either with \code{packrat::\link{unbundle}} (which
restores the project as well), \R's own \code{utils::\link{untar}}, or
through most system \code{tar} implementations.

The tar binary is selected using the same heuristic as \code{\link{restore}}.
}
