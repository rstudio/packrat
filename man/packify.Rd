\name{packify}
\alias{packify}
\title{Install packrat startup directives}
\usage{
  packify(dir = ".")
}
\arguments{
  \item{dir}{The directory in which to install .Rprofile
  and .Renviron files.}
}
\description{
  Install .Rprofile and .Renviron files in the given
  directory to make it use a private package library.
}
\details{
  Packrat uses entries in the \code{.Rprofile} and
  \code{.Renviron} files to keep package library operations
  (such as \code{\link{install.packages}}) local to the
  project's local library.

  It is not normally necessary to call \code{packify}
  directly; these files are normally installed by
  \code{\link{bootstrap}}. \code{packify} can be used to
  restore the files if they are missing (for instance, if
  they were not added to source control, or were
  accidentally removed).

  You'll need to restart R in the specified directory after
  running \code{packify} in order to start using the
  private package library.
}

