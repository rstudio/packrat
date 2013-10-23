\name{status}
\alias{status}
\title{Show differences between the last snapshot and the library}
\usage{
  status(appDir = ".", lib.loc = libdir(appDir), quiet = FALSE)
}
\arguments{
  \item{appDir}{The directory that contains the R project.}

  \item{lib.loc}{The library to examine. Defaults to the private library
associated with the application directory.}

  \item{quiet}{\code{TRUE} to suppress output, \code{FALSE} (the default) to
show output}
}
\description{
Shows the differences between the project's packrat dependencies, its private
package library, and its R scripts.
}
\details{
These differences are created when you use the normal R package management
commands like \code{\link{install.packages}}, \code{\link{update.packages}},
and \code{\link{remove.packages}}. To bring these differences into packrat, you
can use \code{\link{snapshot}}.

Differences can also arise if one of your collaborators adds or removes
packages from the packrat dependencies. In this case, you simply need to tell
packrat to update your private package library using \code{\link{restore}}.
}

