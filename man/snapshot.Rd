\name{snapshot}
\alias{snapshot}
\title{
Capture and store the packages and versions in use. 
}
\description{
Finds the packages in use by the R code in the application, and stores a list of
those packages, their sources, and their current versions in packrat. 
}
\usage{
snapshot(appDir = ".", available = NULL, lib.loc = libdir(appDir),
sourcePackagePaths = NULL, orphan.check = TRUE, dry.run = FALSE)
}
\arguments{
  \item{appDir}{Directory containing application. Defaults
  to current working directory.
}
  \item{available}{A database of available packages, as returned by
\code{\link{available.packages}}. It is only necessary to supply this parameter
if the state being snapshotted includes packages not installed locally, which
is rare.  
}
  \item{lib.loc}{The library to snapshot. Defaults to the private library
associated with the given directory.
}
  \item{sourcePackagePaths}{A character vector of directories containing R
package sources. It is only necessary to supply this parameter when using a
package for which sources exist on neither CRAN or Github. 
}
  \item{orphan.check}{\code{TRUE} to check for orphaned packages; \code{FALSE}
to skip the check. Packrat only considers packages used by your code and
packages which are dependencies of packages used by your code. Any other
package in the private library is considered an orphan.  If the packages are
truly orphans, they can be removed with \code{\link{remove.packages}}; if they
are not, you can make packrat aware that your project needs them by adding a
\code{require} statement to any R file (see \code{\link{appDependencies}}).
}
  \item{dry.run}{Computes the changes to your packrat state that would be made
if a snapshot were performed, and prints them to the console.}
}
\note{
\code{snapshot} modifies the project's \code{packrat.lock} file, and the
sources stored in the project's \code{packrat.sources} directory. If you are
working with a version control system, your collaborators can sync the changes
to these files and then use \code{\link{restore}} to apply your snapshot.
}
\examples{
# Take a snapshot of the current project
snapshot()

# See what changes would be included in a snapshot
snapshot(dry.run = TRUE)

# Take a snapshot of a project that includes a custom package
snapshot(sourcePackagePaths = "~/R/MyCustomPackage")
}
