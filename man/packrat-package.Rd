\name{packrat-package}
\alias{packrat-package}
\alias{packrat}
\docType{package}
\title{
Packrat
}
\description{
Packrat is a tool for managing the R packages your project depends on in 
an isolated, portable, and reproducible way.
}
\details{
Use packrat to make your R projects more:

\itemize{
\item \strong{Isolated}: Installing a new or updated package for one project won't break your other projects, and vice versa. That's because packrat gives each project its own private package library.
\item \strong{Portable}: Easily transport your projects from one computer to another, even across different platforms. Packrat makes it easy to install the packages your project depends on.
\item \strong{Reproducible}: Packrat records the exact package versions you depend on, and ensures those exact versions are the ones that get installed wherever you go.}

Use \code{bootstrap} to create a new packrat project, \code{snapshot} to record
changes to your project's library, and \code{restore} to recreate your library
the way it was the last time you took a snapshot. 

Using these simple functions and sharing packrat's files lets you collaborate in a shared, consistent environment with others as your project grows and changes, and provides an easy way to share your results when you're done. 
}
\author{
RStudio, Inc.
}
\keyword{ package }
\seealso{
}
\examples{
# Create a new packrat project from an existing directory of R code
bootstrap()

# Install a package and take a snapshot of the new state of the library
install.packages("TTR")
snapshot()

# Accidentally remove a package and restore to add it back
remove.packages("TTR")
restore()
}
