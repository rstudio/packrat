\name{bootstrap}
\alias{bootstrap}
\title{Initialize Packrat on a new or existing R project}
\usage{
  bootstrap(appDir = ".", sourcePackagePaths = character())
}
\arguments{
  \item{appDir}{The directory that contains the R project.}

  \item{sourcePackagePaths}{List of paths to unpacked R package source directories.
  Use this argument only if your project depends on packages that are 
  not available on CRAN or Github.}
}
\description{
  Given an application directory, makes a new packrat project in the directory.
}
\details{
  \code{bootstrap} works as follows:
  \enumerate{
  \item Application dependencies are computed by examining the R code as described in \code{\link{appDependencies}}.
  \item A snapshot is taken of the version of each package currently used by the application as described in \code{\link{snapshot}}, and each package's sources 
are downloaded.
  \item A private library is created in the directory. 
  \item The snapshot is applied to the directory as described in \code{\link{restore}}. 
  }
  When \code{bootstrap} is finished, all the packages on which the project 
  depends are installed in a new, private library located inside the project
  directory.
  
  You must restart your R session in the given application directory after 
  running \code{bootstrap} in order for the changes to take effect. 
  
  When R is started in the directory, it will use the new, private library
  by default whenever you make library changes (using 
  \code{\link{install.packages}}, etc.). You can sync this private library
  with packrat using \code{\link{snapshot}} and \code{\link{restore}}.
}

