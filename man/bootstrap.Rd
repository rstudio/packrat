\name{bootstrap}
\alias{bootstrap}
\title{Initialize Packrat on a new or existing R project}
\usage{
  bootstrap(appDir = ".", sourcePackagePaths = character())
}
\arguments{
  \item{appDir}{The directory that contains the R project.}

  \item{sourcePackagePaths}{Optional list of paths to
  unpacked R package source directories.}
}
\description{
  Given an application directory, computes the
  application's dependencies, and places the application's
  dependencies under packrat control.
}
\details{
  You must restart your R session after running bootstrap
  in order for the changes to take effect.
}

