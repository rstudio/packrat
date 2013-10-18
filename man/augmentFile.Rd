\name{augmentFile}
\alias{augmentFile}
\title{Add the contents of srcFile into targetFile, with "magic" comments bracketing
the contents. Should be safe to run multiple times--each subsequent call will
replace the contents between magic comments.}
\usage{
  augmentFile(srcFile, targetFile, preferTop)
}
\description{
  Add the contents of srcFile into targetFile, with "magic"
  comments bracketing the contents. Should be safe to run
  multiple times--each subsequent call will replace the
  contents between magic comments.
}
\keyword{internal}

