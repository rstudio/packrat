# This package contains methods invoked by the RStudio IDE. If a breaking change
# is made to the signature or behavior of these methods, bump this version to
# prevent older versions of RStudio from attempting integration.
#
# Note that:
# - RStudio uses the version number of the package to determine if the package
#   meets the minimum version requirement for integration (is the package too
#   old?), and this protocol number to determine the maximum version requirement
#   (is it too new?).
# - Backwards compatibility is presumed: that is, if RStudio has protocol 3 and
#   the package has protocol 2, it is accepted. The package version number, not
#   the protocol version number, is used to determine whether the package is too
#   old to be compatible.

.RStudio_protocol_version = 1
