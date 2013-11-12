---
layout: home
title: Limitations and caveats
---

Here are a couple of things to be aware of as you begin to use packrat.

### Package types

Packrat can only work with three types of packages: 

1. Packages installed from a CRAN repository (or a CRAN-like repository such as Bioconductor)
2. Packages installed by `devtools::install_github`, version 1.4 or later.
3. Local source packages (via the `sourcePackagePaths` parameter)

If you depend on a package that doesn't fall into any of these types, you'll need to coerce it into one of these types. The easiest way to do this is to put the package's source into a `.tar.gz` file, and use the `sourcePackagePaths` parameter. Note that if you use this mechanism, you will need to update the version of the package every time you make a change to the source code; Packrat relies on version numbers for local source packages to know when they've been updated.

### Building packages

Packrat prefers to install binary versions of packages from CRAN-like repositories when available. However, many packages don't have binaries, and even those that do now may not have them in the near future (CRAN does not archive binaries, only sources). 

Packrat stores the sources for each package locally, so that your project never depends on having platform binaries available from a mirror.

However, this means it is almost certain that you'll need to be able to build source packages locally in order to use Packrat. See [Package Development Prerequisites](http://www.rstudio.com/ide/docs/packages/prerequisites) to learn more about the tools to install for your operating system. It's recommended that you and your collaborators prepare your machines for package development before using Packrat.