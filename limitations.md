---
layout: home
title: Limitations and caveats
---

Here are a couple of things to be aware of as you begin to use packrat.

### Package types

Packrat currently only works with three types of packages: 

1. Packages installed from a CRAN repository (or a CRAN-like repository such
   as BioConductor)
2. Packages installed by `devtools::install_github`, version 1.4 or later.
3. Local source packages (as discovered within a 'local repository'; see the
   `local.repos` option within `?"packrat-options"`)

If you depend on a package that doesn't fall into any of these types, you'll
need to coerce it into one of these types. We recommend building a local
repository, acting as a place where source R packages will live (and can be
used) by your packrat projects. For example, you might keep all of your R
source packages in a directory called:

    ~/R/packages

and then, to use this directory as a local repository in your packrat
projects, you would use:

    ## Specify a local repository on startup packrat::init(options =
    list(local.repos = "~/R/packages"))

or

    ## Set the local repositories for an existing packrat project
    packrat::set_opts(local.repos = "~/R/packages")

After doing this, packages from these repositories can be installed with
`packrat::install_local()`. For example:

    ## install a package `lassoTools` that lives within a local repository
    packrat::install_local("lassoTools")

and Packrat will automatically discover the package `lassoTools` within the
local repository.

### Building packages

Packrat prefers to install binary versions of packages from CRAN-like
repositories when available. However, many packages don't have binaries, and
even those that do now may not have them in the near future (CRAN does not
archive binaries, only sources). 

Packrat stores the sources for each package locally, so that your project
never depends on having platform binaries available from a mirror.

However, this means it is almost certain that you'll need to be able to build
source packages locally in order to use Packrat. See [Package Development
Prerequisites](http://www.rstudio.com/ide/docs/packages/prerequisites) to
learn more about the tools to install for your operating system. It's
recommended that you and your collaborators prepare your machines for package
development before using Packrat.


### Initialization

To enable Packrat, a small piece of code is added to the `.Rprofile` file in the
project's directory.  (If the file doesn't exist yet, it is created.)  This has
the following consequences:

- Any local or site-wide initialization file is overridden.  If you want to execute those
  files in addition, source them from your project's `.Rprofile`, e.g.:

      source("~/.Rprofile")

- To enable Packrat when starting R from a subdirectory in your project
  (e.g., when knitting a report that lives in a subdirectory),
  create an `.Rprofile` with the following contents in the subdirectory
  (and in each intermediate subdirectory along the path to that file):

      source("../.Rprofile", chdir = TRUE)
