[![Build Status](https://travis-ci.org/rstudio/packrat.svg?branch=master)](https://travis-ci.org/rstudio/packrat)
[![Coverage Status](https://codecov.io/github/rstudio/packrat/coverage.svg?branch=master)](https://codecov.io/github/rstudio/packrat?branch=master)

# packrat

Packrat is a dependency management system for R.

Use packrat to make your R projects more:

* **Isolated:** Installing a new or updated package for one project won't break
  your other projects, and vice versa. That's because packrat gives each
  project its own private package library.
* **Portable:** Easily transport your projects from one computer to another,
  even across different platforms. Packrat makes it easy to install the
  packages your project depends on.
* **Reproducible:** Packrat records the exact package versions you depend on,
  and ensures those exact versions are the ones that get installed wherever you
  go.

See the [project page](https://rstudio.github.io/packrat/) for more information,
or join the discussion at
[packrat-discuss](https://groups.google.com/forum/#!forum/packrat-discuss).
Read the [release
notes](https://github.com/rstudio/packrat/blob/master/NEWS.md) to learn what's
new in Packrat.

# Quick-start Guide

Start by installing Packrat:

    install.packages("packrat")

Then, start a new R session at the base directory of your project and type:

    packrat::init()

This will install Packrat, set up a private library to be used for this
project, and then place you in `packrat mode`. While in packrat mode, calls to
functions like `install.packages` and `remove.packages` will modify the
private project library, rather than the user library.

When you want to manage the state of your private library, you can use the
Packrat functions:

- `packrat::snapshot()`: Save the current state of your library.
- `packrat::restore()`: Restore the library state saved in the most recent
  snapshot.
- `packrat::clean()`: Remove unused packages from your library.

Share a Packrat project with `bundle` and `unbundle`:
- `packrat::bundle()`: Bundle a packrat project, for easy sharing.
- `packrat::unbundle()`: Unbundle a packrat project, generating a project
  directory with libraries restored from the most recent snapshot.

Navigate projects and set/get options with:
- `packrat::on()`, `packrat::off()`: Toggle packrat mode on and off, for
  navigating between projects within a single R session.
- `packrat::get_opts`, `packrat::set_opts`: Get/set project-specific settings.

Manage ad-hoc local repositories (note that these are a separate entity from
CRAN-like repositories):
- `packrat::set_opts(local.repos = ...)` can be used to specify *local
  repositories*; that is, directories containing (unzipped) package sources.
- `packrat::install_local()` installs packages available in a local
  repository.

For example, suppose I have the (unzipped) package sources for
[`digest`](https://cran.r-project.org/package=digest) located
within the folder`~/git/R/digest/`. To install this package, you can use:

    packrat::set_opts(local.repos = "~/git/R")
    packrat::install_local("digest")

There are also utility functions for using and managing packages in the
external / user library, and can be useful for leveraging packages in the user
library that you might not want as project-specific dependencies, e.g.
`devtools`, `knitr`, `roxygen2`:

- `packrat::extlib()`: Load an external package.
- `packrat::with_extlib()`: With an external package, evaluate an expression.
  The external package is loaded only for the duration of the evaluated
  expression, but note that there may be other side effects associated with
  the package's `.onLoad`, `.onAttach` and `.onUnload` calls that we may not
  be able to fully control.

# Workflows

Packrat supports a set of common analytic workflows:

1. `As-you-go`: use `packrat::init()` to initialize packrat with your project,
   and use it to manage your project library while you develop your analysis.
   As you install and remove packages, you can use `packrat::snapshot()` and
   `packrat::restore()` to maintain the R packages in your project. For
   collaboration, you can either use your favourite version control system, or
   use `packrat::bundle()` to generate a bundled version of your project that
   collaborators can use with `packrat::unbundle()`.

2. `When-you're-done`: take an existing or complete analysis (preferably
   collected within one directory), and call `packrat::init()` to immediately
   obtain R package sources for all packages used in your project, and snapshot
   that state so it can hence be preserved across time.

# Setting up your own custom, CRAN-like repositories

Please view the [set-up
guide](https://rstudio.github.io/packrat/custom-repos.html) here for a simple
walkthrough in how you might set up your own, local, custom CRAN repository.
