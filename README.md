[![Build Status](https://travis-ci.org/rstudio/packrat.png)](https://travis-ci.org/rstudio/packrat)
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

See the [project page](http://rstudio.github.io/packrat/) for more information,
or join the discussion at
[packrat-discuss](https://groups.google.com/forum/#!forum/packrat-discuss).

# Quick-start Guide

Start by installing Packrat:

    if (!require("devtools")) install.packages("devtools")
    devtools::install_github("rstudio/packrat")

Then, start a new R session at the base directory of your project and type:

    packrat::bootstrap()

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
  directory with libraries restored.

Navigate projects and set/get options with:
- `packrat::packrat_mode()`: Toggle packrat mode on and off, for navigating
  between projects within a single R session.
- `packrat::get_opts`, `packrat::set_opts`: Get/set project-specific settings.

There are also utility functions for using and managing packages in the
external / user library, and can be useful for leveraging packages in the user
library that you might not want as project-specific dependencies, e.g.
`devtools`, `knitr`, `roxygen2`:

- `packrat::extlib()`: Load an external package.
- `packrat::with_extlib()`: With an external package, evaluate an expression. The
  external package is loaded only for the duration of the evaluated
  expression.

