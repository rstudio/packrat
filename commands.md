---
layout: home
title: Packrat commands
---
<style type="text/css">
pre {
  font-weight: bold;
  margin-top: 42px !important;
}
</style>

The following is a list of the R functions in the packrat package that you'll
use most often. You can find more detailed documentation by typing
`help(package="packrat")` at the R console.

For more background on how these commands fit into the packrat workflow, see
[the walkthrough](walkthrough.html).

### Main Functionality

- `packrat::init()`: Initializes the current working directory as a Packrat
  project.

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

Navigate projects with:

- `packrat::on()`, `packrat::off()`: Toggle packrat mode on and off, for navigating
  between projects within a single R session.

### Options

Packrat project-specific options can be accessed and set with:

- `packrat::get_opts()`: Get project options for the current packrat project.
- `packrat::set_opts()`: Set project options for the current packrat project.
- `packrat::opts$<option>()`: Get / set a project option `<option>`.

The current set of available options is:

#### auto.snapshot

  Perform automatic, asynchronous snapshots when running interactively?
  (`TRUE` / `FALSE`; defaults to `TRUE`)

#### use.cache

  Install packages into a global cache, which is then shared across projects?
  By default, an OS-specific application data directory is used, but this can
  be overridden with the `R_PACKRAT_CACHE_DIR` environment variable.

#### print.banner.on.startup

  Print the banner on startup? Can be one of `TRUE` (always print),
  `FALSE` (never print), and 'auto` (do the right thing).

#### vcs.ignore.lib

  Add the packrat private library to your version control system ignore?
  (`TRUE` / `FALSE`; defaults to `TRUE`)

#### vcs.ignore.src

  Add the packrat private sources to your version control system ignore?
  (`TRUE` / `FALSE`; defaults to `FALSE`)

#### external.packages

  Packages which should be loaded from the user library upon entering packrat mode.
  This can be useful for very large packages which you don't want duplicated across
  multiple projects, e.g. Bioconductor annotation packages.

#### local.repos

  Local 'repositories'; i.e., directories containing package sources either as
  folders or as package tarballs. (Character vector; empty by default)

### Utilities

There are also utility functions for using and managing packages in the
external / user library, and can be useful for leveraging packages in the user
library that you might not want as project-specific dependencies, e.g.
`devtools`, `knitr`, `roxygen2`:

- `packrat::extlib()`: Load an external package.
- `packrat::with_extlib()`: With an external package, evaluate an expression. The
  external package is loaded only for the duration of the evaluated
  expression.

