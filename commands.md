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

    packrat::init()

Initializes the current working directory as a Packrat project. This creates
the supporting files and directories listed below in "Anatomy of a Packrat
project", including a private library and snapshot.

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

- `packrat::on()`, `packrat::off()`: Toggle packrat mode on and off, for navigating
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

