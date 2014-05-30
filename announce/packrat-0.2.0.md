# Packrat 0.2.0

We're excited to announce version 0.2.0 of packrat! We've introduced a number
of exciting features in this release:

- **`packrat_mode`** allows you to seamlessly step in and out of packrat mode,
  for when you would would like to manage or use external projects while
  working with a packrat project. In addition, once you have entered packrat
  mode, the project directory is remembered for all later calls to packrat
  functions, e.g. `snapshot`, `clean`, `restore`, and so on -- so calling these
  functions without arguments will use the project directory, even if you have
  navigated outside of the project or to a project sub-directory.

- The packrat project files have been migrated into a single `packrat/` folder
  -- this keeps pollution in the base directory of your project down. The
  remappings are:
    - `packrat.sources` -> `packrat/src`,
    - `library` -> `packrat/lib`,
    - `packrat.lock` -> `packrat/packrat.lock`, and
    - the `.Renviron` has been removed.

- **`bundle`** and **`unbundle`** allow you to zip up your project as a tarball
  for easy sharing,

- **`with_extlib`** and **`extlib`** allow you to (temporarily) load and use
  packages within the user library; this can be useful if you want to leverage
  another package (for example, `devtools::install_github`) while avoiding
  dependencies on `devtools` itself,

- Packrat can now handle source package tarballs, rather than just source
  package folders,

- A number of bugs relating to `status()` output have been fixed and tweaked,
  to give better information to the user.

- Projects in packrat mode will have automatic, asynchronous snapshots for safe
  actions: with automatic snapshots on, packrat will automatically upgrade
  packages that are out of date, or add packages that are new to the lock file
  (e.g. when seen by an `install.packages()` call). Downgrades, removals and
  'crossgrades' will be ignored and will require you to take an appropriate
  action (guided by the information provided by `status()`).

- Packrat will warn you if you have user libraries in the system library path.
  In addition, packrat supplies a script for OS X users, to assist in migrating
  user packages from the system library to a separate user library. (By
  default, R versions compiled for Mac OS X install all packages into the
  system library; for packrat to function correctly we require that user and
  system libraries be separate -- this script should help facilitate the
  process. If you have any problems with migration, please let us know!)

We will also be working on integration with the RStudio IDE in the coming
weeks: here, we can provide a user-friendly interface for many of the packrat
actions, and also leverage many tools within the IDE for better and smarter
automatic updating of your packrat projects. Stay tuned!

If you'd like to migrate a 'packrat 0.1.0' project to the new format, please do
the following:

1. Navigate to your packrat project directory,
2. Remove the project `.Rprofile` file,
3. Start an R session in this folder,
4. Run the following script:

    if (!require("devtools")) install.packages("devtools")
    devtools::install_github("rstudio/packrat") packrat::migrate()

After this, you can restart your R session, and you should be good to go!

Thanks, Kevin

