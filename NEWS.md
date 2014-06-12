# Packrat 0.2.0.99 (Unreleased)

- Repositories are now stored in the lockfile as key-value pairs, and these
  repositories are used on startup. This mechanism will allow for more
  granular control over which repositories are valid, as well as for custom
  (e.g. non-CRAN) repositories.

- Isolation is now achieved using a different mechanism on OS X. Because user
  libraries can be installed into the 'system' library on OS X, we work around
  this by symlinking all 'base' and 'recommended' libraries into a private
  packrat 'system' library, and using that instead.

- Packrat gains project-specific options. `packrat/packrat.opts` is a DCF file
  of project-specific settings that can be queried and set through
  `packrat::get_opts` and `packrat::set_opts`. The current valid options are:
  - `auto.snapshot`: perform automatic, asynchronous, safe snapshots? This
    will automatically update the lock file when a new package is installed,
    for example.
  - `vcs.ignore.lib`, `vcs.ignore.src`: Ignore the `packrat/[lib/src]`
    directories in your version control system? Currently, only `git` and
    `svn` are supported.

- Packrat can now handle source package tarballs, in addition to source folders.

- API functions have been appropriately `snake_case`d and function arguments
  have been `dot.cased`. We apologize for any broken workflows here; but we
  imagine that most of use of packrat is done through calls to exported
  functions with no arguments passed, so this shouldn't disrupt most workflows.
  
- Packrat has introduced support for R packages -- you should now be able to
  develop R packages using packrat to manage your dependencies. Work here is
  on-going and feedback is appreciated.
  
- `search_path()` allows you to discover which packages are currently
  `attach`ed, and from which library each package has been loaded from.
  
- `status()` now (invisibly) returns a `data.frame` outlining the current
  status of your project, in addition to printing information to the console.
  
- `packrat_mode` gains an initial `on` argument, analogous to `devtools::dev_mode`.
  See `?packrat_mode` for more details.
  
- `packrat_mode` now attempts to clean the search path when entering packrat mode.
  Any packages loaded from the user library will be unloaded before entering
  packrat mode.
  
- The `bootstrap.R` script has been updated to work better with `bundle` / `unbundle`:
  after `bundle`ing a packrat project, one should be able to initialize a new
  project using a combination of `unbundle` and `source('packrat/bootstrap.R')`.
  
- Migration scripts for Mac users have been added to packrat, to migrate
  user libraries away from the system library, to provide the library separation
  that packrat requires.

# Packrat 0.2.0

There has been a change in the directory structure for packrat project files.
If you'd like to migrate a 'packrat 0.1.0' project to the new format, please do
the following:

1. Navigate to your packrat project directory,
2. Remove the project `.Rprofile` file,
3. Start an R session in this folder,
4. Run the following R code:

    if (!require("devtools")) install.packages("devtools")
    devtools::install_github("rstudio/packrat") packrat::migrate()

After this, you can restart your R session, and you should be good to go!

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
