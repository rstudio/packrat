# Packrat 0.3.0.99 (Unreleased)

- The `source.packages` argument for `init` and `snapshot` has now been
  removed. In lieu, projects can now specify local repositories -- packages
  will be looked up in these directories when attempting to use a source
  package.

- Added `install_local` for installing packages from local repositories. For
  example, `install_local("abc")` will attempt to find and install a package
  named `abc` from the local repositories as specified within `packrat.opts`.

- Added option `use.cache`, for creating a persistent global cache of
  installed packages which can be easily symlinked and reused across multiple
  projects. When active, this will force installation of one version of a
  package only once -- thereafter, any projects requiring that package can
  symlink to the installed version in the cache.

- Use junction points on Windows to enable caching of packages.

- Added `opts` as an object for conveniently getting / setting specific
  options. For example, calling `opts$auto.snapshot()` returns the current
  value for the `auto.snapshot` option, while `opts$auto.snapshot(TRUE)` sets
  `auto.snapshot` on.

- Fixed spurious `download.file` warnings on Windows.

- Errors when installing packages on Windows are now captured and reported to
  the user.

# Packrat 0.3.0

- The `packrat/bootstrap.R` script has been renamed to `packrat/init.R`.

- Fixed a bug in `packrat::set_opts()` where `NULL` entries were not handled
  correctly.

- `packrat::user_lib()` and `packrat::packrat_lib()` can be used to query the
  locations for the user library and packrat library, respectively.

- `packrat::install_github()` works as a dependency-free
  `devtools::install_github()`, and ensures Packrat projects using GitHub
  packages do not have an implicit dependency on `devtools`.

- `packrat::disable()` can be used to disable Packrat on a project -- it
  removes the autoloader from the `.Rprofile` and turns off Packrat mode. (If
  you want to fully clean out the packrat library / sources, you will have to
  delete the `packrat/` subdirectory explicitly)

- `packrat::packrat_mode()` has been refactored into two separate functions:
  `packrat::on()` and `packrat::off()`.

- `.Rmd` files are now parsed for YAML dependencies (for
  [`rmarkdown`](http://rmarkdown.rstudio.com/) dependencies).

- Recommended packages (e.g. `lattice`) are now only taken as Packrat
  dependencies if explicitly installed by the user -- for example, if you want
  to include `lattice` (which is distributed with R, but updated on CRAN) in
  Packrat, you must explicitly call `install.packages("lattice")` to get that
  package into the private library.

- `packrat::snapshot()` now takes `available.packages()` as a default argument
  to `available`, assisting in dependency inference for packages inferred yet
  not installed.

- `packrat::snapshot()` now takes the state of the library, along with
  dependencies inferred in the code, as 'truth' -- any packages installed or
  inferred will now be entered into the lockfile on a `packrat::snapshot()`
  call (with caveats for 'recommended' packages, e.g. `lattice`)

- `packrat::snapshot()` no longer installs missing packages.

- Failed `packrat::init`s now clean up any files / directories generated.

- `packrat::bootstrap` has been renamed to `packrat::init` --
  `packrat::bootstrap` remains as a (deprecated) alias.

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
  `packrat::get_opts` and `packrat::set_opts`. Please see `?"packrat-options"`
  for more information.

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
  
- `packrat::on` now attempts to clean the search path when entering packrat mode.
  Any packages loaded from the user library will be unloaded before entering
  packrat mode.
  
- The `init.R` script has been updated to work better with `bundle` / `unbundle`:
  after `bundle`ing a packrat project, one should be able to initialize a new
  project using a combination of `unbundle` and `source('packrat/init.R')`.
  
- Migration scripts for Windows users have been added to packrat, to migrate
  user libraries away from the system library, to provide the library separation
  that Packrat requires.

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
