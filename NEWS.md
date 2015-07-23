# Packrat 0.4.4 (Unreleased)

- Packrat no longer creates empty ignore files (thanks, @aronatkins)

- Bioconductor repositories are now created internally by packrat. This should 
  resolve errors where Bioconductor repository URLs are generated when an
  incompatible version of `BiocInstaller` is installed.
  
- Packrat now understands the `pkgType = "both"` option and can properly
  restore projects when that option is set.
  
- The `ignored.packages` option has been added, allowing users to specify
  packages that should not be tracked by packrat. Such packaes will not
  enter the lockfile on `packrat::snapshot()` calls; nor will they be
  cleaned out on `packrat::restore()` calls.

- Simple functions for interacting with the set of available repositories
  have been added. See `?repository-management` for more details.
  
- Facilities for interacting with local CRAN-like repositories have been added.
  This feature will eventually supersede the functionality offered by packrat's
  'ad-hoc' local repositories. The functions `packrat::repos_create()` and
  `packrat::upload_package()` are the two main API functions currently exported
  for uploading (source) packages to a local CRAN-like repository. By using a
  local CRAN-like repository, pre-existing workflows using `library()` and
  `install.packages()` to install packages should 'just work', as long as the
  `repos` option is properly set.

- The cache directory layout has been modified to ensure help (`?`) calls
  succeed. This is a breaking change with older versions of Packrat, and so
  newer versions of Packrat will use a new cache folder. (#194)
  
- Packrat issues a warning on `packrat::init()` if it was unable to infer the
  source of a particular package on initialization and instead uses the latest
  CRAN version.

- Packrat now infers whether a project implicitly depends on Shiny.

- Packrat now properly infers whether a package is from a CRAN-like repository,
  by checking if the `source` field maps to one of the repository names in
  `getOption('repos')`. (#185)

# Packrat 0.4.3

- Packrat avoids superfluous calls to `available.packages()` when possible,
  to avoid unnecessary internet requests.

- A bug where `packrat::status()` could cause the `R` session to hang was fixed.
  (#179)

- Added the `R` option `packrat.default.project.options`, which are now respected
  on e.g. `packrat::init()`. You can set
  `options(packrat.default.project.options = list(...))` to set your own
  default project options to use across Packrat projects.

# Packrat 0.4.2

- Packrat properly infers whether a project is an R package. A project
  with a `DESCRIPTION` file that has no `Type:` field, or has the
  `Type: Package` field, will be considered as an `R` package.
  
- Custom library paths can be set through the `R_PACKRAT_LIB_DIR` environment
  variable, which can be useful when using Packrat for non-local dependency
  management or deployment.

- A bug in the propagation of BioC repositories was fixed.

- Symlinks to `R` packages are created and destroyed more conservatively; this
  should help prevent problems where multiple `R` processes are acting within
  a single Packrat project.
  
- The autoloader was not properly created in rare cases (thanks, @krlmlr!)

- `install_local()` now forces `lib` and `repos` to be passed as named arguments,
  to avoid insidious errors. (#162)

- Packrat no longer removes the lockfile on `disable`; rather, it simply removes
  the autoloader. (#161)

- Packrat projects can now be non-interactively bootstrapped using the command:
  `R --vanilla -f packrat/init.R --args --bootstrap-packrat`. (#158)

- `packrat::bundle()` gains an 'omit.cran.src' argument, for ignoring package
  sources that are retrievable from CRAN. (#156)

- Packrat now understands how to install packages from custom CRAN-like
  repositories. (#153)

- Packrat now infers itself to be a git-managed project if any of its parent
  directories contains a `.git` folder. This ensures packrat projects included
  as sub-directories of a git-managed folder are properly understood as git-managed
  projects.

- `packrat::bundle()` gains an 'include.vcs.history' argument, for
  specifying whether VCS history folders (e.g. `.git/`, `.svn/`) should
  be packaged as part of the bundle. (#159)

- `packrat::bundle()` gains an 'include.bundles' argument, for specifying
  whether previously generated bundles are included as part of new bundles.

- Packrat now properly bundles projects on Windows (files in the packrat
  folder were not properly bundled when using the internal version of R's tar)
  (#152)

- Packrat gains the 'load.external.packages.on.startup' option, to control
  whether external packages are loaded on startup.

- Allow users to specify packrat itself as an external package (experimental, #147)

- The automatic snapshot mechanism now re-uses the `available.packages()`
  cache, to avoid unnecessary internet requests to CRAN.

- `packrat::status()` no longer asks you to uninstall packages that are
  found within the `Suggests:` field of the project's R package dependency
  chain, but does not require you to track these packages in the lockfile
  either. (#109)

- Downloads are now retried (up to a default of 5 times) on failure -- this
  should help with intermittent timeouts when e.g. downloading archives from
  GitHub. (#134)

- Packrat now adds `.Rprofile` to the `.Rbuildignore` file used for package
  development. (#138)

# Packrat 0.4.1

- Coincides with first CRAN release of packrat.

- Disabled migration warnings on Windows (as we now use junction points if
  possible to facilitate isolation)

- Fix bug with `tar` and projects containing a large number of files (#129)

- External packages (alongside their dependencies) are now symlinked to the
  `packrat/lib-ext` folder -- this allows external packages to function better
  alongside regular packrat workflows.

- If package symlinking (junctioning) fails, we instead copy the package to
  the corresponding packrat `lib*` folder.

# Packrat 0.4.0

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
