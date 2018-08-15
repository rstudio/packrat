We're very excited to announce Packrat 0.3.0! A number of bug and API
refinements have made their way into this release. Some noteworthy additions
in this release:

1. Automatic snapshots that automatically update your lockfile when
   non-destructive changes occur to your library (e.g. after an
   `install.packages` call, we can automatically update the lockfile to record
   any new packages),
2. Packrat integration with the latest release of
   [RStudio](https://www.rstudio.com/products/rstudio/download/) will make
   using Packrat with your projects easier than ever,
3. Packrat projects now gain project-specific options, allowing you to control
   a number of features -- please see `?"packrat-options"` for more details;
   with current new options:
   - auto.snapshot: Control whether automatic, asynchronous snapshots are
     performed,
   - vcs.ignore.lib: Ignore the `packrat/lib/` folder with version control
     systems?
   - vcs.ignore.src: Ignore the `packrat/src/` folder with version control
     systems?
   - print.banner.on.startup: Print the packrat banner on startup?
   - external.packages: Load packages from the user library on initialization?
     Useful for e.g. large Bioconductor annotation packages which you may not
     want to duplicate across projects.
4. Packrat does its best to ensure explicit R session restarts commissioned by
   the user are not required, and leverages RStudio to handle restarts when
   necessary,
5. Packrat has gained (provisional) support for R package development,
   although work here is still ongoing.

With the added RStudio integration, keeping your projects isolated and
reproducible should be easier than ever.

A full set of NEWS items follows:

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
  [`rmarkdown`](https://rmarkdown.rstudio.com/) dependencies).

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

