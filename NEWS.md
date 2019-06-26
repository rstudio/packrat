# Packrat 0.6.0 (UNRELEASED)

- BREAKING CHANGE: The default Packrat cache directory has changed, and now
  includes an R version suffix. This helps avoid populating a single cache with
  versions of packages compiled for different versions of R (as there is no
  guarantee that packages compiled for e.g. R 3.4.x will work with R 3.5.x). If
  you'd like to migrate your old Packrat cache directory, you can move the
  directory at `dirname(packrat:::appDataDir())` to `packrat:::appDataDir()`.

- Packrat no longer attempts to unload packages loaded from the user library
  after calling `packrat::init()`, as this could fail in a myriad of cases.
  (To ensure proper isolation of the session, you should still restart R after
  invoking `packrat::init()`).

- Packrat now properly annotates the RemoteType field when downloading packages
  from GitLab and Bitbucket. (#564)

- Packrat no longer removes old source packages during a restore. (#560)

- Added support for GitLab: packages downloaded from GitLab can now be
  restored by Packrat. (#562, @akgold)

- Fixed an issue where tangled R code chunks containing invalid R code prevented 
  Packrat from finding any dependencies.  Packrat will now look for package
  dependencies within each code chunk independently. (#551)

- Packrat no longer sets `LC_ALL=C` when building source packages, as this can
  lead to errors when building packages containing non-ASCII text. (#545)

- Fixed an issue where ignored packages would still be queried by
  `packrat::unused_packages()`, which affected other APIs like `packrat::clean()`.
  (#525)

- Fixed an issue where newly-added project options did not get their correct
  default value when no entry existed within the `packrat.opts` file. (#496)

# Packrat 0.5.0

- Packrat now supports both of BiocManager and BiocInstaller (as used for
  discovering the Bioconductor repositories active for the current project).
  BiocManager will be used for R >= 3.6.0; BiocInstaller will be used otherwise.

- The R option `packrat.dependency.discovery.disabled` can be set to TRUE to
  disable dependency discovery in projects. This can be useful if you find
  Packrat's dependency discovery is slow (as it can be in projects containing
  a large number of R Markdown files). (#513, @ras44)
  
- The scheme used for hashing packages that enter the Packrat cache has
  changed -- now, a defined ordering of fields is used when hashing a
  package's DESCRIPTION file. Note that this implies a package may need to be
  re-cached on restore, in the case that its hash has changed. This change
  should not affect any existing packages in the cache. (#505, @aronatkins)

- `packrat::with_extlib()` now works with no `packages` provided;
  both with and without this option, the new behavior is that `expr`
  is executed in an environment where the original (not packrat)
  library search path is in place.

- A project is now only considered 'packified' if it has both a Packrat
  lockfile as well as the associated autoloader in the project `.Rprofile`.

- Calling `packrat::init()` on a project that already contains a Packrat
  lockfile no longer attempts to re-snapshot and restore the project.

- Packrat now supports R packages available on BitBucket, courtesy of a PR from
  @mariamedp. (#481)

- Added the project option `symlink.system.packages`: users can now configure
  whether base R packages from the system library are symlinked into a private
  library `packrat/lib-R`. Disabling this can be useful if you intentionally
  want Packrat to use packages that have been installed into the system library.

- Fixed an issue where attempts to snapshot could fail when
  the R libraries live on a network drive.

# Packrat 0.4.9-3

- Adjusted unit tests to accommodate new CRAN package checks.

- Fixed an issue where `packrat::repos_upload()` would fail to re-compress
  uploaded tarballs. (#474)

# Packrat 0.4.9-2

- Fix a regression where attempts to download packages from GitHub when
  devtools is not available could fail. (#464)

# Packrat 0.4.9-1

- Fix test errors on CRAN.

# Packrat 0.4.9

- Packrat now understands how to install R packages from private GitHub
  repositories. (The `GITHUB_PAT` environment variable should be set with
  an access token that provides access to the associated repositories.)
  (#449, #448, @ras44)

- Packrat gained the `get_lockfile_metadata()` and `set_lockfile_metadata()`
  functions, for changing metadata associated with a particular Packrat project;
  e.g. the active R version, or the active set of repositories. (#429, @cderv)

- Packrat now ignores all source files within a `packrat/` directory when
  inferring dependencies, not just the `packrat/` directory discovered at
  the top level. (#385)

- Packrat no longer includes the `.Rhistory` file when creating bundles. (#401)

- Packrat now properly handles the plain 'http' protocol when using versions
  R >= 3.2. (We now set `options(download.file.method = "wininet")` rather than
  `options(download.file.method = "internal")` in such cases.)

- The `infer.dependencies` argument can now be used to switch off the scanning of
  code for dependencies when using `packrat::init()` and `packrat::snapshot()`.

- Packrat no longer fails to download the current version of a package if
  the binary and source branches of the active repositories are out of sync.

- Packrat now attempts to parse scripts using UTF-8 encoding in addition to the
  system encoding. This should primarily help users on Windows who (rightly)
  save their documents using UTF-8 encoding rather than the default system
  encoding.

- Packrat now screens out empty package names discovered during package
  dependency discovery. (#314)
  
- The Packrat global cache is now enabled on Windows. Junction points
  (rather than symbolic links) are used to populate entries in the
  private Packrat library.

- The 'lib-R' and 'lib-ext' directories now use architecture-specific
  sub-directories for their libraries. This should further help in cases
  where multiple versions of R are operating within a Packrat project
  at the same time.

- Packrat now better handles cases where non-symlink files find their
  way into the 'lib-R' and 'lib-ext' folders.

- Packrat now better handles packages that contain trailing
  newlines in their DESCRIPTION file -- it now avoids
  inserting intervening blank lines between records, which
  should resolve the 'Error: contains a blank line' error that
  can occur during package installation.

- Packrat now only checks for the 'BiocInstaller' package within the
  Packrat private library, when attempting to ascertain whether
  Bioconductor is used by a particular project.

- Packrat no longer automatically restores projects on startup when
  Packrat is not detected within the library directory.

- Packrat now more eagerly caches packages during `packrat::restore()` --
  packages will be immediately cached following successful installation,
  rather than at the end of the restore process. (#324)

- Packrat now better handles cases where multiple R processes attempt
  to write the same package to the cache at the same time. (#333)

- Packrat now properly checks whether a package exists in the cache
  before attempting to copy / symlink that package to the active library
  directory. (#335)

- Users can now control whether Packrat snapshots sources by setting the
  `R_PACKRAT_SNAPSHOT_SOURCES` environment variable to a 'truthy' value, or by
  setting the R option `packrat.snapshot.sources`.

- Packrat now provides APIs for accessing the active paths to resource
  directories, with:
  
  - `packrat::project_dir(project)`
  - `packrat::src_dir(project)`
  - `packrat::lib_dir(project)`
  - `packrat::bundles_dir(project)`
  
  See `?packrat-resources` for more details.

- Packrat better preserves the pre-existing contents of ignore files. (#332)

# Packrat 0.4.8

- Packrat better handles UTF-8 encoded R Markdown documents on Windows.
  (#329, @jmcphers)

- The source directory for a Packrat project can now be over-ridden either by
  setting the `R_PACKRAT_SRC_DIR` environment variable, or the 'packrat.src.dir'
  R option. Clever use of this should allow multiple projects to share a cache
  of downloaded package sources.

- The library directory for a Packrat project can now be over-ridden either by
  setting the `R_PACKRAT_LIB_DIR` environment variable, or the `packrat.lib.dir`
  R option. This can be used if you'd like the library directory for a project
  to be stored in an alternate location -- this can be useful for Packrat projects
  stored in e.g. Dropbox folders. A similar treatment is supplied for the Packrat
  bundles directory, with the `R_PACKRAT_BUNDLES_DIR` environment variable and
  the `packrat.bundles.dir` project option.

- Packrat now stores the active project as an environment variable,
  `R_PACKRAT_PROJECT_DIR`. This should help in situations where R
  sub-processes are launched while a packrat project is active, and those
  sub-processes need to access the active packrat library.

- Packrat now respects the 'repos' field used in the Packrat lockfile when
  restoring a project. (#316)

- Packrat better reports download failures, mentioning lack of internet
  connectivity as a potential culprit. (#306)

- Packrat no longer treats YAML parse failures (when attempting to scour R Markdown
  documents for R package dependencies) as fatal errors. (#312)

- Packrat now first attempts to move folders to the cache, and falls back to a
  directory copy when this fails. This should improve caching performance in
  the cases where the Packrat cache lies on the same volume as the active
  project.

- `packrat::set_opts()` gains an argument, 'persist', controlling whether newly
  set options should be persisted. This allows session-temporary project
  options to be set.

- Packrat now only updates ignore files (e.g., '.gitignore') when they have
  actually changed. (#303, @mdshw5)

- Packrat now passes '-s' when invoking 'curl', thereby ensuring quiet output
  when downloading fails. This should resolve issues where attempts to use
  packrat without internet connectivity caused spurious messages of the form
  'curl: (22) The requested URL returned error: 404 Not Found' to be printed.

- Packrat now passes '-g' when invoking 'curl', thereby disabling curl's
  globbing parser. This should help ensure that URLs containing e.g. '[' and
  ']' characters can be successfully downloaded as expected.

- Packrat now queries 'api.github.com', rather than 'www.github.com', when
  attempting to download package sources.

- Packrat now properly respects the 'packrat::opts$ignored.packages()' project
  option when restoring a project.

- Packrat handles lockfiles with no packages available. (#294)

- Fixed an issue where attempting to form junction points to separate
  drives on Windows could fail. (@raubreywhite, #288)

# Packrat 0.4.7

- Packrat now always uses R's internal `tar` method when bundling a
  project, for cross-platform consistency.

- The 'use.cache' option is disabled on Windows -- this may be revisited
  in the future once we have a reliable mechanism for detecting whether
  a particular directory is a reparse point without the use of compiled
  code within packrat.
  
- Packrat uses `devtools` + `httr` (when available) to download files and
  directories from GitHub URLs. This should enable users to allow packrat
  to access private GitHub repositories as long as the `GITHUB_PAT`
  environment variable is set to an appropriate private access token.
  See `?devtools::install_github` for more details on setting up a private
  access token.
  
- The `install_github()` shim from packrat has been removed -- please
  use `devtools::install_github()`, either by taking an explicit dependency
  on the `devtools` package, or by loading it from the user library with
  the packrat option `packrat::opts$external.packages("devtools")`.
  
- Packrat is now smarter when managing symlinks within the project library
  (when package caching is enabled). This should allow multiple R processes
  to use the same packrat project at the same time. (Previously, there was
  risk that one R session might clear / refresh symlinks while another process
  attempted to access them).
  
- Packrat no longer erroneously generates recursive symlinks (and attempts
  to clean up any recursive symlinks discovered as appropriate).
  
- Packrat now records the original library paths within its `.onLoad()`
  handler, and uses these library paths when attempting to load packages
  from the user library.
  
- Fixed a bug where recursive hashing of a package's LinkingTo dependencies
  could fail.

- Fixed a bug where `with_extlib()` could force a promise in the wrong
  environment.
  
# Packrat 0.4.6

- Packrat gains the option `snapshot.recommended.packages()`, to control
  whether Recommended packages detected in the system library should become
  part of the lockfile.

- Silence noisy 'FAILED' messages that occurred when querying a repository
  for the existence of a package (in its archives) failed.

- Fixed a regression where dependencies were not properly discovered when
  using (R <= 3.1).

- Fixed an issue where the 'quiet.package.installation()' option could cause
  a restore failure, if it was not already set for the active project.

# Packrat 0.4.5

- Fixed an issue where the autoloader header in a project's `.Rprofile`
  could become duplicated.

- Packrat now attempts to choose a secure download method when downloading
  files from `https` URLs, if a default download method is not already set.

- The code used for detecting package dependencies has been re-written and
  refactored, and should properly avoid detecting `x` as a dependency in e.g.
  `library(x, character.only = TRUE)`.

- `packrat::snapshot()` now updates the active repositories in the lock file,
  even if no packages have changed.

- Packrat no longer creates empty ignore files. (@aronatkins)

# Packrat 0.4.4

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
