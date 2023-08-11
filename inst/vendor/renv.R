#
# renv 1.0.1 [rstudio/renv#5dc2fc9]: A dependency management toolkit for R.
# Generated using `renv:::vendor()` at 2023-08-11 10:13:20.914354.
#

# aaa.R ----------------------------------------------------------------------


# global variables
the <- new.env(parent = emptyenv())

# detect if we're running within R CMD build
building <- function() {
  nzchar(Sys.getenv("R_CMD")) &&
    grepl("Rbuild", basename(dirname(getwd())), fixed = TRUE)
}


# abi.R ----------------------------------------------------------------------


renv_abi_check <- function(packages = NULL,
                           ...,
                           libpaths = NULL,
                           project  = NULL)
{
  if (renv_platform_windows()) {
    writef("- ABI conflict checks are not yet implemented on Windows.")
    return()
  }

  # disable via option if necessary
  enabled <- getOption("renv.abi.check", default = TRUE)
  if (identical(enabled, FALSE))
    return()

  # resolve arguments
  project <- renv_project_resolve(project)
  libpaths <- libpaths %||% renv_libpaths_all()

  # read installed packages
  packages <- packages %||% renv_abi_packages(project, libpaths)

  # analyze each package
  problems <- stack()
  map(packages, function(package) {
    tryCatch(
      renv_abi_check_impl(package, problems),
      error = warnify
    )
  })

  # report problmes
  data <- problems$data()
  if (empty(data)) {
    fmt <- "- No ABI conflicts were detected in the set of installed packages."
    writef(fmt)
    return(invisible(data))
  }

  # combine everything together
  tbl <- bind(data)

  # make reports for each different type
  reasons <- unique(tbl$reason)
  if ("Rcpp_precious_list" %in% reasons) {
    packages <- sort(unique(tbl$package[tbl$reason == "Rcpp_precious_list"]))
    caution_bullets(
      "The following packages were built against a newer version of Rcpp than is currently available:",
      packages,
      c(
        paste(
          "These packages depend on Rcpp (>= 1.0.7);",
          "however, Rcpp", renv_package_version("Rcpp"), "is currently installed."
        ),
        "Consider installing a new version of Rcpp with 'install.packages(\"Rcpp\")'."
      )
    )
  }

  invisible(tbl)

}

renv_abi_check_impl <- function(package, problems) {

  # find path to package
  pkgpath <- renv_package_find(package)

  # look for an associated shared object
  shlib <- renv_package_shlib(pkgpath)
  if (!file.exists(shlib))
    return()

  # read symbols from LinkingTo dependency packages
  pkgdesc <- renv_description_read(path = pkgpath)
  if (is.null(pkgdesc$LinkingTo))
    return()

  # read symbols from the library
  symbols <- renv_abi_symbols(shlib)

  # handle Rcpp
  linkdeps <- renv_description_parse_field(pkgdesc$LinkingTo)
  if ("Rcpp" %in% linkdeps$Package)
    renv_abi_check_impl_rcpp(package, symbols, problems)

  # TODO: other checks? more direct symbol checks for other packages?

}

renv_abi_check_impl_rcpp <- function(package, symbols, problems) {

  # read Rcpp symbols
  rcpplib <- renv_package_shlib("Rcpp")
  rcppsyms <- renv_abi_symbols(rcpplib)

  # perform checks for different versions of Rcpp
  renv_abi_check_impl_rcpp_preciouslist(package, symbols, rcppsyms, problems)

}

renv_abi_check_impl_rcpp_preciouslist <- function(package, symbols, rcppsyms, problems) {

  # check for dependency on Rcpp_precious APIs
  required <- grep("Rcpp_precious", symbols$symbol, value = TRUE)
  if (empty(required))
    return()

  # check for Rcpp_precious APIs being available
  available <- grep("Rcpp_precious", rcppsyms$symbol, value = TRUE)
  if (length(available))
    return()

  problem <- renv_abi_problem(
    package    = paste(package, renv_package_version(package)),
    dependency = paste("Rcpp", renv_package_version("Rcpp")),
    reason     = "Rcpp_precious_list"
  )

  problems$push(problem)

}

renv_abi_symbols <- function(path, args = NULL) {

  # invoke nm to read symbols
  output <- renv_system_exec(
    command = "nm",
    args    = c(args, renv_shell_path(path)),
    action  = "reading symbols"
  )

  # parse output
  parts <- strsplit(output, "\\s+")
  data <- .mapply(c, parts, NULL)
  names(data) <- c("offset", "type", "symbol")

  # join into data.frame
  as_data_frame(data)

}

renv_abi_problem <- function(package, dependency, reason) {

  list(
    package    = package,
    dependency = dependency,
    reason     = reason
  )

}

renv_abi_packages <- function(project, libpaths) {

  # create a lockfile
  lockfile <- snapshot(
    library  = libpaths,
    lockfile = NULL,
    type     = "all",
    project  = project
  )

  # return package names
  names(lockfile$Packages)

}


# abort.R --------------------------------------------------------------------


abort <- function(message, ..., body = NULL, class = NULL) {

  # create condition object
  cnd <- if (is.character(message)) {
    structure(class = c(class, "error", "condition"), list(
      message = paste(c(message, body), collapse = "\n"),
      meta = list(message = message, body = body),
      ...
    ))
  } else if (inherits(message, "condition")) {
    message
  } else {
    stop("internal error: abort called with unexpected message")
  }

  # if we were called with a custom condition object not having our meta,
  # just throw it as-is
  if (is.null(cnd$meta))
    stop(cnd)

  # signal the condition, giving calling handlers a chance to run first
  signalCondition(cnd)

  # if we got here, then there wasn't any tryCatch() handler on the stack.
  # handle printing of the error ourselves, and then stop with fallback.
  all <- c(
    cnd$meta$body, if (length(cnd$meta$body)) "",
    paste("Error:", paste(cnd$meta$message, collapse = "\n"))
  )

  # write error message to stderr, as errors might normally do
  writeLines(all, con = stderr())

  # create the fallback, but 'dodge' the existing error handlers
  fallback <- cnd
  fallback$message <- ""
  class(fallback) <- "condition"

  # disable error printing for the empty error
  renv_scope_options(show.error.messages = FALSE)

  # now throw the error
  stop(fallback)

}


# acls.R ---------------------------------------------------------------------


renv_acls_reset <- function(source, target = dirname(source)) {

  # only run on Linux for now
  if (!renv_platform_linux())
    return(FALSE)

  # skip if we don't have 'getfacl', 'setfacl'
  getfacl <- Sys.which("getfacl"); setfacl <- Sys.which("setfacl")
  if (!nzchar(getfacl) || !nzchar(setfacl))
    return(FALSE)

  # build command
  fmt <- "getfacl %s 2> /dev/null | setfacl -R --set-file=- %s 2> /dev/null"
  cmd <- sprintf(fmt, renv_shell_path(target), renv_shell_path(source))

  # execute it
  # TODO: Should we report errors? If so, how?
  catch(
    renv_system_exec(
      command = cmd,
      action = "resetting ACLs",
      quiet = TRUE
    )
  )

}


# actions.R ------------------------------------------------------------------


actions <- function(action = c("snapshot", "restore"),
                    ...,
                    project = NULL,
                    library = NULL,
                    lockfile = NULL,
                    type = settings$snapshot.type(project = project),
                    clean = FALSE)
{
  action   <- match.arg(action)
  project  <- renv_project_resolve(project)
  lockfile <- lockfile %||% renv_lockfile_path(project = project)

  renv_project_lock(project = project)

  switch(
    action,
    snapshot = renv_actions_snapshot(project, library, lockfile, type),
    restore  = renv_actions_restore(project, library, lockfile, clean)
  )
}

renv_actions_merge <- function(snap, lock, diff) {

  fields <- c("Package", "Version", "Source")
  defaults <- data.frame(
    "Package"          = character(),
    "Library Version"  = character(),
    "Library Source"   = character(),
    "Lockfile Version" = character(),
    "Lockfile Source"  = character(),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  lhs <- bapply(unname(renv_lockfile_records(snap)), `[`, fields)
  if (length(lhs))
    names(lhs) <- c("Package", paste("Library",  names(lhs)[-1L]))

  rhs <- bapply(unname(renv_lockfile_records(lock)), `[`, fields)
  if (length(rhs))
    names(rhs) <- c("Package", paste("Lockfile", names(rhs)[-1L]))

  merged <- if (length(lhs) && length(rhs))
    merge(lhs, rhs, by = "Package", all = TRUE)
  else if (length(lhs))
    lhs
  else if (length(rhs))
    rhs
  else
    defaults

  actions <- data.frame(Package = names(diff),
                        Action = as.character(diff),
                        check.names = FALSE,
                        stringsAsFactors = FALSE)

  all <- merge(merged, actions, by = "Package")

  missing <- setdiff(names(defaults), names(all))
  all[missing] <- NA_character_

  all

}

renv_actions_snapshot <- function(project, library, lockfile, type) {

  lock <- renv_lockfile_load(project = project)
  snap <- snapshot(project = project,
                   library = library,
                   lockfile = NULL,
                   type = type)

  diff <- renv_lockfile_diff_packages(lock, snap)
  renv_actions_merge(snap, lock, diff)

}

renv_actions_restore <- function(project, library, lockfile, clean) {

  # NOTE: we use a simple snapshot here as we just want to know the
  # difference in library state before and after applying the lockfile;
  # that is, we want to know what the library looks like without any
  # filtering of what records would be reported from the library
  lock <- renv_lockfile_load(project = project)
  snap <- snapshot(project = project,
                   library = library,
                   lockfile = NULL,
                   type = "all")

  diff <- renv_lockfile_diff_packages(snap, lock)
  actions <- renv_actions_merge(snap, lock, diff)
  renv_actions_restore_clean(actions, clean, project)

}

renv_actions_restore_clean <- function(actions, clean, project) {

  # if not cleaning, then we don't do any removals
  if (!clean) {
    filtered <- actions[actions$Action != "remove", ]
    return(filtered)
  }

  # otherwise, only process removals in the project library
  projlib <- renv_paths_library(project = project)
  locations <- renv_package_find(actions$Package)

  keep <- actions$Action != "remove" | dirname(locations) == projlib
  actions[keep, ]

}


# activate.R -----------------------------------------------------------------


#' Activate or deactivate a project
#'
#' @description
#' `activate()` enables renv for a project in both the current session and
#' in all future sessions. You should not generally need to call `activate()`
#' yourself as it's called automatically by [renv::init()], which is the best
#' way to start using renv in a new project.
#'
#' `activate()` first calls [renv::scaffold()] to set up the project
#' infrastructure. Most importantly, this creates a project library and adds a
#' an auto-loader to `.Rprofile` to ensure that the project library is
#' automatically used for all future instances of the project. It then restarts
#' the session to use that auto-loader.
#'
#' `deactivate()` removes the infrastructure added by `activate()`, and
#' restarts the session. By default it will remove the auto-loader from the
#' `.Rprofile`; use `clean = TRUE` to also delete the lockfile and the project
#' library.
#'
#' # Temporary deactivation
#'
#' If you need to temporarily disable autoload activation you can set
#' the `RENV_CONFIG_AUTOLOADER_ENABLED` envvar, e.g.
#' `Sys.setenv(RENV_CONFIG_AUTOLOADER_ENABLED = "false")`.
#'
#' @inherit renv-params
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # activate the current project
#' renv::activate()
#'
#' # activate a separate project
#' renv::activate("~/projects/analysis")
#'
#' # deactivate the currently-activated project
#' renv::deactivate()
#'
#' }
activate <- function(project = NULL, profile = NULL) {

  renv_consent_check()
  renv_scope_error_handler()

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  renv_profile_set(profile)

  renv_activate_impl(
    project = project,
    profile = profile,
    version = NULL
  )

  invisible(project)

}

renv_activate_impl <- function(project,
                               profile,
                               version = NULL,
                               load    = TRUE,
                               restart = TRUE)
{
  # prepare renv infrastructure
  renv_infrastructure_write(
    project = project,
    profile = profile,
    version = version
  )

  # ensure renv is imbued into the new library path if necessary
  if (!renv_tests_running())
    renv_imbue_self(project)

  # restart session if requested
  if (restart && !renv_tests_running())
    return(renv_restart_request(project, reason = "renv activated"))

  if (renv_rstudio_available())
    renv_rstudio_initialize(project)

  # try to load the project
  if (load) {
    setwd(project)
    load(project)
  }

  invisible(project)

}

renv_activate_version <- function(project) {

  # try to get version from activate.R
  methods <- list(
    renv_activate_version_lockfile,
    renv_activate_version_activate,
    renv_activate_version_metadata
  )

  for (method in methods) {
    version <- catch(method(project))
    if (is.character(version))
      return(version)
  }

  fmt <- "failed to determine renv version for project %s"
  stopf(fmt, renv_path_pretty(project))

}

renv_activate_version_activate <- function(project) {

  # get path to the activate script
  activate <- renv_paths_activate(project = project)
  if (!file.exists(activate))
    return(NULL)

  # check for version
  contents <- readLines(activate, warn = FALSE)
  line <- grep("version <-", contents, fixed = TRUE, value = TRUE)[[1L]]
  version <- parse(text = line)[[1L]][[3L]]

  # check for sha as well
  line <- grep("attr(version, \"sha\")", contents, fixed = TRUE, value = TRUE)
  if (length(line)) {
    sha <- parse(text = line)[[1L]][[3L]]
    attr(version, "sha") <- sha
  }

  version

}

renv_activate_version_lockfile <- function(project) {

  path <- renv_lockfile_path(project)
  if (!file.exists(path))
    return(NULL)

  # read the renv record
  lockfile <- renv_lockfile_read(path)
  records <- renv_lockfile_records(lockfile)
  renv_metadata_version_create(records[["renv"]])

}

renv_activate_version_metadata <- function(project) {
  the$metadata$version
}

renv_activate_prompt <- function(action, library, prompt, project) {

  # check whether we should ask user to activate
  ask <-
    config$activate.prompt() &&
    prompt &&
    interactive() &&
    is.null(library) &&
    !renv_project_loaded(project) &&
    !is_testing()

  # for snapshot, since users might want to snapshot their system library
  # in an renv-lite configuration, only prompt if it looks like they're
  # working within an renv project that hasn't been loaded
  if ("snapshot" %in% action) {
    libpath <- renv_paths_library(project = project)
    ask <- ask && file.exists(libpath)
  }

  if (!ask)
    return(FALSE)

  renv_activate_prompt_impl(action, project)


}

renv_activate_prompt_impl <- function(action, project = NULL) {
  title <- c(
    sprintf(
      "It looks like you've called renv::%s() in a project that hasn't been activated yet.",
      action
    ),
    "How would you like to proceed?"
  )
  choices <- c(
    activate = "Activate the project and use the project library.",
    continue = "Do not activate the project and use the current library paths.",
    cancel = "Cancel and resolve the situation another way."
  )

  choice <- menu(choices, title, default = "continue")
  switch(choice,
    activate = { activate(project = project); TRUE },
    continue = FALSE,
    cancel = cancel(),
  )
}


# addins.R -------------------------------------------------------------------


renv_addins_embed_ui <- function() {

  miniUI::miniPage(
    miniUI::gadgetTitleBar("Embed a Lockfile"),
    miniUI::miniContentPanel(
      shiny::verticalLayout(
        shiny::fileInput(
          inputId     = "lockfile",
          label       = "Lockfile path:",
          placeholder = "(Use default)"
        )
      )
    )
  )

}

renv_addins_embed_server <- function(input, output, session) {

  shiny::observeEvent(input$done, {

    # notify the user that we're working now
    progress <- shiny::Progress$new(
      session = shiny::getDefaultReactiveDomain(),
      style   = "notification"
    )

    progress$set(message = "Embedding lockfile...")

    # get editor context
    context <- rstudioapi::getSourceEditorContext()

    # validate we have a path
    path <- context$path
    if (!nzchar(path))
      stop("cannot embed lockfile into an unsaved file", call. = FALSE)

    # get project path
    project <- rstudioapi::getActiveProject()

    # read lockfile
    lockfile <- input$lockfile
    if (!is.null(lockfile))
      lockfile <- renv_lockfile_read(file = lockfile$datapath)

    # save document and run embed
    rstudioapi::documentSave(id = context$id)
    embed(path = path, lockfile = lockfile, project = project)

    # stop app
    invisible(shiny::stopApp())

  })

}

renv_addins_embed <- function() {

  # first, check that shiny and miniUI are available
  for (package in c("miniUI", "rstudioapi", "shiny")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      fmt <- "required package '%s' is not available"
      stopf(fmt, package)
    }
  }

  # ask the user to save the document first if necessary
  context <- rstudioapi::getSourceEditorContext()
  if (!nzchar(context$path))
    stop("this addin cannot be run with an unsaved document")

  # okay, we can run the addin
  shiny::runGadget(
    app    = renv_addins_embed_ui(),
    server = renv_addins_embed_server,
    viewer = shiny::dialogViewer(
      dialogName = "Embed Lockfile",
      width  = 400,
      height = 200
    )
  )
}



# aliases.R ------------------------------------------------------------------


# aliases used primarily for nicer / normalized text output
the$aliases <- list(
  bioc         = "Bioconductor",
  bioconductor = "Bioconductor",
  bitbucket    = "Bitbucket",
  cellar       = "Cellar",
  cran         = "CRAN",
  git2r        = "Git",
  github       = "GitHub",
  gitlab       = "GitLab",
  local        = "Local",
  repository   = "Repository",
  standard     = "Repository",
  url          = "URL",
  xgit         = "Git"
)

alias <- function(text) {
  the$aliases[[text]] %||% text
}


# archive.R ------------------------------------------------------------------


renv_archive_type <- function(archive) {

  ext <- fileext(archive)

  if (ext %in% c(".tgz", ".tar", ".tar.gz"))
    return("tar")
  else if (ext %in% c(".zip"))
    return("zip")
  else
    return("unknown")

}

renv_archive_list <- function(archive) {
  suppressWarnings(renv_archive_list_impl(archive))
}

renv_archive_list_impl <- function(archive) {

  switch(
    renv_archive_type(archive),
    tar = untar(archive, list = TRUE),
    zip = unzip(archive, list = TRUE)[["Name"]],
    stopf("don't know how to list files in archive '%s'", basename(archive))
  )

}

renv_archive_decompress <- function(archive, files = NULL, exdir = ".", ...) {

  switch(
    renv_archive_type(archive),
    tar = renv_archive_decompress_tar(archive, files = files, exdir = exdir, ...),
    zip = renv_archive_decompress_zip(archive, files = files, exdir = exdir, ...),
    stopf("don't know how to decompress archive '%s'", basename(archive))
  )

}

renv_archive_decompress_tar <- function(archive, files = NULL, exdir = ".", ...) {

  # if an appropriate system tar is available, use it
  tar <- renv_tar_exe()
  if (nzchar(tar))
    return(renv_tar_decompress(tar, archive = archive, files = files, exdir = exdir, ...))

  # when using internal TAR, we want to suppress warnings
  # (otherwise we get noise about global PAX headers)
  suppressWarnings(untar(archive, files = files, exdir = exdir, tar = "internal", ...))
  return(TRUE)

}

renv_archive_decompress_zip <- function(archive, files = NULL, exdir = ".", ...) {

  # the default unzip tool will give warnings rather than
  # errors if R was unable to extract from a zip archive
  status <- tryCatch(
    unzip(archive, files = files, exdir = exdir, ...),
    condition = identity
  )

  if (inherits(status, "condition")) {
    fmt <- "failed to decompress '%s' [%s]"
    stopf(fmt, basename(archive), conditionMessage(status))
  }

  TRUE

}

renv_archive_find <- function(archive, pattern) {
  files <- renv_archive_list(archive)
  grep(pattern, files, value = TRUE)
}

renv_archive_read <- function(archive, file) {

  type <- renv_archive_type(archive)
  case(
    type == "tar" ~ renv_archive_read_tar(archive, file),
    type == "zip" ~ renv_archive_read_zip(archive, file),
    ~ stopf("don't know how to read file from archive %s", renv_path_pretty(archive))
  )

}

renv_archive_read_tar <- function(archive, file) {

  # if an appropriate tar is available, use it
  tar <- renv_tar_exe()
  if (nzchar(tar)) {
    args <- c("xf", renv_shell_path(archive), "-O", renv_shell_path(file))
    return(renv_system_exec(tar, args, action = "reading file from archive"))
  }

  # create extraction directory
  exdir <- renv_scope_tempfile("renv-archive-")
  ensure_directory(exdir)

  # unpack the requested file
  suppressWarnings(untar(archive, files = file, exdir = exdir, tar = "internal"))

  # and read it
  archive <- file.path(exdir, file)
  readLines(archive, warn = FALSE)

}

renv_archive_read_zip <- function(archive, file) {
  renv_scope_tempdir()
  conn <- unz(archive, file, encoding = "native.enc")
  defer(close(conn))
  readLines(conn, warn = FALSE)
}


# autoload.R -----------------------------------------------------------------


#' Auto-load the active project
#'
#' Automatically load the renv project associated with a particular directory.
#' renv will search parent directories for the renv project root; if found,
#' that project will be loaded via [renv::load()].
#'
#' To enable the renv auto-loader, you can place:
#'
#' ```
#' renv::autoload()
#' ````
#'
#' into your site-wide or user `.Rprofile` to ensure that renv projects are
#' automatically loaded for any newly-launched \R sessions, even if those \R
#' sessions are launched within the sub-directory of an renv project.
#'
#' If you'd like to launch \R within the sub-directory of an renv project
#' without auto-loading renv, you can set the environment variable:
#'
#' ```
#' RENV_AUTOLOAD_ENABLED = FALSE
#' ```
#'
#' before starting \R.
#'
#' Note that `renv::autoload()` is only compatible with projects using
#' `renv 0.15.3` or newer, as it relies on features within the `renv/activate.R`
#' script that are only generated with newer versions of renv.
#'
#' @export
autoload <- function() {
  invisible(renv_autoload_impl())
}

renv_autoload_impl <- function() {

  # check if we're disabled
  enabled <- Sys.getenv("RENV_AUTOLOAD_ENABLED", unset = "TRUE")
  if (!truthy(enabled))
    return(FALSE)

  # bail if load is already being called
  loading <- getOption("renv.load.running")
  if (identical(loading, TRUE))
    return(FALSE)

  # avoid recursion
  running <- getOption("renv.autoload.running")
  if (identical(running, TRUE))
    return(FALSE)

  # set our flag
  renv_scope_options(renv.autoload.running = TRUE)

  # try to find a project
  project <- catch(renv_project_find())
  if (inherits(project, "error"))
    return(FALSE)

  # move to project directory
  renv_scope_wd(project)

  # if we have a project profile, source it
  profile <- file.path(project, ".Rprofile")
  if (file.exists(profile)) {
    sys.source(profile, envir = globalenv())
    return(TRUE)
  }

  # if we have an activate script, run it
  activate <- file.path(project, "renv/activate.R")
  if (file.exists(activate)) {
    sys.source(activate, envir = globalenv())
    return(TRUE)
  }

  # otherwise, just try to load the project
  load(project)
  TRUE

}


# available-packages.R -------------------------------------------------------


# tools for querying information about packages available on CRAN.
# note that this does _not_ merge package entries from multiple repositories;
# rather, a list of databases is returned (one for each repository)
available_packages <- function(type,
                               repos = NULL,
                               limit = NULL,
                               quiet = FALSE,
                               cellar = FALSE)
{
  dynamic(

    key = list(
      type = type,
      repos = repos %||% getOption("repos"),
      cellar = cellar
    ),

    value = renv_available_packages_impl(
      type   = type,
      repos  = repos,
      limit  = limit,
      quiet  = quiet,
      cellar = cellar
    )

  )
}

renv_available_packages_impl <- function(type,
                                         repos = NULL,
                                         limit = NULL,
                                         quiet = FALSE,
                                         cellar = FALSE)
{
  limit <- limit %||% Sys.getenv("R_AVAILABLE_PACKAGES_CACHE_CONTROL_MAX_AGE", "3600")
  repos <- renv_repos_normalize(repos %||% getOption("repos"))

  # invalidate cache if http_proxy or https_proxy environment variables change,
  # since those could effect (or even re-direct?) repository URLs
  envkeys <- c("http_proxy", "https_proxy", "HTTP_PROXY", "HTTPS_PROXY")
  envvals <- Sys.getenv(envkeys, unset = NA)

  # invalidate the cache if 'renv.download.headers' changes as well
  headers <- getOption("renv.download.headers")
  key <- list(repos = repos, type = type, headers = headers, envvals)

  # retrieve available packages
  dbs <- if (length(repos)) index(
    scope = "available-packages",
    key   = key,
    value = renv_available_packages_query(type, repos, quiet),
    limit = as.integer(limit)
  )

  # include cellar if requested
  dbs[["__renv_cellar__"]] <- if (cellar)
    renv_available_packages_cellar(type = type)

  dbs

}

renv_available_packages_query <- function(type, repos, quiet = FALSE) {

  if (quiet)
    renv_scope_options(renv.verbose = FALSE)

  fmt <- "- Querying repositories for available %s packages ... "
  printf(fmt, type)

  # exclude repositories which are known to not have packages available
  if (type == "binary") {
    ignored <- setdiff(grep("^BioC", names(repos), value = TRUE), "BioCsoft")
    repos <- repos[setdiff(names(repos), ignored)]
  }

  # request repositories
  urls <- contrib.url(repos, type)
  errors <- new.env(parent = emptyenv())
  dbs <- map(urls, renv_available_packages_query_impl, type = type, errors = errors)
  names(dbs) <- names(repos)

  # notify finished
  writef("Done!")

  # propagate errors
  errors <- as.list(errors)
  if (empty(errors))
    return(dbs)

  header <- "renv was unable to query available packages from the following repositories:"
  msgs <- enum_chr(errors, function(url, cnds) {
    msgs <- map_chr(cnds, conditionMessage)
    paste(c(header(url), msgs, ""), collapse = "\n")
  })

  caution_bullets(header, msgs)
  filter(dbs, Negate(is.null))

}

renv_available_packages_query_impl_packages_rds <- function(url) {
  path <- file.path(url, "PACKAGES.rds")
  destfile <- renv_scope_tempfile("renv-packages-", fileext = ".rds")

  download(url = path, destfile = destfile, quiet = TRUE)
  suppressWarnings(readRDS(destfile))
}

renv_available_packages_query_impl_packages_gz <- function(url) {
  path <- file.path(url, "PACKAGES.gz")
  destfile <- renv_scope_tempfile("renv-packages-", fileext = ".gz")

  download(url = path, destfile = destfile, quiet = TRUE)
  suppressWarnings(read.dcf(destfile))
}

renv_available_packages_query_impl_packages <- function(url) {
  path <- file.path(url, "PACKAGES")
  destfile <- renv_scope_tempfile("renv-packages-")

  download(url = path, destfile = destfile, quiet = TRUE)
  suppressWarnings(read.dcf(destfile))
}

renv_available_packages_query_impl <- function(url, type, errors) {

  # define query_impl methods for the different PACKAGES
  methods <- list(
    renv_available_packages_query_impl_packages_rds,
    renv_available_packages_query_impl_packages_gz,
    renv_available_packages_query_impl_packages
  )

  stack <- stack()
  seize <- function(restart) {
    function(condition) {
      stack$push(condition)
      invokeRestart(restart)
    }
  }

  for (method in methods) {

    db <- withCallingHandlers(
      catch(method(url)),
      warning = seize(restart = "muffleWarning"),
      message = seize(restart = "muffleMessage")
    )

    if (inherits(db, "error")) {
      stack$push(db)
      next
    }

    return(renv_available_packages_success(db, url, type))

  }

  assign(url, stack$data(), envir = errors)
  NULL

}

renv_available_packages_success <- function(db, url, type) {

  # convert to data.frame
  db <- as_data_frame(db)
  if (nrow(db) == 0L)
    return(db)

  # build repository url
  repository <- rep.int(url, nrow(db))

  # update with path
  path <- db$Path
  if (length(path)) {
    set <- !is.na(path)
    repository[set] <- paste(url, path[set], sep = "/")
  }

  # set it
  db$Repository <- repository

  # add in necessary missing columns
  required <- c(
    "Package", "Version", "Priority",
    "Depends", "Imports", "LinkingTo", "Suggests", "Enhances",
    "License", "License_is_FOSS", "License_restricts_use",
    "OS_type", "Archs", "MD5sum",
    if (type %in% "source") "NeedsCompilation",
    "File", "Repository"
  )

  missing <- setdiff(required, names(db))
  db[missing] <- NA_character_
  db <- db[required]

  # filter as appropriate
  db <- renv_available_packages_filter(db)

  # remove row names
  row.names(db) <- NULL

  # ok
  db

}

renv_available_packages_entry <- function(package,
                                          type   = "source",
                                          repos  = NULL,
                                          filter = NULL,
                                          quiet  = FALSE,
                                          prefer = NULL)
{

  # if filter is a string, treat it as an explicit version requirement
  version <- NULL
  if (is.character(filter)) {
    version <- filter
    filter <- function(entries) {
      matches <- which(entries$Version == version)
      candidate <- head(matches, n = 1L)
      entries[candidate, ]
    }
  }

  # by default, provide a filter that selects the newest-available package
  filter <- filter %||% function(entries) {
    version <- numeric_version(entries$Version)
    ordered <- order(version, decreasing = TRUE)
    entries[ordered[[1]], ]
  }

  # read available packages
  dbs <- available_packages(
    type  = type,
    repos = repos,
    quiet = quiet
  )

  # if a preferred repository is marked and available, prefer using that
  if (length(prefer) == 1L && prefer %in% names(dbs)) {
    idx <- match(prefer, names(dbs))
    ord <- c(idx, setdiff(seq_along(dbs), idx))
    dbs <- dbs[ord]
  }

  # iterate through repositories, and find first matching
  for (i in seq_along(dbs)) {

    db <- dbs[[i]]
    matches <- which(db$Package == package)
    if (empty(matches))
      next

    entries <- db[matches, ]
    entry <- filter(entries)
    if (nrow(entry) == 0)
      next

    entry[["Type"]] <- type
    entry[["Name"]] <- names(dbs)[[i]] %||% ""
    return(entry)

  }

  # report package + version if both available
  pkgver <- if (length(version))
    paste(package, version)
  else
    package

  fmt <- "failed to find %s for '%s' in package repositories"
  stopf(fmt, type, pkgver)

}

renv_available_packages_record <- function(entry, type) {

  # check to see if this is already a proper record
  attrs <- attributes(entry)
  keys <- c("type", "url")
  if (all(keys %in% names(attrs)))
    return(entry)

  # otherwise, construct it
  record <- entry

  if (identical(record$Name, "__renv_cellar__")) {
    record$Source     <- "Cellar"
    record$Repository <- NULL
    record$Name       <- NULL
  } else {
    record$Source     <- "Repository"
    record$Repository <- entry$Name
    record$Name       <- NULL
  }

  # form url
  url <- entry$Repository
  path <- entry$Path
  if (length(path) && !is.na(path))
    url <- paste(url, path, sep = "/")

  attr(record, "type") <- type
  attr(record, "url")  <- url

  record

}

renv_available_packages_latest_repos_impl <- function(package, type, repos) {

  # get available packages
  dbs <- available_packages(
    type   = type,
    repos  = repos,
    quiet  = TRUE,
    cellar = TRUE
  )

  fields <- c(
    "Package", "Version",
    "OS_type", "NeedsCompilation",
    "Repository", "Path", "File"
  )

  entries <- bapply(dbs, function(db) {

    # extract entries for this package
    entries <- rows(db, db$Package == package)
    if (nrow(entries) == 0L)
      return(entries)

    # keep only compatible rows + the required fields
    cols(entries, intersect(fields, names(db)))

  }, index = "Name")

  if (is.null(entries))
    return(NULL)

  # sort based on version
  version <- numeric_version(entries$Version)
  ordered <- order(version, decreasing = TRUE)

  # extract newest entry
  entry <- as.list(entries[ordered[[1L]], ])

  # remove an NA file entry if necessary
  # https://github.com/rstudio/renv/issues/1045
  if (length(entry$File) && is.na(entry$File))
    entry$File <- NULL

  # return newest-available version
  renv_available_packages_record(entry, type)

}

renv_available_packages_latest <- function(package,
                                           type = NULL,
                                           repos = NULL)
{
  methods <- list(
    renv_available_packages_latest_repos,
    if (renv_mran_enabled())
      renv_available_packages_latest_mran
  )

  errors <- stack()

  entries <- lapply(methods, function(method) {

    if (is.null(method))
      return(NULL)

    entry <- catch(method(package, type, repos))
    if (inherits(entry, "error")) {
      errors$push(entry)
      return(NULL)
    }

    entry

  })

  # if both entries are null, error
  if (all(map_lgl(entries, is.null))) {
    map(errors$data(), warning)
    stopf("package '%s' is not available", package)
  } else if (is.null(entries[[2L]])) {
    return(entries[[1L]])
  } else if (is.null(entries[[1L]])) {
    return(entries[[2L]])
  }

  # extract both entries
  lhs <- entries[[1L]]
  rhs <- entries[[2L]]

  # extract versions
  lhsv <- package_version(lhs$Version %||% "0.0")
  rhsv <- package_version(rhs$Version %||% "0.0")

  # if the versions don't match, take the newest one
  if (lhsv > rhsv)
    return(lhs)
  else if (rhsv > lhsv)
    return(rhs)

  # otherwise, if we have a binary from the active package repositories,
  # use those; otherwise, use the mran binary
  if (identical(lhsv, rhsv)) {
    if (identical(attr(lhs, "type", exact = TRUE), "binary"))
      return(lhs)
    else
      return(rhs)
  }

  # otherwise, return the regular repository entry
  lhs

}

renv_available_packages_latest_mran <- function(package,
                                                type = NULL,
                                                repos = NULL)
{
  if (!config$mran.enabled())
    stop("MRAN is not enabled")

  type <- type %||% getOption("pkgType")
  if (identical(type, "source"))
    stop("MRAN database requires binary packages to be available")

  # ensure local MRAN database is up-to-date
  renv_mran_database_refresh(explicit = FALSE)

  # attempt to read it
  database <- catch(renv_mran_database_load())
  if (inherits(database, "error"))
    return(database)

  # get entry for this version of R + platform
  suffix <- contrib.url("", type = "binary")
  entry <- database[[suffix]]
  if (is.null(entry))
    stopf("no MRAN records available from repository URL '%s'", suffix)

  # find all available packages
  keys <- attr(entry, "keys")
  pattern <- paste0("^", package, " ")
  matching <- grep(pattern, keys, perl = TRUE, value = TRUE)
  if (empty(matching))
    stopf("package '%s' is not available from MRAN", package)

  # take the latest-available package
  entries <- unlist(mget(matching, envir = entry))
  sorted <- sort(entries, decreasing = TRUE)
  key <- names(sorted)[[1L]]
  idate <- sorted[[1L]]

  # split into package, version
  index <- regexpr(" ", key, fixed = TRUE)
  version <- substring(key, index + 1)

  # return an appropriate record
  record <- list(
    Package    = package,
    Version    = version,
    Source     = "Repository",
    Repository = "MRAN"
  )

  # convert from integer to date
  date <- as.Date(idate, origin = "1970-01-01")

  # form url to binary package
  base <- renv_mran_url(date, suffix)
  name <- renv_retrieve_name(record, type = "binary")
  url <- file.path(base, name)

  # tag record with url + type
  attr(record, "url")  <- dirname(url)
  attr(record, "type") <- "binary"

  record
}

renv_available_packages_latest_repos <- function(package,
                                                 type = NULL,
                                                 repos = NULL)
{
  type  <- type %||% getOption("pkgType")
  repos <- repos %||% getOption("repos")

  # detect requests for only source packages
  if (identical(type, "source"))
    return(renv_available_packages_latest_repos_impl(package, "source", repos))

  # detect requests for only binary packages
  if (grepl("\\bbinary\\b", type))
    return(renv_available_packages_latest_repos_impl(package, "binary", repos))

  # otherwise, check both source and binary repositories
  src <- renv_available_packages_latest_repos_impl(package, "source", repos)
  bin <- renv_available_packages_latest_repos_impl(package, "binary", repos)

  # choose an appropriate record
  if (is.null(src) && is.null(bin))
    stopf("package '%s' is not available", package)
  else if (is.null(src))
    renv_available_packages_record(bin, "binary")
  else if (is.null(bin))
    renv_available_packages_record(src, "source")
  else
    renv_available_packages_latest_select(src, bin)
}

renv_available_packages_latest_select <- function(src, bin) {

  # if the binary is at least as old as the source version,
  # then use the binary version
  if (renv_version_compare(bin$Version, src$Version) >= 0)
    return(renv_available_packages_record(bin, "binary"))

  # if the user has requested we skip source repositories,
  # use the binary anyway
  ipcs <- getOption("install.packages.check.source", default = "yes")
  if (!identical(ipcs, "yes"))
    return(renv_available_packages_record(bin, "binary"))

  # if the package requires compilation, check to see whether
  # the user has opted in to compiling packages from source
  nc <- identical(src$NeedsCompilation, "yes")
  if (nc) {

    # check user preference re: compilation from source
    ipcfs <- getOption(
      "install.packages.compile.from.source",
      default = Sys.getenv("R_COMPILE_AND_INSTALL_PACKAGES")
    )

    # if make is not available, then we can't build from source
    make <- Sys.getenv("MAKE", unset = "make")
    if (!nzchar(Sys.which(make)))
      ipcfs <- "never"

    # if we're on macOS and command line tools are not available,
    # then we can't build from sources
    if (renv_platform_macos() && !renv_xcode_available())
      ipcfs <- "never"

    if (identical(ipcfs, "never"))
      return(renv_available_packages_record(bin, "binary"))

  }

  # take the source version
  renv_available_packages_record(src, "source")

}

renv_available_packages_cellar <- function(type, project = NULL) {

  # look in the cellar
  project <- renv_project_resolve(project)
  roots <- renv_cellar_roots(project = project)

  # look for packages
  all <- list.files(
    path         = roots,
    all.files    = TRUE,
    full.names   = TRUE,
    recursive    = TRUE,
    include.dirs = FALSE
  )

  # keep only files with matching extensions
  ext <- renv_package_ext(type = type)
  keep <- all[fileext(all) %in% ext]

  # construct records for each cellar entry
  records <- lapply(keep, function(path) {

    # infer package name, version from tarball name
    base <- basename(keep)
    idx <- regexpr("_", base, fixed = TRUE)
    package <- substring(base, 1L, idx - 1L)
    version <- substring(base, idx + 1L, nchar(base) - nchar(ext))

    # set the Repository field
    prefix <- if (renv_platform_windows()) "file:///" else "file://"
    repository <- paste0(prefix, dirname(path))

    # build record
    list(
      Package = package,
      Version = version,
      Repository = repository
    )

  })

  bind(records)

}

renv_available_packages_filter <- function(db) {

  # sanity check
  if (is.null(db) || nrow(db) == 0L)
    return(db)

  # TODO: subarch? duplicates?
  # remove packages which won't work on this OS
  db <- renv_available_packages_filter_ostype(db)
  db <- renv_available_packages_filter_version(db)

  # return filtered database
  db

}

renv_available_packages_filter_ostype <- function(db) {
  ostype <- db$OS_type
  ok <- is.na(ostype) | ostype %in% .Platform$OS.type
  rows(db, ok)
}

renv_available_packages_filter_version <- function(db) {

  depends <- db$Depends

  # find the packages which express an R dependency
  splat <- strsplit(depends, "\\s*,\\s*", perl = TRUE)

  # remove the non-R dependencies
  table <- c("R ", "R\n", "R(")
  splat <- map(splat, function(requirements) {
    requirements[match(substr(requirements, 1L, 2L), table, 0L) != 0L]
  })

  # collect the unique R dependencies
  dependencies <- unique(unlist(splat))

  # convert this to a simpler form
  pattern <- "^R\\s*\\(([^\\d\\s+]+)\\s*([^\\)]+)\\)$"
  matches <- gsub(pattern, "\\1 \\2", dependencies, perl = TRUE)

  # split into operator and version
  idx <- regexpr(" ", matches, fixed = TRUE)
  ops <- substring(matches, 1L, idx - 1L)
  version <- numeric_version(substring(matches, idx + 1L))

  # bundle the calls for efficiency
  ok <- rep.int(NA, length(ops))
  names(ok) <- dependencies

  # iterate over the operations, and update our vector
  rversion <- getRversion()
  for (op in unique(ops)) {
    idx <- ops == op
    ok[idx] <- do.call(op, list(rversion, version[idx]))
  }

  # now, map the names back to their computed values, and check whether
  # all requirements were satisfied
  ok <- map_lgl(splat, function(requirements) {
    all(ok[requirements])
  })

  rows(db, ok)

}

# flattens available packages, keeping only the newest version
renv_available_packages_flatten <- function(dbs) {

  # stack the databases together
  stacked <- bind(dbs)

  # order by package + version
  # TODO: 'order()' is kind of slow for numeric versions; can we do better?
  index <- with(stacked, order(Package, numeric_version(Version), decreasing = TRUE))
  ordered <- rows(stacked, index)

  # remove duplicates
  dupes <- duplicated(ordered$Package)
  filtered <- rows(ordered, !dupes)

  # ready to return
  filtered

}


# backports.R ----------------------------------------------------------------


if (is.null(.BaseNamespaceEnv$lengths)) {

  lengths <- function(x, use.names = TRUE) {
    vapply(x, length, numeric(1), USE.NAMES = use.names)
  }

}


# base64.R -------------------------------------------------------------------


the$base64_table <- as.integer(charToRaw("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="))

renv_base64_encode_main <- function(input) {

  ni <- as.integer(length(input))
  if (ni < 3L)
    return(integer())

  no <- ni %/% 3L * 4L
  output <- integer(no)

  i0 <- seq.int(1L, ni - 2L, by = 3L)
  i1 <- seq.int(2L, ni - 1L, by = 3L)
  i2 <- seq.int(3L, ni - 0L, by = 3L)

  o0 <- seq.int(1L, no - 3L, by = 4L)
  o1 <- seq.int(2L, no - 2L, by = 4L)
  o2 <- seq.int(3L, no - 1L, by = 4L)
  o3 <- seq.int(4L, no - 0L, by = 4L)

  output[o0] <- the$base64_table[1L + bitwShiftR(input[i0], 2L)]

  output[o1] <- the$base64_table[1L + bitwOr(
    bitwShiftL(bitwAnd(input[i0], 0x03L), 4L),
    bitwShiftR(bitwAnd(input[i1], 0xF0L), 4L)
  )]

  output[o2] <- the$base64_table[1L + bitwOr(
    bitwShiftL(bitwAnd(input[i1], 0x0FL), 2L),
    bitwShiftR(bitwAnd(input[i2], 0xC0L), 6L)
  )]

  output[o3] <- the$base64_table[1L + bitwAnd(input[i2], 0x3FL)]

  output

}

renv_base64_encode_rest <- function(input) {

  ni <- as.integer(length(input))
  remaining <- ni %% 3L
  if (remaining == 0L)
    return(integer())

  output <- rep.int(61L, 4L)
  i <- ni - remaining + 1

  output[1L] <- the$base64_table[1L + bitwShiftR(input[i + 0L], 2L)]

  if (remaining == 1L) {

    output[2L] <- the$base64_table[1L + bitwShiftL(bitwAnd(input[i + 0L], 0x03L), 4L)]

  } else if (remaining == 2L) {

    output[2L] <- the$base64_table[1L + bitwOr(
      bitwShiftL(bitwAnd(input[i + 0L], 0x03L), 4L),
      bitwShiftR(bitwAnd(input[i + 1L], 0xF0L), 4L)
    )]

    output[3L] <- the$base64_table[1L + bitwShiftL(bitwAnd(input[i + 1L], 0x0FL), 2L)]

  }

  output

}

renv_base64_encode <- function(text) {

  # convert to raw vector
  input <- case(
    is.character(text) ~ as.integer(charToRaw(text)),
    is.raw(text)       ~ as.integer(text),
    ~ stopf("unexpected input type '%s'", typeof(text))
  )

  encoded <- c(
    renv_base64_encode_main(input),
    renv_base64_encode_rest(input)
  )

  rawToChar(as.raw(encoded))

}

the$base64_decode_table <- NULL
renv_base64_decode_table <- function() {
  the$base64_decode_table <- the$base64_decode_table %||% {
    table <- integer(255)
    text <- "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
    table[utf8ToInt(text)] <- seq_len(nchar(text)) - 1L
    table
  }
}

renv_base64_decode_main <- function(input) {

  ni <- length(input)
  no <- (ni * 3L) %/% 4L

  output <- integer(no)

  i0 <- seq(from = 1L, to = ni - 3L, by = 4L)
  i1 <- seq(from = 2L, to = ni - 2L, by = 4L)
  i2 <- seq(from = 3L, to = ni - 1L, by = 4L)
  i3 <- seq(from = 4L, to = ni - 0L, by = 4L)

  o0 <- seq.int(1L, no - 2L, by = 3L)
  o1 <- seq.int(2L, no - 1L, by = 3L)
  o2 <- seq.int(3L, no - 0L, by = 3L)

  t <- renv_base64_decode_table()

  output[o0] <- bitwOr(
    bitwAnd(bitwShiftL(t[input[i0]], 2L), 255L),
    bitwAnd(bitwShiftR(t[input[i1]], 4L), 255L)
  )

  output[o1] <- bitwOr(
    bitwAnd(bitwShiftL(t[input[i1]], 4L), 255L),
    bitwAnd(bitwShiftR(t[input[i2]], 2L), 255L)
  )

  output[o2] <- bitwOr(
    bitwAnd(bitwShiftL(t[input[i2]], 6L), 255L),
    bitwAnd(bitwShiftR(t[input[i3]], 0L), 255L)
  )

  output

}

renv_base64_decode <- function(encoded) {

  # remove newlines
  if (c(regexpr("\n", encoded, fixed = TRUE)) != -1L)
    encoded <- gsub("\n", "", encoded, fixed = TRUE)

  # convert to raw vector
  input <- case(
    is.character(encoded) ~ as.integer(charToRaw(encoded)),
    is.raw(encoded)       ~ as.integer(encoded),
    ~ stopf("unexpected input type '%s'", typeof(encoded))
  )

  # decode vector
  output <- renv_base64_decode_main(input)

  # trim off padded bits
  n <- length(input)
  if (input[n - 1L] == 61L)
    output <- head(output, n = -2L)
  else if (input[n] == 61L)
    output <- head(output, n = -1L)

  # convert back to string
  rawToChar(as.raw(output))

}


# bind.R ---------------------------------------------------------------------


bind <- function(data, names = NULL, index = "Index") {

  # keep only non-empty data
  data <- Filter(NROW, data)
  if (!length(data))
    return(NULL)

  # check for quick exit
  if (length(data) == 1L) {

    # no-name case
    if (is.null(names(data))) {
      rhs <- data[[1L]]
      names(rhs) <- names(rhs) %||% names
      return(as_data_frame(rhs))
    }

    # named case
    lhs <- list(rep.int(names(data), times = NROW(data[[1L]])))
    names(lhs) <- index
    rhs <- as.list(data[[1L]])
    return(as_data_frame(c(lhs, rhs)))

  }

  # ensure all datasets have the same column names
  # try to preserve the ordering of names if possible
  # (try to find one dataset which has all column relevant column names)
  nms <- character()
  for (i in seq_along(data)) {
    names(data[[i]]) <- names(data[[i]]) %||% names
    nmsi <- names(data[[i]])
    if (length(nmsi) > length(nms))
      nms <- nmsi
  }

  # check now if we've caught all relevant names; if we didn't,
  # just fall back to a "dumb" union
  allnms <- unique.default(unlist(lapply(data, names), use.names = FALSE))
  if (!setequal(nms, allnms))
    nms <- allnms

  # we've collected all names; now fill with NAs as necessary
  filled <- map(data, function(datum) {
    datum[setdiff(nms, names(datum))] <- NA
    datum[nms]
  })

  # we've collected and ordered each data.frame, now merge them
  rhs <- .mapply(c, filled, list(use.names = FALSE))
  names(rhs) <- names(filled[[1L]])

  if (is.null(names(data))) {
    names(rhs) <- names(rhs) %||% names
    return(as_data_frame(rhs))
  }

  if (index %in% names(rhs)) {
    fmt <- "name collision: bound list already contains column called '%s'"
    stopf(fmt, index)
  }

  lhs <- list()
  rows <- function(item) nrow(item) %||% length(item[[1L]])
  lhs[[index]] <- rep.int(names(filled), times = map_dbl(filled, rows))

  as_data_frame(c(lhs, rhs))

}







# binding.R ------------------------------------------------------------------


renv_binding_lock <- function(envir, symbol) {
  .BaseNamespaceEnv$lockBinding(symbol, envir)
}

renv_binding_locked <- function(envir, symbol) {
  .BaseNamespaceEnv$bindingIsLocked(symbol, envir)
}

renv_binding_unlock <- function(envir, symbol) {
  .BaseNamespaceEnv$unlockBinding(symbol, envir)
}

renv_binding_replace <- function(envir, symbol, replacement) {

  # get the original definition
  original <- envir[[symbol]]

  # if the binding is locked, temporarily unlock it
  if (renv_binding_locked(envir, symbol)) {
    defer(renv_binding_lock(envir, symbol))
    renv_binding_unlock(envir, symbol)
  }

  # update the binding
  assign(symbol, replacement, envir = envir)

  # return old definition
  original

}


# bioconductor.R -------------------------------------------------------------


renv_bioconductor_manager <- function() {
  if (getRversion() >= "3.5.0")
    "BiocManager"
  else
    "BiocInstaller"
}

renv_bioconductor_init <- function(library = NULL) {
  renv_scope_options(renv.verbose = FALSE)

  if (identical(renv_bioconductor_manager(), "BiocManager"))
    renv_bioconductor_init_biocmanager(library)
  else
    renv_bioconductor_init_biocinstaller(library)
}

renv_bioconductor_init_biocmanager <- function(library = NULL) {

  library <- library %||% renv_libpaths_active()
  if (renv_package_installed("BiocManager", lib.loc = library))
    return(TRUE)

  ensure_directory(library)
  install("BiocManager", library = library)
  TRUE

}

renv_bioconductor_init_biocinstaller <- function(library = NULL) {

  library <- library %||% renv_libpaths_active()
  if (renv_package_installed("BiocInstaller", lib.loc = library))
    return(TRUE)

  url <- "https://bioconductor.org/biocLite.R"
  destfile <- renv_scope_tempfile("renv-bioclite-", fileext = ".R")
  download(url, destfile = destfile, quiet = TRUE)

  ensure_directory(library)
  renv_scope_libpaths(library)
  source(destfile)
  TRUE

}

renv_bioconductor_version <- function(project, refresh = FALSE) {

  # check and see if we have an override via option
  version <- getOption("renv.bioconductor.version")
  if (!is.null(version))
    return(version)

  # check and see if the project has been configured to use a specific
  # Bioconductor release
  if (!refresh) {
    version <- settings$bioconductor.version(project = project)
    if (length(version))
      return(version)
  }

  # if BiocVersion is installed, use it
  if (renv_package_available("BiocVersion"))
    return(format(packageVersion("BiocVersion")[1, 1:2]))

  # make sure the required bioc package is available
  renv_bioconductor_init()

  # otherwise, infer the Bioconductor version from installed packages
  case(

    renv_package_available("BiocManager") ~ {
      BiocManager <- renv_scope_biocmanager()
      format(BiocManager$version())
    },

    renv_package_available("BiocVersion") ~ {
      BiocInstaller <- renv_namespace_load("BiocInstaller")
      format(BiocInstaller$biocVersion())
    }

  )

}

# Returns the union of the inferred Bioconductor repositories, together with the
# current value of the 'repos' R option. The Bioconductor repositories are
# placed first in the repository list.
renv_bioconductor_repos <- function(project = NULL, version = NULL) {

  # allow bioconductor repos override
  repos <- getOption("renv.bioconductor.repos")
  if (!is.null(repos))
    return(repos)

  # make sure the required bioc package is available
  renv_bioconductor_init()

  # read Bioconductor version (normally set during restore)
  version <- version %||% renv_bioconductor_version(project = project)

  # read Bioconductor repositories (prefer BiocInstaller for older R)
  if (identical(renv_bioconductor_manager(), "BiocManager"))
    renv_bioconductor_repos_biocmanager(version)
  else
    renv_bioconductor_repos_biocinstaller(version)

}

renv_bioconductor_repos_biocmanager <- function(version) {

  BiocManager <- renv_scope_biocmanager()
  version <- version %||% BiocManager$version()

  tryCatch(
    BiocManager$.repositories(site_repository = character(), version = version),
    error = function(e) {
      BiocManager$repositories(version = version)
    }
  )

}

renv_bioconductor_repos_biocinstaller <- function(version) {
  BiocInstaller <- asNamespace("BiocInstaller")
  version <- version %||% BiocInstaller$biocVersion()
  BiocInstaller$biocinstallRepos(version = version)
}

renv_bioconductor_required <- function(records) {

  for (record in records)
    if (identical(record$Source, "Bioconductor"))
      return(TRUE)

  FALSE

}


# bootstrap.R ----------------------------------------------------------------


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

catf <- function(fmt, ..., appendLF = TRUE) {

  quiet <- getOption("renv.bootstrap.quiet", default = FALSE)
  if (quiet)
    return(invisible())

  msg <- sprintf(fmt, ...)
  cat(msg, file = stdout(), sep = if (appendLF) "\n" else "")

  invisible(msg)

}

header <- function(label,
                   ...,
                   prefix = "#",
                   suffix = "-",
                   n = min(getOption("width"), 78))
{
  label <- sprintf(label, ...)
  n <- max(n - nchar(label) - nchar(prefix) - 2L, 8L)
  if (n <= 0)
    return(paste(prefix, label))

  tail <- paste(rep.int(suffix, n), collapse = "")
  paste0(prefix, " ", label, " ", tail)

}

startswith <- function(string, prefix) {
  substring(string, 1, nchar(prefix)) == prefix
}

bootstrap <- function(version, library) {

  friendly <- renv_bootstrap_version_friendly(version)
  section <- header(sprintf("Bootstrapping renv %s", friendly))
  catf(section)

  # attempt to download renv
  catf("- Downloading renv ... ", appendLF = FALSE)
  withCallingHandlers(
    tarball <- renv_bootstrap_download(version),
    error = function(err) {
      catf("FAILED")
      stop("failed to download:\n", conditionMessage(err))
    }
  )
  catf("OK")
  on.exit(unlink(tarball), add = TRUE)

  # now attempt to install
  catf("- Installing renv  ... ", appendLF = FALSE)
  withCallingHandlers(
    status <- renv_bootstrap_install(version, tarball, library),
    error = function(err) {
      catf("FAILED")
      stop("failed to install:\n", conditionMessage(err))
    }
  )
  catf("OK")

  # add empty line to break up bootstrapping from normal output
  catf("")

  return(invisible())
}

renv_bootstrap_tests_running <- function() {
  getOption("renv.tests.running", default = FALSE)
}

renv_bootstrap_repos <- function() {

  # get CRAN repository
  cran <- getOption("renv.repos.cran", "https://cloud.r-project.org")

  # check for repos override
  repos <- Sys.getenv("RENV_CONFIG_REPOS_OVERRIDE", unset = NA)
  if (!is.na(repos)) {

    # check for RSPM; if set, use a fallback repository for renv
    rspm <- Sys.getenv("RSPM", unset = NA)
    if (identical(rspm, repos))
      repos <- c(RSPM = rspm, CRAN = cran)

    return(repos)

  }

  # check for lockfile repositories
  repos <- tryCatch(renv_bootstrap_repos_lockfile(), error = identity)
  if (!inherits(repos, "error") && length(repos))
    return(repos)

  # retrieve current repos
  repos <- getOption("repos")

  # ensure @CRAN@ entries are resolved
  repos[repos == "@CRAN@"] <- cran

  # add in renv.bootstrap.repos if set
  default <- c(FALLBACK = "https://cloud.r-project.org")
  extra <- getOption("renv.bootstrap.repos", default = default)
  repos <- c(repos, extra)

  # remove duplicates that might've snuck in
  dupes <- duplicated(repos) | duplicated(names(repos))
  repos[!dupes]

}

renv_bootstrap_repos_lockfile <- function() {

  lockpath <- Sys.getenv("RENV_PATHS_LOCKFILE", unset = "renv.lock")
  if (!file.exists(lockpath))
    return(NULL)

  lockfile <- tryCatch(renv_json_read(lockpath), error = identity)
  if (inherits(lockfile, "error")) {
    warning(lockfile)
    return(NULL)
  }

  repos <- lockfile$R$Repositories
  if (length(repos) == 0)
    return(NULL)

  keys <- vapply(repos, `[[`, "Name", FUN.VALUE = character(1))
  vals <- vapply(repos, `[[`, "URL", FUN.VALUE = character(1))
  names(vals) <- keys

  return(vals)

}

renv_bootstrap_download <- function(version) {

  sha <- attr(version, "sha", exact = TRUE)

  methods <- if (!is.null(sha)) {

    # attempting to bootstrap a development version of renv
    c(
      function() renv_bootstrap_download_tarball(sha),
      function() renv_bootstrap_download_github(sha)
    )

  } else {

    # attempting to bootstrap a release version of renv
    c(
      function() renv_bootstrap_download_tarball(version),
      function() renv_bootstrap_download_cran_latest(version),
      function() renv_bootstrap_download_cran_archive(version)
    )

  }

  for (method in methods) {
    path <- tryCatch(method(), error = identity)
    if (is.character(path) && file.exists(path))
      return(path)
  }

  stop("All download methods failed")

}

renv_bootstrap_download_impl <- function(url, destfile) {

  mode <- "wb"

  # https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17715
  fixup <-
    Sys.info()[["sysname"]] == "Windows" &&
    substring(url, 1L, 5L) == "file:"

  if (fixup)
    mode <- "w+b"

  args <- list(
    url      = url,
    destfile = destfile,
    mode     = mode,
    quiet    = TRUE
  )

  if ("headers" %in% names(formals(utils::download.file)))
    args$headers <- renv_bootstrap_download_custom_headers(url)

  do.call(utils::download.file, args)

}

renv_bootstrap_download_custom_headers <- function(url) {

  headers <- getOption("renv.download.headers")
  if (is.null(headers))
    return(character())

  if (!is.function(headers))
    stopf("'renv.download.headers' is not a function")

  headers <- headers(url)
  if (length(headers) == 0L)
    return(character())

  if (is.list(headers))
    headers <- unlist(headers, recursive = FALSE, use.names = TRUE)

  ok <-
    is.character(headers) &&
    is.character(names(headers)) &&
    all(nzchar(names(headers)))

  if (!ok)
    stop("invocation of 'renv.download.headers' did not return a named character vector")

  headers

}

renv_bootstrap_download_cran_latest <- function(version) {

  spec <- renv_bootstrap_download_cran_latest_find(version)
  type  <- spec$type
  repos <- spec$repos

  baseurl <- utils::contrib.url(repos = repos, type = type)
  ext <- if (identical(type, "source"))
    ".tar.gz"
  else if (Sys.info()[["sysname"]] == "Windows")
    ".zip"
  else
    ".tgz"
  name <- sprintf("renv_%s%s", version, ext)
  url <- paste(baseurl, name, sep = "/")

  destfile <- file.path(tempdir(), name)
  status <- tryCatch(
    renv_bootstrap_download_impl(url, destfile),
    condition = identity
  )

  if (inherits(status, "condition"))
    return(FALSE)

  # report success and return
  destfile

}

renv_bootstrap_download_cran_latest_find <- function(version) {

  # check whether binaries are supported on this system
  binary <-
    getOption("renv.bootstrap.binary", default = TRUE) &&
    !identical(.Platform$pkgType, "source") &&
    !identical(getOption("pkgType"), "source") &&
    Sys.info()[["sysname"]] %in% c("Darwin", "Windows")

  types <- c(if (binary) "binary", "source")

  # iterate over types + repositories
  for (type in types) {
    for (repos in renv_bootstrap_repos()) {

      # retrieve package database
      db <- tryCatch(
        as.data.frame(
          utils::available.packages(type = type, repos = repos),
          stringsAsFactors = FALSE
        ),
        error = identity
      )

      if (inherits(db, "error"))
        next

      # check for compatible entry
      entry <- db[db$Package %in% "renv" & db$Version %in% version, ]
      if (nrow(entry) == 0)
        next

      # found it; return spec to caller
      spec <- list(entry = entry, type = type, repos = repos)
      return(spec)

    }
  }

  # if we got here, we failed to find renv
  fmt <- "renv %s is not available from your declared package repositories"
  stop(sprintf(fmt, version))

}

renv_bootstrap_download_cran_archive <- function(version) {

  name <- sprintf("renv_%s.tar.gz", version)
  repos <- renv_bootstrap_repos()
  urls <- file.path(repos, "src/contrib/Archive/renv", name)
  destfile <- file.path(tempdir(), name)

  for (url in urls) {

    status <- tryCatch(
      renv_bootstrap_download_impl(url, destfile),
      condition = identity
    )

    if (identical(status, 0L))
      return(destfile)

  }

  return(FALSE)

}

renv_bootstrap_download_tarball <- function(version) {

  # if the user has provided the path to a tarball via
  # an environment variable, then use it
  tarball <- Sys.getenv("RENV_BOOTSTRAP_TARBALL", unset = NA)
  if (is.na(tarball))
    return()

  # allow directories
  if (dir.exists(tarball)) {
    name <- sprintf("renv_%s.tar.gz", version)
    tarball <- file.path(tarball, name)
  }

  # bail if it doesn't exist
  if (!file.exists(tarball)) {

    # let the user know we weren't able to honour their request
    fmt <- "- RENV_BOOTSTRAP_TARBALL is set (%s) but does not exist."
    msg <- sprintf(fmt, tarball)
    warning(msg)

    # bail
    return()

  }

  catf("- Using local tarball '%s'.", tarball)
  tarball

}

renv_bootstrap_download_github <- function(version) {

  enabled <- Sys.getenv("RENV_BOOTSTRAP_FROM_GITHUB", unset = "TRUE")
  if (!identical(enabled, "TRUE"))
    return(FALSE)

  # prepare download options
  pat <- Sys.getenv("GITHUB_PAT")
  if (nzchar(Sys.which("curl")) && nzchar(pat)) {
    fmt <- "--location --fail --header \"Authorization: token %s\""
    extra <- sprintf(fmt, pat)
    saved <- options("download.file.method", "download.file.extra")
    options(download.file.method = "curl", download.file.extra = extra)
    on.exit(do.call(base::options, saved), add = TRUE)
  } else if (nzchar(Sys.which("wget")) && nzchar(pat)) {
    fmt <- "--header=\"Authorization: token %s\""
    extra <- sprintf(fmt, pat)
    saved <- options("download.file.method", "download.file.extra")
    options(download.file.method = "wget", download.file.extra = extra)
    on.exit(do.call(base::options, saved), add = TRUE)
  }

  url <- file.path("https://api.github.com/repos/rstudio/renv/tarball", version)
  name <- sprintf("renv_%s.tar.gz", version)
  destfile <- file.path(tempdir(), name)

  status <- tryCatch(
    renv_bootstrap_download_impl(url, destfile),
    condition = identity
  )

  if (!identical(status, 0L))
    return(FALSE)

  renv_bootstrap_download_augment(destfile)

  return(destfile)

}

# Add Sha to DESCRIPTION. This is stop gap until #890, after which we
# can use renv::install() to fully capture metadata.
renv_bootstrap_download_augment <- function(destfile) {
  sha <- renv_bootstrap_git_extract_sha1_tar(destfile)
  if (is.null(sha)) {
    return()
  }

  # Untar
  tempdir <- tempfile("renv-github-")
  on.exit(unlink(tempdir, recursive = TRUE), add = TRUE)
  untar(destfile, exdir = tempdir)
  pkgdir <- dir(tempdir, full.names = TRUE)[[1]]

  # Modify description
  desc_path <- file.path(pkgdir, "DESCRIPTION")
  desc_lines <- readLines(desc_path)
  remotes_fields <- c(
    "RemoteType: github",
    "RemoteHost: api.github.com",
    "RemoteRepo: renv",
    "RemoteUsername: rstudio",
    "RemotePkgRef: rstudio/renv",
    paste("RemoteRef: ", sha),
    paste("RemoteSha: ", sha)
  )
  writeLines(c(desc_lines[desc_lines != ""], remotes_fields), con = desc_path)

  # Re-tar
  local({
    old <- setwd(tempdir)
    on.exit(setwd(old), add = TRUE)

    tar(destfile, compression = "gzip")
  })
  invisible()
}

# Extract the commit hash from a git archive. Git archives include the SHA1
# hash as the comment field of the tarball pax extended header
# (see https://www.kernel.org/pub/software/scm/git/docs/git-archive.html)
# For GitHub archives this should be the first header after the default one
# (512 byte) header.
renv_bootstrap_git_extract_sha1_tar <- function(bundle) {

  # open the bundle for reading
  # We use gzcon for everything because (from ?gzcon)
  # > Reading from a connection which does not supply a 'gzip' magic
  # > header is equivalent to reading from the original connection
  conn <- gzcon(file(bundle, open = "rb", raw = TRUE))
  on.exit(close(conn))

  # The default pax header is 512 bytes long and the first pax extended header
  # with the comment should be 51 bytes long
  # `52 comment=` (11 chars) + 40 byte SHA1 hash
  len <- 0x200 + 0x33
  res <- rawToChar(readBin(conn, "raw", n = len)[0x201:len])

  if (grepl("^52 comment=", res)) {
    sub("52 comment=", "", res)
  } else {
    NULL
  }
}

renv_bootstrap_install <- function(version, tarball, library) {

  # attempt to install it into project library
  dir.create(library, showWarnings = FALSE, recursive = TRUE)
  output <- renv_bootstrap_install_impl(library, tarball)

  # check for successful install
  status <- attr(output, "status")
  if (is.null(status) || identical(status, 0L))
    return(status)

  # an error occurred; report it
  header <- "installation of renv failed"
  lines <- paste(rep.int("=", nchar(header)), collapse = "")
  text <- paste(c(header, lines, output), collapse = "\n")
  stop(text)

}

renv_bootstrap_install_impl <- function(library, tarball) {

  # invoke using system2 so we can capture and report output
  bin <- R.home("bin")
  exe <- if (Sys.info()[["sysname"]] == "Windows") "R.exe" else "R"
  R <- file.path(bin, exe)

  args <- c(
    "--vanilla", "CMD", "INSTALL", "--no-multiarch",
    "-l", shQuote(path.expand(library)),
    shQuote(path.expand(tarball))
  )

  system2(R, args, stdout = TRUE, stderr = TRUE)

}

renv_bootstrap_platform_prefix <- function() {

  # construct version prefix
  version <- paste(R.version$major, R.version$minor, sep = ".")
  prefix <- paste("R", numeric_version(version)[1, 1:2], sep = "-")

  # include SVN revision for development versions of R
  # (to avoid sharing platform-specific artefacts with released versions of R)
  devel <-
    identical(R.version[["status"]],   "Under development (unstable)") ||
    identical(R.version[["nickname"]], "Unsuffered Consequences")

  if (devel)
    prefix <- paste(prefix, R.version[["svn rev"]], sep = "-r")

  # build list of path components
  components <- c(prefix, R.version$platform)

  # include prefix if provided by user
  prefix <- renv_bootstrap_platform_prefix_impl()
  if (!is.na(prefix) && nzchar(prefix))
    components <- c(prefix, components)

  # build prefix
  paste(components, collapse = "/")

}

renv_bootstrap_platform_prefix_impl <- function() {

  # if an explicit prefix has been supplied, use it
  prefix <- Sys.getenv("RENV_PATHS_PREFIX", unset = NA)
  if (!is.na(prefix))
    return(prefix)

  # if the user has requested an automatic prefix, generate it
  auto <- Sys.getenv("RENV_PATHS_PREFIX_AUTO", unset = NA)
  if (auto %in% c("TRUE", "True", "true", "1"))
    return(renv_bootstrap_platform_prefix_auto())

  # empty string on failure
  ""

}

renv_bootstrap_platform_prefix_auto <- function() {

  prefix <- tryCatch(renv_bootstrap_platform_os(), error = identity)
  if (inherits(prefix, "error") || prefix %in% "unknown") {

    msg <- paste(
      "failed to infer current operating system",
      "please file a bug report at https://github.com/rstudio/renv/issues",
      sep = "; "
    )

    warning(msg)

  }

  prefix

}

renv_bootstrap_platform_os <- function() {

  sysinfo <- Sys.info()
  sysname <- sysinfo[["sysname"]]

  # handle Windows + macOS up front
  if (sysname == "Windows")
    return("windows")
  else if (sysname == "Darwin")
    return("macos")

  # check for os-release files
  for (file in c("/etc/os-release", "/usr/lib/os-release"))
    if (file.exists(file))
      return(renv_bootstrap_platform_os_via_os_release(file, sysinfo))

  # check for redhat-release files
  if (file.exists("/etc/redhat-release"))
    return(renv_bootstrap_platform_os_via_redhat_release())

  "unknown"

}

renv_bootstrap_platform_os_via_os_release <- function(file, sysinfo) {

  # read /etc/os-release
  release <- utils::read.table(
    file             = file,
    sep              = "=",
    quote            = c("\"", "'"),
    col.names        = c("Key", "Value"),
    comment.char     = "#",
    stringsAsFactors = FALSE
  )

  vars <- as.list(release$Value)
  names(vars) <- release$Key

  # get os name
  os <- tolower(sysinfo[["sysname"]])

  # read id
  id <- "unknown"
  for (field in c("ID", "ID_LIKE")) {
    if (field %in% names(vars) && nzchar(vars[[field]])) {
      id <- vars[[field]]
      break
    }
  }

  # read version
  version <- "unknown"
  for (field in c("UBUNTU_CODENAME", "VERSION_CODENAME", "VERSION_ID", "BUILD_ID")) {
    if (field %in% names(vars) && nzchar(vars[[field]])) {
      version <- vars[[field]]
      break
    }
  }

  # join together
  paste(c(os, id, version), collapse = "-")

}

renv_bootstrap_platform_os_via_redhat_release <- function() {

  # read /etc/redhat-release
  contents <- readLines("/etc/redhat-release", warn = FALSE)

  # infer id
  id <- if (grepl("centos", contents, ignore.case = TRUE))
    "centos"
  else if (grepl("redhat", contents, ignore.case = TRUE))
    "redhat"
  else
    "unknown"

  # try to find a version component (very hacky)
  version <- "unknown"

  parts <- strsplit(contents, "[[:space:]]")[[1L]]
  for (part in parts) {

    nv <- tryCatch(numeric_version(part), error = identity)
    if (inherits(nv, "error"))
      next

    version <- nv[1, 1]
    break

  }

  paste(c("linux", id, version), collapse = "-")

}

renv_bootstrap_library_root_name <- function(project) {

  # use project name as-is if requested
  asis <- Sys.getenv("RENV_PATHS_LIBRARY_ROOT_ASIS", unset = "FALSE")
  if (asis)
    return(basename(project))

  # otherwise, disambiguate based on project's path
  id <- substring(renv_bootstrap_hash_text(project), 1L, 8L)
  paste(basename(project), id, sep = "-")

}

renv_bootstrap_library_root <- function(project) {

  prefix <- renv_bootstrap_profile_prefix()

  path <- Sys.getenv("RENV_PATHS_LIBRARY", unset = NA)
  if (!is.na(path))
    return(paste(c(path, prefix), collapse = "/"))

  path <- renv_bootstrap_library_root_impl(project)
  if (!is.null(path)) {
    name <- renv_bootstrap_library_root_name(project)
    return(paste(c(path, prefix, name), collapse = "/"))
  }

  renv_bootstrap_paths_renv("library", project = project)

}

renv_bootstrap_library_root_impl <- function(project) {

  root <- Sys.getenv("RENV_PATHS_LIBRARY_ROOT", unset = NA)
  if (!is.na(root))
    return(root)

  type <- renv_bootstrap_project_type(project)
  if (identical(type, "package")) {
    userdir <- renv_bootstrap_user_dir()
    return(file.path(userdir, "library"))
  }

}

renv_bootstrap_validate_version <- function(version, description = NULL) {

  # resolve description file
  #
  # avoid passing lib.loc to `packageDescription()` below, since R will
  # use the loaded version of the package by default anyhow. note that
  # this function should only be called after 'renv' is loaded
  # https://github.com/rstudio/renv/issues/1625
  description <- description %||% packageDescription("renv")

  # check whether requested version 'version' matches loaded version of renv
  sha <- attr(version, "sha", exact = TRUE)
  valid <- if (!is.null(sha))
    renv_bootstrap_validate_version_dev(sha, description)
  else
    renv_bootstrap_validate_version_release(version, description)

  if (valid)
    return(TRUE)

  # the loaded version of renv doesn't match the requested version;
  # give the user instructions on how to proceed
  remote <- if (!is.null(description[["RemoteSha"]])) {
    paste("rstudio/renv", description[["RemoteSha"]], sep = "@")
  } else {
    paste("renv", description[["Version"]], sep = "@")
  }

  # display both loaded version + sha if available
  friendly <- renv_bootstrap_version_friendly(
    version = description[["Version"]],
    sha     = description[["RemoteSha"]]
  )

  fmt <- paste(
    "renv %1$s was loaded from project library, but this project is configured to use renv %2$s.",
    "- Use `renv::record(\"%3$s\")` to record renv %1$s in the lockfile.",
    "- Use `renv::restore(packages = \"renv\")` to install renv %2$s into the project library.",
    sep = "\n"
  )
  catf(fmt, friendly, renv_bootstrap_version_friendly(version), remote)

  FALSE

}

renv_bootstrap_validate_version_dev <- function(version, description) {
  expected <- description[["RemoteSha"]]
  is.character(expected) && startswith(expected, version)
}

renv_bootstrap_validate_version_release <- function(version, description) {
  expected <- description[["Version"]]
  is.character(expected) && identical(expected, version)
}

renv_bootstrap_hash_text <- function(text) {

  hashfile <- tempfile("renv-hash-")
  on.exit(unlink(hashfile), add = TRUE)

  writeLines(text, con = hashfile)
  tools::md5sum(hashfile)

}

renv_bootstrap_load <- function(project, libpath, version) {

  # try to load renv from the project library
  if (!requireNamespace("renv", lib.loc = libpath, quietly = TRUE))
    return(FALSE)

  # warn if the version of renv loaded does not match
  renv_bootstrap_validate_version(version)

  # execute renv load hooks, if any
  hooks <- getHook("renv::autoload")
  for (hook in hooks)
    if (is.function(hook))
      tryCatch(hook(), error = warnify)

  # load the project
  renv::load(project)

  TRUE

}

renv_bootstrap_profile_load <- function(project) {

  # if RENV_PROFILE is already set, just use that
  profile <- Sys.getenv("RENV_PROFILE", unset = NA)
  if (!is.na(profile) && nzchar(profile))
    return(profile)

  # check for a profile file (nothing to do if it doesn't exist)
  path <- renv_bootstrap_paths_renv("profile", profile = FALSE, project = project)
  if (!file.exists(path))
    return(NULL)

  # read the profile, and set it if it exists
  contents <- readLines(path, warn = FALSE)
  if (length(contents) == 0L)
    return(NULL)

  # set RENV_PROFILE
  profile <- contents[[1L]]
  if (!profile %in% c("", "default"))
    Sys.setenv(RENV_PROFILE = profile)

  profile

}

renv_bootstrap_profile_prefix <- function() {
  profile <- renv_bootstrap_profile_get()
  if (!is.null(profile))
    return(file.path("profiles", profile, "renv"))
}

renv_bootstrap_profile_get <- function() {
  profile <- Sys.getenv("RENV_PROFILE", unset = "")
  renv_bootstrap_profile_normalize(profile)
}

renv_bootstrap_profile_set <- function(profile) {
  profile <- renv_bootstrap_profile_normalize(profile)
  if (is.null(profile))
    Sys.unsetenv("RENV_PROFILE")
  else
    Sys.setenv(RENV_PROFILE = profile)
}

renv_bootstrap_profile_normalize <- function(profile) {

  if (is.null(profile) || profile %in% c("", "default"))
    return(NULL)

  profile

}

renv_bootstrap_path_absolute <- function(path) {

  substr(path, 1L, 1L) %in% c("~", "/", "\\") || (
    substr(path, 1L, 1L) %in% c(letters, LETTERS) &&
    substr(path, 2L, 3L) %in% c(":/", ":\\")
  )

}

renv_bootstrap_paths_renv <- function(..., profile = TRUE, project = NULL) {
  renv <- Sys.getenv("RENV_PATHS_RENV", unset = "renv")
  root <- if (renv_bootstrap_path_absolute(renv)) NULL else project
  prefix <- if (profile) renv_bootstrap_profile_prefix()
  components <- c(root, renv, prefix, ...)
  paste(components, collapse = "/")
}

renv_bootstrap_project_type <- function(path) {

  descpath <- file.path(path, "DESCRIPTION")
  if (!file.exists(descpath))
    return("unknown")

  desc <- tryCatch(
    read.dcf(descpath, all = TRUE),
    error = identity
  )

  if (inherits(desc, "error"))
    return("unknown")

  type <- desc$Type
  if (!is.null(type))
    return(tolower(type))

  package <- desc$Package
  if (!is.null(package))
    return("package")

  "unknown"

}

renv_bootstrap_user_dir <- function() {
  dir <- renv_bootstrap_user_dir_impl()
  path.expand(chartr("\\", "/", dir))
}

renv_bootstrap_user_dir_impl <- function() {

  # use local override if set
  override <- getOption("renv.userdir.override")
  if (!is.null(override))
    return(override)

  # use R_user_dir if available
  tools <- asNamespace("tools")
  if (is.function(tools$R_user_dir))
    return(tools$R_user_dir("renv", "cache"))

  # try using our own backfill for older versions of R
  envvars <- c("R_USER_CACHE_DIR", "XDG_CACHE_HOME")
  for (envvar in envvars) {
    root <- Sys.getenv(envvar, unset = NA)
    if (!is.na(root))
      return(file.path(root, "R/renv"))
  }

  # use platform-specific default fallbacks
  if (Sys.info()[["sysname"]] == "Windows")
    file.path(Sys.getenv("LOCALAPPDATA"), "R/cache/R/renv")
  else if (Sys.info()[["sysname"]] == "Darwin")
    "~/Library/Caches/org.R-project.R/R/renv"
  else
    "~/.cache/R/renv"

}

renv_bootstrap_version_friendly <- function(version, shafmt = NULL, sha = NULL) {
  sha <- sha %||% attr(version, "sha", exact = TRUE)
  parts <- c(version, sprintf(shafmt %||% " [sha: %s]", substring(sha, 1L, 7L)))
  paste(parts, collapse = "")
}

renv_bootstrap_exec <- function(project, libpath, version) {
  if (!renv_bootstrap_load(project, libpath, version))
    renv_bootstrap_run(version, libpath)
}

renv_bootstrap_run <- function(version, libpath) {

  # perform bootstrap
  bootstrap(version, libpath)

  # exit early if we're just testing bootstrap
  if (!is.na(Sys.getenv("RENV_BOOTSTRAP_INSTALL_ONLY", unset = NA)))
    return(TRUE)

  # try again to load
  if (requireNamespace("renv", lib.loc = libpath, quietly = TRUE)) {
    return(renv::load(project = getwd()))
  }

  # failed to download or load renv; warn the user
  msg <- c(
    "Failed to find an renv installation: the project will not be loaded.",
    "Use `renv::activate()` to re-initialize the project."
  )

  warning(paste(msg, collapse = "\n"), call. = FALSE)

}


renv_bootstrap_in_rstudio <- function() {
  commandArgs()[[1]] == "RStudio"
}

# Used to work around buglet in RStudio if hook uses readline
renv_bootstrap_flush_console <- function() {
  tryCatch({
    tools <- as.environment("tools:rstudio")
    tools$.rs.api.sendToConsole("", echo = FALSE, focus = FALSE)
  }, error = function(cnd) {})
}


# cache.R --------------------------------------------------------------------


# tools for interacting with the renv global package cache
renv_cache_version <- function() {
  # NOTE: users should normally not override the cache version;
  # this is provided just to make testing easier
  Sys.getenv("RENV_CACHE_VERSION", unset = "v5")
}

renv_cache_version_previous <- function() {
  version <- renv_cache_version()
  number <- as.integer(substring(version, 2L))
  paste("v", number - 1L, sep = "")
}

# given a record, find a compatible version of that package in the cache,
# using a computed hash if available; if no hash is available, then try
# to match based on the package name + version
renv_cache_find <- function(record) {

  # validate required fields -- if any are missing, we can't use the cache
  required <- c("Package", "Version")
  missing <- renv_vector_diff(required, names(record))
  if (length(missing))
    return("")

  # if we have a hash, use it directly
  if (!is.null(record$Hash)) {

    # generate path to package installations in cache
    paths <- with(record, renv_paths_cache(Package, Version, Hash, Package))

    # if there are multiple cache entries, return the first existing one
    # if no entries exist, return path into first cache entry
    for (path in paths)
      if (file.exists(path))
        return(path)

    return(paths[[1L]])

  }

  # if the record doesn't have a hash, check to see if we can still locate a
  # compatible package version within the cache
  root <- with(record, renv_paths_cache(Package, Version))
  hashes <- list.files(root, full.names = TRUE)
  packages <- list.files(hashes, full.names = TRUE)

  # iterate over package paths, read DESCRIPTION, and look
  # for something compatible with the requested record
  for (package in packages) {

    # try to read the DESCRIPTION file
    dcf <- catch(as.list(renv_description_read(package)))
    if (inherits(dcf, "error"))
      next

    # if we're requesting an install from an R package repository,
    # and the cached package has a "Repository" field, then use it
    source <- renv_record_source(record)
    hasrepo <-
      source %in% c("cran", "repository") &&
      "Repository" %in% names(dcf)

    if (hasrepo)
      return(package)

    # check for compatible fields
    fields <- unique(c(
      renv_record_names(record, c("Package", "Version")),
      renv_record_names(dcf, c("Package", "Version"))
    ))

    # drop unnamed fields
    record <- record[nzchar(record)]
    dcf <- dcf[nzchar(dcf)]

    # check identical
    lhs <- keep(record, fields)
    rhs <- keep(dcf, fields)
    if (identical(lhs, rhs))
      return(package)

  }

  # failed; return "" as proxy for missing file
  ""

}

# given the path to a package's description file,
# compute the location it would be assigned if it
# were moved to the renv cache
renv_cache_path <- function(path) {
  record <- renv_description_read(path)
  record$Hash <- renv_hash_description(path)
  renv_cache_find(record)
}

renv_cache_path_components <- function(path) {

  data_frame(
    Package = renv_path_component(path, 1L),
    Hash    = renv_path_component(path, 2L),
    Version = renv_path_component(path, 3L)
  )

}

renv_cache_synchronize <- function(record, linkable = FALSE) {

  # construct path to package in library
  library <- renv_libpaths_active()
  path <- file.path(library, record$Package)
  if (!file.exists(path))
    return(FALSE)

  # bail if the package source is unknown
  # (packages with an unknown source are not cacheable)
  desc <- renv_description_read(path)
  source <- renv_snapshot_description_source(desc)
  if (identical(source, list(Source = "unknown")))
    return(FALSE)

  # bail if record not cacheable
  if (!renv_record_cacheable(record))
    return(FALSE)

  # if we don't have a hash, compute it now
  record$Hash <- record$Hash %||% renv_hash_description(path)

  # construct cache entry
  caches <- renv_cache_find(record)

  # try to synchronize
  copied <- FALSE
  for (cache in caches) {
    copied <- renv_cache_synchronize_impl(cache, record, linkable, path)
    if (copied)
      return(TRUE)
  }

  return(FALSE)

}

renv_cache_synchronize_impl <- function(cache, record, linkable, path) {

  # double-check we have a valid cache path
  if (!nzchar(cache))
    return(FALSE)

  # if our cache -> path link is already up to date, then nothing to do
  if (renv_file_same(cache, path))
    return(TRUE)

  # try to create the cache directory target
  # (catch errors due to permissions, etc)
  parent <- dirname(cache)
  status <- catchall(ensure_directory(parent))
  if (inherits(status, "error"))
    return(FALSE)

  # double-check that the cache is writable
  writable <- local({
    file <- renv_scope_tempfile("renv-tempfile-", tmpdir = parent)
    status <- catchall(file.create(file))
    file.exists(file)
  })

  if (!writable)
    return(FALSE)

  # obtain lock on the cache
  lockpath <- file.path(parent, ".cache.lock")
  renv_scope_lock(lockpath)

  # if we already have a cache entry, back it up
  restore <- renv_file_backup(cache)
  defer(restore())

  # copy package from source location into the cache
  if (linkable) {
    renv_cache_move(path, cache, overwrite = TRUE)
    renv_file_link(cache, path, overwrite = TRUE)
  } else {
    renv_cache_copy(path, cache, overwrite = TRUE)
  }

  if (renv_platform_unix()) {

    # change the cache owner if set
    user <- Sys.getenv("RENV_CACHE_USER", unset = NA)
    if (!is.na(user)) {
      parent <- dirname(dirname(dirname(cache)))
      renv_system_exec(
        command = "chown",
        args    = c("-Rf", renv_shell_quote(user), renv_shell_path(parent)),
        action  = "chowning cached package",
        quiet   = TRUE,
        success = NULL
      )
    }

    # change file modes after copy if set
    mode <- Sys.getenv("RENV_CACHE_MODE", unset = NA)
    if (!is.na(mode)) {
      parent <- dirname(dirname(dirname(cache)))
      renv_system_exec(
        command = "chmod",
        args    = c("-Rf", renv_shell_quote(mode), renv_shell_path(parent)),
        action  = "chmoding cached package",
        quiet   = TRUE,
        success = NULL
      )
    }

    # finally, allow for an arbitrary callback if set
    callback <- getOption("renv.cache.callback")
    if (is.function(callback))
      callback(cache)

  }

  TRUE

}

renv_cache_list <- function(cache = NULL, packages = NULL) {
  caches <- cache %||% renv_paths_cache()
  paths <- map(caches, renv_cache_list_impl, packages = packages)
  unlist(paths, recursive = TRUE, use.names = FALSE)
}

renv_cache_list_impl <- function(cache, packages) {

  # paths to packages in the cache have the following format:
  #
  #    <package>/<version>/<hash>/<package>
  #
  # so find entries in the cache by listing files in each directory
  names <- file.path(cache, packages %||% list.files(cache))
  versions <- list.files(names, full.names = TRUE)
  hashes <- list.files(versions, full.names = TRUE)
  paths <- list.files(hashes, full.names = TRUE)

  # only keep paths that appear to be valid
  valid <- grep(renv_regexps_package_name(), basename(paths))
  paths[valid]

}

renv_cache_problems <- function(paths, reason) {

  data_frame(
    Package = renv_path_component(paths, 1L),
    Version = renv_path_component(paths, 3L),
    Path    = paths,
    Reason  = reason
  )

}

renv_cache_diagnose_corrupt_metadata <- function(paths, problems, verbose) {

  # check for missing metadata files
  metapaths <- file.path(paths, "Meta/package.rds")
  ok <- file.exists(metapaths)
  bad <- paths[!ok]

  if (length(bad)) {

    # nocov start
    if (verbose) {
      caution_bullets(
        "The following package(s) are missing 'Meta/package.rds':",
        renv_cache_format_path(bad),
        "These packages should be purged and reinstalled."
      )
    }
    # nocov end

    data <- renv_cache_problems(
      paths  = bad,
      reason = "'Meta/package.rds' does not exist"
    )

    problems$push(data)

  }

  # check for corrupt / unreadable metadata files
  ok <- map_lgl(metapaths, function(path) {
    rds <- catch(readRDS(path))
    !inherits(rds, "error")
  })

  bad <- paths[!ok]

  if (length(bad)) {

    # nocov start
    if (verbose) {
      caution_bullets(
        "The following package(s) have corrupt 'Meta/package.rds' files:",
        renv_cache_format_path(bad),
        "These packages should be purged and reinstalled."
      )
    }
    # nocov end

    data <- renv_cache_problems(
      paths  = bad,
      reason = "'Meta/package.rds' does not exist"
    )

    problems$push(data)

  }

  paths

}

renv_cache_diagnose_missing_descriptions <- function(paths, problems, verbose) {

  descpaths <- file.path(paths, "DESCRIPTION")
  exists <- file.exists(descpaths)
  bad <- paths[!exists]
  if (empty(bad))
    return(paths)

  # nocov start
  if (verbose) {
    caution_bullets(
      "The following packages are missing DESCRIPTION files in the cache:",
      renv_cache_format_path(bad),
      "These packages should be purged and reinstalled."
    )
  }
  # nocov end

  data <- renv_cache_problems(
    paths  = bad,
    reason = "'DESCRIPTION' file does not exist"
  )

  problems$push(data)
  paths[exists]

}

renv_cache_diagnose_bad_hash <- function(paths, problems, verbose) {

  expected <- map_chr(paths, renv_cache_path)
  wrong <- paths != expected & !file.exists(expected)
  if (!any(wrong))
    return(paths)

  # nocov start
  if (verbose) {

    lhs <- renv_cache_path_components(paths[wrong])
    rhs <- renv_cache_path_components(expected[wrong])

    fmt <- "%s %s [Hash: %s != %s]"
    entries <- sprintf(fmt, lhs$Package, lhs$Version, lhs$Hash, rhs$Hash)

    caution_bullets(
      "The following packages have incorrect hashes:",
      entries,
      "Consider using `renv::rehash()` to re-hash these packages."
    )
  }
  # nocov end

  data <- renv_cache_problems(
    paths  = paths[wrong],
    reason = "unexpected hash"
  )

  problems$push(data)
  paths

}

renv_cache_diagnose_wrong_built_version <- function(paths, problems, verbose) {

  # form paths to DESCRIPTION files
  descpaths <- file.path(paths, "DESCRIPTION")

  # parse the version of R each was built for
  versions <- map_chr(descpaths, function(descpath) {

    tryCatch(
      renv_description_built_version(descpath),
      error = function(e) {
        warning(e)
        NA
      }
    )

  })

  # check for NAs, report and remove them
  isna <- is.na(versions)
  if (any(isna)) {

    # nocov start
    if (verbose) {

      caution_bullets(
        "The following packages have no 'Built' field recorded in their DESCRIPTION file:",
        paths[isna],
        "renv is unable to validate the version of R this package was built for."
      )

    }
    # nocov end

    data <- renv_cache_problems(
      paths = paths[isna],
      reason = "missing Built field"
    )

    problems$push(data)

    paths    <- paths[!isna]
    versions <- versions[!isna]

  }

  # check for incompatible versions
  wrong <- map_lgl(versions, function(version) {
    tryCatch(
      renv_version_compare(version, getRversion(), 2L) != 0,
      error = function(e) {
        warning(e)
        TRUE
      }
    )
  })

  if (!any(wrong))
    return(paths)

  # nocov start
  if (verbose) {

    caution_bullets(
      "The following packages in the cache were built for a different version of R:",
      renv_cache_format_path(paths[wrong]),
      "These packages will need to be purged and reinstalled."
    )

  }
  # nocov end

  data <- renv_cache_problems(
    paths = paths[wrong],
    reason = "built for different version of R"
  )

  problems$push(data)
  paths

}

renv_cache_diagnose <- function(verbose = NULL) {

  verbose <- verbose %||% renv_verbose()

  problems <- stack()
  paths <- renv_cache_list()
  paths <- renv_cache_diagnose_corrupt_metadata(paths, problems, verbose)
  paths <- renv_cache_diagnose_missing_descriptions(paths, problems, verbose)
  paths <- renv_cache_diagnose_bad_hash(paths, problems, verbose)
  paths <- renv_cache_diagnose_wrong_built_version(paths, problems, verbose)

  invisible(bind(problems$data()))

}

renv_cache_acls_reset <- function(target) {

  enabled <- Sys.getenv("RENV_CACHE_ACLS", unset = "TRUE")
  if (enabled)
    renv_acls_reset(target)

}

# copies a package at location 'source' to cache location 'target'
renv_cache_copy <- function(source, target, overwrite = FALSE) {
  ensure_parent_directory(target)
  renv_file_copy(source, target, overwrite = overwrite)
  renv_cache_acls_reset(target)
}

# moves a package from location 'source' to cache location 'target',
# and then links back from 'target' to 'source'
renv_cache_move <- function(source, target, overwrite = FALSE) {

  # move package into the cache if requested
  if (overwrite || !file.exists(target)) {
    ensure_parent_directory(target)
    renv_file_move(source, target, overwrite = TRUE)
  }

  # try to reset ACLs on the cache directory
  renv_cache_acls_reset(target)

  # link from the cache back to the target location
  renv_file_link(target, source, overwrite = TRUE)

}

# nocov start
renv_cache_format_path <- function(paths) {

  # extract path components
  names    <- format(renv_path_component(paths, 1L))
  hashes   <- format(renv_path_component(paths, 2L))
  versions <- format(renv_path_component(paths, 3L))

  # format and write
  fmt <- "%s %s [Hash: %s]"
  sprintf(fmt, names, versions, hashes)

}
# nocov end

renv_cache_clean_empty <- function(cache = NULL) {

  # no-op for Solaris
  if (renv_platform_solaris())
    return(FALSE)

  # move to cache root
  caches <- cache %||% renv_paths_cache()
  for (cache in caches)
    renv_cache_clean_empty_impl(cache)

  TRUE

}

renv_cache_clean_empty_impl <- function(cache) {

  # move to cache directory
  renv_scope_wd(cache)

  # construct system command for removing empty directories
  action <- "removing empty directories"
  if (renv_platform_windows()) {
    args <- c(".", ".", "/S", "/MOVE")
    renv_system_exec("robocopy", args, action, 0:8)
  } else {
    args <- c(".", "-type", "d", "-empty", "-delete")
    renv_system_exec("find", args, action)
  }

  TRUE

}

renv_cache_package_validate <- function(path) {

  if (renv_project_type(path) == "package")
    return(TRUE)

  type <- renv_file_type(path, symlinks = FALSE)
  if (!nzchar(type))
    return(FALSE)

  name <- if (type == "directory") "directory" else "file"
  fmt <- "%s %s exists but does not appear to be an R package"
  warningf(fmt, name, shQuote(path))

  FALSE

}

renv_cache_config_enabled <- function(project) {
  config$cache.enabled() && settings$use.cache(project = project)
}

renv_cache_config_symlinks <- function(project) {

  usesymlinks <-
    config$cache.symlinks(default = NULL) %||%
    renv_cache_config_symlinks_default(project = project)

  usesymlinks && settings$use.cache(project = project)

}

renv_cache_config_symlinks_default <- function(project) {

  # on linux, we can always use symlinks
  if (renv_platform_unix())
    return(TRUE)

  # on Windows, only try to use symlinks (junction points) if the cache
  # and the project library appear to live on the same drive
  libpath <- renv_paths_library(project = project)
  cachepath <- renv_paths_cache()

  # TODO: with this change, anyone using networks not mapped to a local drive
  # would need to opt-in to using symlinks, but that's probably okay?
  all(
    substring(libpath, 1L, 2L) == substring(cachepath, 1L, 2L),
    substring(libpath, 2L, 2L) == ":",
    substring(cachepath, 2L, 2L) == ":"
  )


}

renv_cache_linkable <- function(project, library) {
  renv_cache_config_enabled(project = project) &&
    renv_cache_config_symlinks(project = project) &&
    getOption(
      "renv.cache.linkable",
      renv_path_same(library, renv_paths_library(project = project))
    )
}


# call.R ---------------------------------------------------------------------


# given a call of the form e.g. 'pkg::foo()' or 'foo()',
# check that method 'foo()' is truly being called and
# strip off the 'pkg::' part for easier parsing
renv_call_expect <- function(node, package, methods) {

  if (!is.call(node))
    return(NULL)

  # check for call of the form 'pkg::foo(a, b, c)'
  colon <- renv_call_matches(node[[1L]], name = c("::", ":::"), n_args = 2)

  if (colon) {

    # validate the package name
    lhs <- node[[1L]][[2L]]
    if (as.character(lhs) != package)
      return(NULL)

    # extract the inner call
    rhs <- node[[1L]][[3L]]
    node[[1L]] <- rhs
  }

  # check for method match
  match <-
    is.name(node[[1L]]) &&
    as.character(node[[1L]]) %in% methods

  if (!match)
    return(NULL)

  node

}

renv_call_normalize <- function(node, stack) {

  # check for magrittr pipe -- if this part of the expression is
  # being piped into, then we need to munge the call
  ispipe <- renv_call_matches(node, name = c("%>%", "%T>%", "%<>%"))

  if (!ispipe)
    return(node)

  # get lhs and rhs of piped expression
  lhs <- node[[2L]]
  rhs <- node[[3L]]

  # handle rhs symbols
  if (is.symbol(rhs))
    rhs <- call(as.character(rhs))

  # check for usage of '.'
  # if it exists, replace each with lhs
  hasdot <- FALSE
  dot <- as.symbol(".")
  for (i in seq_along(rhs)) {
    if (identical(dot, rhs[[i]])) {
      hasdot <- TRUE
      rhs[[i]] <- lhs
    }
  }

  if (hasdot)
    return(rhs)

  # otherwise, mutate rhs call with lhs passed as first argument
  args <- as.list(rhs)
  as.call(c(args[[1L]], lhs, args[-1L]))

}


renv_call_matches <- function(call, name = NULL, n_args = NULL) {
  if (!is.call(call))
    return(FALSE)

  if (!is.null(name)) {
    if (!is.name(call[[1]]))
      return(FALSE)

    if (!as.character(call[[1]]) %in% name)
      return(FALSE)
  }

  if (!is.null(n_args) && length(call) != n_args + 1L)
    return(FALSE)

  TRUE
}


# caution.R ------------------------------------------------------------------


caution <- function(fmt = "", ..., con = stdout()) {
  enabled <- getOption("renv.caution.verbose", default = TRUE)
  if (!is.null(fmt) && enabled)
    writeLines(sprintf(fmt, ...), con = con)
}

caution_bullets <- function(preamble = NULL,
                            values = NULL,
                            postamble = NULL,
                            ...,
                            bullets = TRUE,
                            emitter = NULL)
{
  if (empty(values))
    return(invisible())

  renv_dots_check(...)

  lines <- c(
    if (length(preamble))  paste(preamble, collapse = "\n"),
    if (bullets)
      paste("-", values, collapse = "\n")
    else
      paste(values, collapse = "\n"),
    if (length(postamble)) paste(postamble, collapse = "\n"),
    ""
  )

  text <- paste(lines, collapse = "\n")
  renv_caution_impl(text, emitter)
}

renv_caution_impl <- function(text, emitter = NULL) {

  # NOTE: Used by vetiver, so perhaps is part of the API.
  # We should think of a cleaner way of exposing this.
  # https://github.com/rstudio/renv/issues/1413
  emitter <- emitter %||% {
    getOption("renv.pretty.print.emitter", default = caution)
  }

  emitter(text)
  invisible(NULL)

}


# cellar.R -------------------------------------------------------------------


renv_cellar_roots <- function(project = NULL) {
  c(
    renv_paths_renv("cellar", project = project),
    renv_paths_renv("local", project = project),
    renv_paths_cellar(),
    renv_paths_local()
  )
}

renv_cellar_database <- function(project = NULL) {

  # find cellar root directories
  project <- renv_project_resolve(project)
  roots <- renv_cellar_roots(project)

  # list files both at top-level + one nested level
  paths <- list.files(roots, full.names = TRUE)
  paths <- c(paths, list.files(paths, full.names = TRUE))

  # grab files that look like packages
  extpat <- "(?:\\.tar\\.gz|\\.tgz|\\.zip)$"
  paths <- grep(extpat, paths, value = TRUE)

  # parse into data.frame
  base <- basename(paths)
  parts <- strsplit(base, "_", fixed = TRUE)
  package <- map_chr(parts, `[[`, 1L)
  rest <- map_chr(parts, `[[`, 2L)
  version <- sub(extpat, "", rest)

  data_frame(
    Package = package,
    Version = version,
    Path    = paths
  )

}

renv_cellar_latest <- function(package, project) {

  db <- renv_cellar_database(project = project)
  db <- rows(db, db$Package == package)
  db <- rows(db, order(package_version(db$Version), decreasing = TRUE))
  if (nrow(db) == 0L)
    return(record)

  entry <- db[1, ]
  list(
    Package = entry$Package,
    Version = entry$Version,
    Source  = "Cellar"
  )

}


# check.R --------------------------------------------------------------------


renv_check_unknown_source <- function(records, project = NULL) {

  # nothing to do if we have no records
  if (empty(records))
    return(TRUE)

  # for testing, we ignore renv
  if (renv_tests_running())
    records$renv <- NULL

  # keep only records which have unknown source
  unknown <- filter(records, function(record) {

    source <- renv_record_source(record)
    if (source != "unknown")
      return(FALSE)

    localpath <- tryCatch(
      renv_retrieve_cellar_find(record, project),
      error = function(e) ""
    )

    if (file.exists(localpath))
      return(FALSE)

    TRUE

  })

  # if all records have a known source, return TRUE
  if (empty(unknown))
    return(TRUE)

  # provide warning
  if (!renv_tests_running())
    renv_warnings_unknown_sources(unknown)

  # return FALSE to indicate failed validation
  FALSE

}




# checkout.R -----------------------------------------------------------------


#' Checkout a repository
#'
#' `renv::checkout()` can be used to retrieve the latest-availabe packages from
#' a (set of) package repositories.
#'
#' `renv::checkout()` is most useful with services like the Posit's
#' [Package Manager](https://packagemanager.rstudio.com/), as it
#' can be used to switch between different repository snapshots within an
#' renv project. In this way, you can upgrade (or downgrade) all of the
#' packages used in a particular renv project to the package versions
#' provided by a particular snapshot.
#'
#' If your library contains packages installed from other remote sources (e.g.
#' GitHub), but a version of a package of the same name is provided by the
#' repositories being checked out, then please be aware that the package will be
#' replaced with the version provided by the requested repositories. This could
#' be a concern if your project uses \R packages from GitHub whose name matches
#' that of an existing CRAN package, but is otherwise unrelated to the package
#' on CRAN.
#'
#' @inheritParams renv-params
#'
#' @param repos The \R package repositories to use.
#'
#' @param packages The packages to be installed. When `NULL` (the default),
#'   all packages currently used in the project will be installed, as
#'   determined by [renv::dependencies()]. The recursive dependencies of these
#'   packages will be included as well.
#'
#' @param date The snapshot date to use. When set, the associated snapshot as
#'   available from the Posit's public
#'   [Package Manager](https://packagemanager.rstudio.com/) instance will be
#'   used. Ignored if `repos` is non-`NULL`.
#'
#' @param actions The action(s) to perform with the requested repositories.
#'   This can either be "snapshot", in which `renv` will generate a lockfile
#'   based on the latest versions of the packages available from `repos`, or
#'   "restore" if you'd like to install those packages. You can use
#'   `c("snapshot", "restore")` if you'd like to generate a lockfile and
#'   install those packages in the same step.
#'
#' @examples
#' \dontrun{
#'
#' # check out packages from PPM using the date '2023-01-02'
#' renv::checkout(date = "2023-01-02")
#'
#' # alternatively, supply the full repository path
#' renv::checkout(repos = "https://packagemanager.rstudio.com/cran/2023-01-02")
#'
#' # only check out some subset of packages (and their recursive dependencies)
#' renv::checkout(packages = "dplyr", date = "2023-01-02")
#'
#' }
#' @export
checkout <- function(repos = NULL,
                     ...,
                     packages = NULL,
                     date     = NULL,
                     clean    = FALSE,
                     actions  = "restore",
                     project  = NULL)
{
  renv_consent_check()
  renv_scope_error_handler()
  renv_dots_check(...)

  project  <- renv_project_resolve(project)
  renv_project_lock(project = project)

  # set new repositories
  repos <- repos %||% renv_checkout_repos(date)
  options(repos = repos)

  # TODO: Activate Bioconductor if it appears to be used by this project

  # select packages to install
  packages <- packages %||% renv_checkout_packages(project = project)

  # get the associated remotes for these packages
  remotes <- renv_checkout_remotes(packages, project)

  # parse these into package records
  records <- map(remotes, renv_remotes_resolve)

  # create a lockfile matching this request
  lockfile <- renv_lockfile_init(project)
  lockfile$Packages <- records

  # perform requested actions
  for (action in actions) {
    case(
      action == "snapshot" ~ renv_lockfile_write(lockfile, file = renv_lockfile_path(project)),
      action == "restore"  ~ restore(lockfile = lockfile, clean = clean),
      ~ stopf("unrecognized action '%s'")
    )
  }

  invisible(lockfile)

}

renv_checkout_packages <- function(project) {
  renv_dependencies_impl(
    project,
    field = "Package",
    dev = TRUE
  )
}

renv_checkout_remotes <- function(packages, project) {

  # get available packages
  dbs <- available_packages(type = "source")
  if (is.null(dbs))
    stop("no package repositories are available")

  # flatten so we only see the latest version of a package
  db <- renv_available_packages_flatten(dbs)

  # keep only packages which appear to be available in the repositories
  packages <- intersect(packages, db$Package)

  # remove ignored packages -- note we intentionally do this before
  # computing recursive dependencies as we don't want to allow users
  # to ignore a recursive dependency of a required package
  ignored <- c("renv", renv_project_ignored_packages(project))
  packages <- setdiff(packages, ignored)

  # compute recursive dependencies for these packages
  renv_checkout_recdeps(packages, db)

}

renv_checkout_recdeps <- function(packages, db) {

  # initialize environment (will map package names to discovered remotes)
  envir <- new.env(parent = emptyenv())

  # set R to NA since it's a common non-package 'dependency' for packages
  envir$R <- NA

  # iterate through dependencies
  for (package in packages)
    renv_checkout_recdeps_impl(package, db, envir)

  # get list of discovered dependencies
  recdeps <- as.list.environment(envir, all.names = TRUE)

  # drop any NA values
  recdeps <- filter(recdeps, Negate(is.na))

  # return sorted vector
  recdeps[csort(names(recdeps))]

}

renv_checkout_recdeps_impl <- function(package, db, envir) {

  # check if we've already visited this package
  if (!is.null(envir[[package]]))
    return()

  # get entry from database
  entry <- rows(db, db$Package == package)
  if (nrow(entry) == 0L) {
    envir[[package]] <- NA_character_
    return()
  }

  # set discovered remote
  envir[[package]] <- with(entry, paste(Package, Version, sep = "@"))

  # iterate through hard dependencies
  fields <- c("Depends", "Imports", "LinkingTo")
  for (field in fields) {
    value <- entry[[field]]
    if (!is.null(value) && !is.na(value)) {
      value <- renv_description_parse_field(entry[[field]])
      for (package in value$Package)
        if (is.null(envir[[package]]))
          renv_checkout_recdeps_impl(package, db, envir)
    }
  }

  # for soft dependencies, only include those if they're currently installed
  # TODO: or check if it's in the lockfile?
  value <- entry[["Suggests"]]
  if (!is.null(value) && !is.na(value)) {
    value <- renv_description_parse_field(value)
    for (package in value$Package)
      if (is.null(envir[[package]]))
        if (renv_package_installed(package))
          renv_checkout_recdeps_impl(package, db, envir)
  }

}

renv_checkout_repos <- function(date) {

  # if no date was provided, just use default repositories
  if (is.null(date))
    return(getOption("repos"))

  # build path to repository snapshot location
  root <- dirname(config$ppm.url())
  url <- file.path(root, date)
  if (renv_download_available(file.path(url, "src/contrib/PACKAGES")))
    return(c(PPM = url))

  # requested date not available; try to search a bit
  candidate <- date
  for (i in 1:7) {
    candidate <- format(as.Date(candidate) - 1L)
    url <- file.path(root, candidate)
    if (renv_download_available(file.path(url, "src/contrib/PACKAGES"))) {
      fmt <- "- Snapshot date '%s' not available; using '%s' instead"
      printf(fmt, date, candidate)
      return(c(PPM = url))
    }
  }

  stopf("repository snapshot '%s' not available", date)

}


# clean.R --------------------------------------------------------------------


#' Clean a project
#'
#' Clean up a project and its associated \R libraries.
#'
#' # Actions
#'
#' The following clean actions are available:
#'
#' \describe{
#'
#' \item{`package.locks`}{
#'
#'   During package installation, \R will create package locks in the
#'   library path, typically named `00LOCK-<package>`. On occasion, if package
#'   installation fails or \R is terminated while installing a package, these
#'   locks can be left behind and will inhibit future attempts to reinstall
#'   that package. Use this action to remove such left-over package locks.
#'
#' }
#'
#' \item{`library.tempdirs`}{
#'
#'   During package installation, \R may create temporary directories with
#'   names of the form `file\w{12}`, and on occasion those files can be
#'   left behind even after they are no longer in use. Use this action to
#'   remove such left-over directories.
#' }
#'
#' \item{`system.library`}{
#'
#'   In general, it is recommended that only packages distributed with \R
#'   are installed into the default library (the library path referred to
#'   by `.Library`). Use this action to remove any user-installed packages
#'   that have been installed to the system library.
#'
#'   Because this action is destructive, it is by default never run -- it
#'   must be explicitly requested by the user.
#'
#' }
#'
#' \item{`unused.packages`}{
#'
#'   Remove packages that are installed in the project library, but no longer
#'   appear to be used in the project sources.
#'
#'   Because this action is destructive, it is by default only run in
#'   interactive sessions when prompting is enabled.
#'
#' }
#'
#' }
#'
#'
#' @inherit renv-params
#'
#' @param actions The set of clean actions to take. See the documentation in
#'   **Actions** for a list of available actions, and the default actions
#'   taken when no actions are supplied.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # clean the current project
#' renv::clean()
#'
#' }
clean <- function(project = NULL,
                  ...,
                  actions = NULL,
                  prompt  = interactive())
{
  renv_scope_error_handler()
  renv_dots_check(...)

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)
  renv_scope_verbose_if(prompt)

  renv_activate_prompt("clean", NULL, prompt, project)

  actions <- actions %||% renv_clean_actions(prompt)

  all <- list(
    package.locks     = renv_clean_package_locks,
    library.tempdirs  = renv_clean_library_tempdirs,
    system.library    = renv_clean_system_library,
    unused.packages   = renv_clean_unused_packages
  )

  methods <- all[actions]
  for (method in methods)
    tryCatch(method(project, prompt), error = warnify)

  writef("- The project has been cleaned.")
  invisible(project)
}

renv_clean_actions <- function(prompt) {

  default <- c(
    "package.locks",
    "library.tempdirs"
  )

  unsafe <- c(
    # "system.library",
    "unused.packages"
  )

  c(default, if (prompt) unsafe)

}

renv_clean_library_tempdirs <- function(project, prompt) {

  ntd <- function() {
    writef("- No temporary directories were found in the project library.")
    FALSE
  }

  library <- renv_paths_library(project = project)
  children <- list.files(library, full.names = TRUE)

  bad <- grep("/file\\w{12}$", children, value = TRUE)
  if (empty(bad))
    return(ntd())

  # nocov start
  if (prompt || renv_verbose()) {

    caution_bullets("The following directories will be removed:", bad)

    if (prompt && !proceed())
      cancel()

  }
  # nocov end

  unlink(bad, recursive = TRUE)
  TRUE

}


# remove user packages in system library
renv_clean_system_library <- function(project, prompt) {

  ntd <- function() {
    writef("- No non-system packages were discovered in the system library.")
    FALSE
  }

  # explicitly query for packages
  syslib <- renv_path_normalize(renv_libpaths_system())
  db <- installed_packages(lib.loc = syslib, priority = "NA")
  packages <- setdiff(db$Package, "translations")

  # also look for leftover package folders
  # (primarily for Windows, where .dlls from old packages can be left behind)

  # nocov start
  if (renv_platform_windows()) {
    folders <- list.files(syslib, full.names = TRUE)
    descpaths <- file.path(folders, "DESCRIPTION")
    missing <- !file.exists(descpaths)
    packages <- union(packages, basename(folders)[missing])
  }
  # nocov end

  # check for any packages needing removal
  if (empty(packages))
    return(ntd())

  # nocov start
  if (prompt || renv_verbose()) {

    caution_bullets(
      "The following non-system packages are installed in the system library:",
      packages,
      c(
        "Normally, only packages distributed with R should be installed in the system library.",
        "These packages will be removed.",
        "If necessary, consider reinstalling these packages in your site library."
      )
    )

    if (prompt && !proceed())
      cancel()

  }
  # nocov end

  remove(packages, library = syslib)
  TRUE

}

renv_clean_unused_packages <- function(project, prompt) {

  ntd <- function() {
    writef("- No unused packages were found in the project library.")
    FALSE
  }

  # find packages installed in the project library
  library <- renv_paths_library(project = project)
  installed <- list.files(library)
  if (empty(installed))
    return(ntd())

  # find packages used in the project and their recursive dependencies
  packages <- renv_snapshot_dependencies(project, dev = TRUE)
  paths <- renv_package_dependencies(packages, project = project)
  packages <- names(paths)

  # figure out which packages aren't needed
  removable <- renv_vector_diff(installed, packages)
  if (empty(removable))
    return(ntd())

  # nocov start
  if (prompt || renv_verbose()) {

    caution_bullets(
      c(
        "The following packages are installed in the project library,",
        "but appear to be no longer used in your project."
      ),
      removable,
      "These packages will be removed."
    )

    if (prompt && !proceed())
      cancel()

  }
  # nocov end

  remove(removable, library = library)
  return(TRUE)

}

renv_clean_package_locks <- function(project, prompt) {

  ntd <- function() {
    writef("- No stale package locks were found.")
    FALSE
  }

  # find 00LOCK directories in library
  library <- renv_paths_library(project = project)
  lock <- list.files(path = library, pattern = "^00LOCK", full.names = TRUE)
  if (empty(lock))
    return(ntd())

  # check to see which are old
  now <- Sys.time()
  mtime <- file.mtime(lock)
  mtime[is.na(mtime)] <- now
  diff <- difftime(now, mtime, units = "secs")
  old <- lock[diff > 120]
  if (empty(old))
    return(ntd())

  # nocov start
  if (prompt || renv_verbose()) {

    caution_bullets(
      "The following stale package locks were discovered in your library:",
      basename(old),
      "These locks will be removed."
    )

    if (prompt && !proceed())
      cancel()

  }
  # nocov end

  unlink(old, recursive = TRUE)
  TRUE
}

# nocov start
renv_clean_cache <- function(project, prompt) {

  ntd <- function() {
    writef("- No unused packages were found in the renv cache.")
    FALSE
  }

  # find projects monitored by renv
  projects <- renv_paths_root("projects")
  projlist <- character()
  if (file.exists(projects))
    projlist <- readLines(projects, warn = FALSE, encoding = "UTF-8")

  # inform user if any projects are missing
  missing <- !file.exists(projlist)
  if (any(missing)) {

    caution_bullets(
      "The following projects are monitored by renv, but no longer exist:",
      projlist[missing],
      "These projects will be removed from renv's project list."
    )

    if (prompt && !proceed())
      cancel()

    writeLines(projlist[!missing], con = projects, useBytes = TRUE)

  }

  action <- function(project) {
    library <- renv_paths_library(project = project)
    packages <- list.files(library, full.names = TRUE)
    descs <- file.path(packages, "DESCRIPTION")
    existing <- file.exists(descs)
    map_chr(descs[existing], renv_cache_path, USE.NAMES = FALSE)
  }

  # for each project, find packages used in their renv private library,
  # and look for entries in the cache
  projlist <- projlist[!missing]
  callback <- renv_progress_callback(action, length(projlist))
  used <- uapply(projlist, callback)

  # check what packages are actually available in the cache
  available <- renv_cache_list()

  diff <- renv_vector_diff(available, used)
  if (empty(diff))
    return(ntd())

  if (prompt || renv_verbose()) {

    caution_bullets(
      "The following packages are installed in the cache but no longer used:",
      renv_cache_format_path(diff),
      "These packages will be removed."
    )

    if (prompt && !proceed())
      cancel()

  }

  # remove the directories
  unlink(diff, recursive = TRUE)
  renv_cache_clean_empty()
  writef("- %i package(s) have been removed.", length(diff))
  TRUE

}
# nocov end


# cli.R ----------------------------------------------------------------------


renv_cli_install <- function(target = NULL) {

  # get path to bundled tool
  exe <- if (renv_platform_windows()) "bin/renv.bat" else "bin/renv"
  path <- system.file(exe, package = "renv")

  # copy into directory on PATH
  target <- target %||% path.expand("~/bin/renv")
  ensure_parent_directory(target)
  file.copy(path, target)

  writef("- renv binary copied to %s.", renv_path_pretty(target))
  invisible(target)

}

renv_cli_exec <- function(clargs = commandArgs(trailingOnly = TRUE)) {
  invisible(renv_cli_exec_impl(clargs))
}

renv_cli_exec_impl <- function(clargs) {

  # check for tool called without arguments, or called with '--help'
  usage <-
    length(clargs) == 0 ||
    clargs[1L] %in% c("help", "--help")

  if (usage)
    return(renv_cli_usage())

  # extract method
  method <- clargs[1L]

  # check request for help on requested method
  help <-
    clargs[2L] %in% c("help", "--help")

  if (help)
    return(renv_cli_help(method))

  # check for known function in renv
  exports <- getNamespaceExports("renv")
  if (!method %in% exports)
    return(renv_cli_unknown(method, exports))

  # begin building call
  args <- list(call("::", as.name("renv"), as.name(method)))

  for (clarg in clargs[-1L]) {

    # convert '--no-<flag>' into a FALSE parameter
    if (grepl("^--no-", clarg)) {
      key <- substring(clarg, 6L)
      args[[key]] <- FALSE
    }

    # convert '--param=value' flags
    else if (grepl("^--[^=]+=", clarg)) {
      index <- regexpr("=", clarg, fixed = TRUE)
      key <- substring(clarg, 3L, index - 1L)
      val <- substring(clarg, index + 1L)
      args[[key]] <- renv_cli_parse(val)
    }

    # convert '--flag' into a TRUE parameter
    else if (grepl("^--", clarg)) {
      key <- substring(clarg, 3L)
      args[[key]] <- TRUE
    }

    # convert 'param=value' flags
    else if (grepl("=", clarg, fixed = TRUE)) {
      index <- regexpr("=", clarg, fixed = TRUE)
      key <- substring(clarg, 1L, index - 1L)
      val <- substring(clarg, index + 1L)
      args[[key]] <- renv_cli_parse(val)
    }

    # take other parameters as-is
    else {
      args[[length(args) + 1L]] <- renv_cli_parse(clarg)
    }

  }

  # invoke method with parsed arguments
  expr <- as.call(args)
  eval(expr = expr, envir = globalenv())

}

renv_cli_usage <- function() {

  usage <- "
Usage: renv [method] [args...]

[method] should be the name of a function exported from renv.
[args...] should be arguments accepted by that function.

Use renv [method] --help for more information about the associated function.

Examples:

  # basic commands
  renv init      # initialize a project
  renv snapshot  # snapshot project library
  renv restore   # restore project library
  renv status    # check project status

  # install a package
  renv install dplyr

  # run a script in an renv project
  renv run path/to/script.R
"

  writeLines(usage, con = stderr())

}

renv_cli_help <- function(method) {
  print(help(method, package = "renv"))
}

renv_cli_unknown <- function(method, exports) {

  # report unknown command
  caution("renv: '%s' is not a known command.", method)

  # check for similar commands
  distance <- c(adist(method, exports))
  names(distance) <- exports
  n <- min(distance)
  if (n > 2)
    return(1L)

  candidates <- names(distance)[distance == n]
  fmt <- "did you mean %s?"
  caution(fmt, paste(shQuote(candidates), collapse = " or "))
  return(1L)

}

renv_cli_parse <- function(text) {

  # handle logical-like values up-front
  if (text %in% c("true", "True", "TRUE"))
    return(TRUE)
  else if (text %in% c("false", "False", "FALSE"))
    return(FALSE)

  # parse the expression
  value <- parse(text = text)[[1L]]
  if (is.language(value)) text else value

}


# conda.R --------------------------------------------------------------------


# given the path to a Python installation managed by conda, attempt to
# find the conda installation + executable used to create it
renv_conda_find <- function(python) {

  tryCatch(
    renv_conda_find_impl(python),
    error = function(e) {
      warning(e)
      ""
    }
  )

}

renv_conda_find_impl <- function(python) {

  # read the conda environment's history to try to find conda
  base <- dirname(python)
  if (!renv_platform_windows())
    base <- dirname(base)

  history <- file.path(base, "conda-meta/history")
  if (!file.exists(history))
    return("")

  contents <- readLines(history, n = 2L, warn = FALSE)
  if (length(contents) < 2)
    return("")

  line <- substring(contents[2L], 8L)
  index <- regexpr(" ", line, fixed = TRUE)
  if (index == -1L)
    return("")

  conda <- substring(line, 1L, index - 1L)
  if (renv_platform_windows())
    conda <- file.path(dirname(conda), "conda.exe")

  # prefer condabin if it exists
  condabin <- file.path(dirname(conda), "../condabin", basename(conda))
  if (file.exists(condabin))
    conda <- condabin

  # bail if conda wasn't found
  if (!file.exists(conda))
    return("")

  renv_path_canonicalize(conda)

}


# condition.R ----------------------------------------------------------------


renv_condition_signal <- function(class = NULL, data = NULL) {
  condition <- list(message = character(), call = NULL, data = data)
  class(condition) <- c(class, "renv.condition", "condition")
  signalCondition(condition)
}


# config-defaults.R ----------------------------------------------------------


# Auto-generated by renv_zzz_bootstrap_config()

#' @rdname config
#' @export
#' @format NULL
config <- list(

  activate.prompt = function(..., default = TRUE) {
    renv_config_get(
      name    = "activate.prompt",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  autoloader.enabled = function(..., default = TRUE) {
    renv_config_get(
      name    = "autoloader.enabled",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  auto.snapshot = function(..., default = FALSE) {
    renv_config_get(
      name    = "auto.snapshot",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  bitbucket.host = function(..., default = "api.bitbucket.org/2.0") {
    renv_config_get(
      name    = "bitbucket.host",
      type    = "character[1]",
      default = default,
      args    = list(...)
    )
  },

  copy.method = function(..., default = "auto") {
    renv_config_get(
      name    = "copy.method",
      type    = "*",
      default = default,
      args    = list(...)
    )
  },

  connect.timeout = function(..., default = 20L) {
    renv_config_get(
      name    = "connect.timeout",
      type    = "integer[1]",
      default = default,
      args    = list(...)
    )
  },

  connect.retry = function(..., default = 3L) {
    renv_config_get(
      name    = "connect.retry",
      type    = "integer[1]",
      default = default,
      args    = list(...)
    )
  },

  cache.enabled = function(..., default = TRUE) {
    renv_config_get(
      name    = "cache.enabled",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  cache.symlinks = function(..., default = .Platform$OS.type == "unix") {
    renv_config_get(
      name    = "cache.symlinks",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  dependency.errors = function(..., default = "reported") {
    renv_config_get(
      name    = "dependency.errors",
      type    = "character[1]",
      default = default,
      args    = list(...)
    )
  },

  dependencies.limit = function(..., default = 1000L) {
    renv_config_get(
      name    = "dependencies.limit",
      type    = "integer[1]",
      default = default,
      args    = list(...)
    )
  },

  exported.functions = function(..., default = "*") {
    renv_config_get(
      name    = "exported.functions",
      type    = "character[*]",
      default = default,
      args    = list(...)
    )
  },

  external.libraries = function(..., default = NULL) {
    renv_config_get(
      name    = "external.libraries",
      type    = "character[*]",
      default = default,
      args    = list(...)
    )
  },

  filebacked.cache = function(..., default = TRUE) {
    renv_config_get(
      name    = "filebacked.cache",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  github.host = function(..., default = "api.github.com") {
    renv_config_get(
      name    = "github.host",
      type    = "character[1]",
      default = default,
      args    = list(...)
    )
  },

  gitlab.host = function(..., default = "gitlab.com") {
    renv_config_get(
      name    = "gitlab.host",
      type    = "character[1]",
      default = default,
      args    = list(...)
    )
  },

  hydrate.libpaths = function(..., default = NULL) {
    renv_config_get(
      name    = "hydrate.libpaths",
      type    = "character[*]",
      default = default,
      args    = list(...)
    )
  },

  install.build = function(..., default = FALSE) {
    renv_config_get(
      name    = "install.build",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  install.remotes = function(..., default = TRUE) {
    renv_config_get(
      name    = "install.remotes",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  install.shortcuts = function(..., default = TRUE) {
    renv_config_get(
      name    = "install.shortcuts",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  install.staged = function(..., default = TRUE) {
    renv_config_get(
      name    = "install.staged",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  install.transactional = function(..., default = TRUE) {
    renv_config_get(
      name    = "install.transactional",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  install.verbose = function(..., default = FALSE) {
    renv_config_get(
      name    = "install.verbose",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  locking.enabled = function(..., default = FALSE) {
    renv_config_get(
      name    = "locking.enabled",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  mran.enabled = function(..., default = FALSE) {
    renv_config_get(
      name    = "mran.enabled",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  pak.enabled = function(..., default = FALSE) {
    renv_config_get(
      name    = "pak.enabled",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  ppm.enabled = function(..., default = TRUE) {
    renv_config_get(
      name    = "ppm.enabled",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  ppm.default = function(..., default = TRUE) {
    renv_config_get(
      name    = "ppm.default",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  ppm.url = function(..., default = "https://packagemanager.posit.co/cran/latest") {
    renv_config_get(
      name    = "ppm.url",
      type    = "character[1]",
      default = default,
      args    = list(...)
    )
  },

  repos.override = function(..., default = NULL) {
    renv_config_get(
      name    = "repos.override",
      type    = "character[*]",
      default = default,
      args    = list(...)
    )
  },

  rspm.enabled = function(..., default = TRUE) {
    renv_config_get(
      name    = "rspm.enabled",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  sandbox.enabled = function(..., default = TRUE) {
    renv_config_get(
      name    = "sandbox.enabled",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  shims.enabled = function(..., default = TRUE) {
    renv_config_get(
      name    = "shims.enabled",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  snapshot.inference = function(..., default = TRUE) {
    renv_config_get(
      name    = "snapshot.inference",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  snapshot.validate = function(..., default = TRUE) {
    renv_config_get(
      name    = "snapshot.validate",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  startup.quiet = function(..., default = NULL) {
    renv_config_get(
      name    = "startup.quiet",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  synchronized.check = function(..., default = TRUE) {
    renv_config_get(
      name    = "synchronized.check",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  updates.check = function(..., default = FALSE) {
    renv_config_get(
      name    = "updates.check",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  updates.parallel = function(..., default = 2L) {
    renv_config_get(
      name    = "updates.parallel",
      type    = "*",
      default = default,
      args    = list(...)
    )
  },

  user.environ = function(..., default = TRUE) {
    renv_config_get(
      name    = "user.environ",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  user.library = function(..., default = FALSE) {
    renv_config_get(
      name    = "user.library",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  },

  user.profile = function(..., default = FALSE) {
    renv_config_get(
      name    = "user.profile",
      type    = "logical[1]",
      default = default,
      args    = list(...)
    )
  }

)


# config.R -------------------------------------------------------------------


#' User-level settings
#'
#' Configure different behaviors of renv.
#'
#' For a given configuration option:
#'
#' 1. If an \R option of the form `renv.config.<name>` is available,
#'    then that option's value will be used;
#'
#' 2. If an environment variable of the form `RENV_CONFIG_<NAME>` is available,
#'   then that option's value will be used;
#'
#' 3. Otherwise, the default for that particular configuration value is used.
#'
#' Any periods (`.`)s in the option name are transformed into underscores (`_`)
#' in the environment variable name, and vice versa. For example, the
#' configuration option `auto.snapshot` could be configured as:
#'
#' - `options(renv.config.auto.snapshot = <...>)`
#' - `Sys.setenv(RENV_CONFIG_AUTO_SNAPSHOT = <...>)`
#'
#' Note that if both the \R option and the environment variable are defined, the
#' \R option will be used instead. Environment variables can be more useful when
#' you want a particular configuration to be automatically inherited by child
#' processes; if that behavior is not desired, then the R option may be
#' preferred.
#'
#' If you want to set and persist these options across multiple projects, it is
#' recommended that you set them in a a startup `.Renviron` file; e.g. in your
#' own `~/.Renviron`, or in the R installation's `etc/Rprofile.site` file. See
#' [Startup] for more details.
#'
#' Configuration options can also be set within the project `.Rprofile`, but
#' be aware the options should be set before `source("renv/activate.R")` is
#' called.
#'
#' @eval renv_roxygen_config_section()
#'
#' @section Copy methods:
#'
#' If you find that renv is unable to copy some directories in your
#' environment, you may want to try setting the `copy.method` option. By
#' default, renv will try to choose a system tool that is likely to succeed in
#' copying files on your system -- `robocopy` on Windows, and `cp` on Unix.
#' renv will also instruct these tools to preserve timestamps and attributes
#' when copying files. However, you can select a different method as
#' appropriate.
#'
#' The following methods are supported:
#'
#' \tabular{ll}{
#' `auto`     \tab Use `robocopy` on Windows, and `cp` on Unix-alikes. \cr
#' `R`        \tab Use \R's built-in `file.copy()` function. \cr
#' `cp`       \tab Use `cp` to copy files. \cr
#' `robocopy` \tab Use `robocopy` to copy files. (Only available on Windows.) \cr
#' `rsync`    \tab Use `rsync` to copy files. \cr
#' }
#'
#' You can also provide a custom copy method if required; e.g.
#'
#' ```
#' options(renv.config.copy.method = function(src, dst) {
#'   # copy a file from 'src' to 'dst'
#' })
#' ```
#'
#' Note that renv will always first attempt to copy a directory first to a
#' temporary path within the target folder, and then rename that temporary path
#' to the final target destination. This helps avoid issues where a failed
#' attempt to copy a directory could leave a half-copied directory behind
#' in the final location.
#'
#' @section Project-local settings:
#'
#' For settings that should persist alongside a particular project, the
#' various settings available in [settings] can be used.
#'
#' @examples
#'
#' # disable automatic snapshots
#' options(renv.config.auto.snapshot = FALSE)
#'
#' # disable with environment variable
#' Sys.setenv(RENV_CONFIG_AUTO_SNAPSHOT = FALSE)
#'
#' @rdname config
#' @name config
NULL

renv_config_get <- function(name,
                            scope   = "config",
                            type    = "*",
                            default = NULL,
                            args    = NULL)
{
  # check for R option of associated name
  optname <- tolower(name)
  optkey <- paste("renv", scope, optname, sep = ".")
  optval <- getOption(optkey)
  if (!is.null(optval))
    return(renv_config_validate(name, optval, type, default, args))

  # check for environment variable
  envname <- gsub(".", "_", toupper(name), fixed = TRUE)
  envkey <- paste("RENV", toupper(scope), envname, sep = "_")
  envval <- Sys.getenv(envkey, unset = NA)
  if (!is.na(envval) && nzchar(envval)) {
    decoded <- renv_config_decode_envvar(envkey, envval)
    return(renv_config_validate(name, decoded, type, default, args))
  }

  # return default if nothing found
  default

}

renv_config_decode_envvar <- function(envname, envval) {

  map <- env(
    "NULL" = NULL,
    "NA"   = NA,
    "NaN"  = NaN,
    "true" = TRUE,
    "True" = TRUE,
    "TRUE" = TRUE,
    "false" = FALSE,
    "False" = FALSE,
    "FALSE" = FALSE
  )

  if (exists(envval, envir = map, inherits = FALSE))
    return(get(envval, envir = map, inherits = FALSE))

  libvars <- c("RENV_CONFIG_EXTERNAL_LIBRARIES", "RENV_CONFIG_HYDRATE_LIBPATHS")
  pattern <- if (envname %in% libvars)
    "\\s*[:;,]\\s*"
  else
    "\\s*,\\s*"

  strsplit(envval, pattern, perl = TRUE)[[1L]]

}

renv_config_validate <- function(name, value, type, default, args) {

  # no validation required for type = '*'
  if (identical(type, "*"))
    return(value)

  # if 'value' is a function, invoke it with args
  if (is.function(value)) {
    value <- catch(do.call(value, args))
    if (inherits(value, "error")) {
      warning(value, call. = FALSE)
      return(default)
    }
  }

  # parse the type string
  pattern <- paste0(
    "^",          # start of specifier
    "([^[(]+)",   # type name
    "[[(]",       # opening bracket
    "([^])]+)",   # length specifier
    "[])]",       # closing bracket
    "$"           # end of specifier
  )

  m <- regexec(pattern, type)
  matches <- regmatches(type, m)
  fields <- matches[[1L]]

  # extract declared mode, size
  mode <- fields[[2L]]
  size <- fields[[3L]]

  # validate the requested size for this option
  if (!renv_config_validate_size(value, size)) {
    fmt <- "value for option '%s' does not satisfy constraint '%s'"
    warningf(fmt, name, type)
  }

  # convert NULL values to requested type
  if (is.null(value)) {
    value <- convert(value, mode)
    return(value)
  }

  # otherwise, validate that this is a valid option
  if (identical(storage.mode(value), mode))
    return(value)

  # try converting
  converted <- catchall(convert(value, mode))
  if (any(is.na(converted)) || inherits(converted, "condition")) {
    fmt <- "'%s' does not satisfy constraint '%s' for config '%s'; using default '%s' instead"
    warningf(fmt, stringify(value), type, name, stringify(default))
    return(default)
  }

  # ok, validated + converted option
  converted

}

renv_config_validate_size <- function(value, size) {

  case(
    size == "*" ~ TRUE,
    size == "+" ~ length(value) > 0,
    size == "?" ~ length(value) %in% c(0, 1),
    TRUE        ~ as.numeric(size) == length(value)
  )

}

renv_config_install_staged <- function(default = TRUE) {

  values <- c(
    config$install.staged(default = NULL),
    config$install.transactional(default = NULL),
    default
  )

  values[[1L]]

}


# consent.R ------------------------------------------------------------------


#' Consent to usage of renv
#'
#' Provide consent to renv, allowing it to write and update certain files
#' on your filesystem.
#'
#' As part of its normal operation, renv will write and update some files
#' in your project directory, as well as an application-specific cache
#' directory. These paths are documented within [paths].
#'
#' In accordance with the
#' [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html),
#' renv must first obtain consent from you, the user, before these actions
#' can be taken. Please call `renv::consent()` first to provide this consent.
#'
#' You can also set the \R option:
#'
#' ```
#' options(renv.consent = TRUE)
#' ```
#'
#' to implicitly provide consent for e.g. non-interactive \R sessions.
#'
#' @param provided The default provided response. If you need to provide
#'   consent from a non-interactive \R session, you can invoke
#'   `renv::consent(provided = TRUE)` explicitly.
#'
#' @return `TRUE` if consent is provided, or an \R error otherwise.
#'
#' @export
consent <- function(provided = FALSE) {

  # assume consent if embedded
  if (renv_metadata_embedded())
    return(TRUE)

  # compute path to root directory
  root <- renv_paths_root()
  if (renv_file_type(root) == "directory") {
    writef("- Consent to use renv has already been provided -- nothing to do.")
    return(invisible(TRUE))
  }

  # write welcome message
  template <- system.file("resources/WELCOME", package = "renv")
  contents <- readLines(template)
  replacements <- list(RENV_PATHS_ROOT = renv_path_pretty(root))
  welcome <- renv_template_replace(contents, replacements)
  writef(welcome)

  # ask user if they want to proceed
  response <- catchall(proceed(default = provided))
  if (!identical(response, TRUE)) {
    msg <- "consent was not provided; operation aborted"
    stop(msg, call. = FALSE)
  }

  # cache the user response
  options(renv.consent = TRUE)
  ensure_directory(root)
  writef("- %s has been created.", renv_path_pretty(root))

  invisible(TRUE)

}

renv_consent_check <- function() {

  # check for explicit consent
  consent <- getOption("renv.consent")
  if (identical(consent, TRUE))
    return(TRUE)
  else if (identical(consent, FALSE))
    stopf("consent has been explicitly withdrawn")

  # check for existence of root
  root <- renv_paths_root()
  if (renv_file_type(root) == "directory")
    return(TRUE)

  # check for implicit consent
  consented <-
    !interactive() ||
    renv_envvar_exists("CI") ||
    renv_envvar_exists("GITHUB_ACTION") ||
    renv_envvar_exists("RENV_PATHS_ROOT") ||
    file.exists("/.singularity.d") ||
    renv_virtualization_type() != "native"

  if (consented) {
    ensure_directory(root)
    return(TRUE)
  }

  # looks like the user's first interactive use of renv
  consent()

}


# cran.R ---------------------------------------------------------------------


# nocov start

renv_cran_status <- function(email   = NULL,
                             package = NULL,
                             view    = "maintainer")
{
  case(
    view == "maintainer" ~ renv_cran_status_maintainer(email, package),
    TRUE                 ~ stopf("unrecognized view '%s'", view)
  )

}

renv_cran_status_maintainer <- function(email, package) {

  email <- email %||% renv_cran_status_maintainer_email(package = package)
  parts <- strsplit(email, "@", fixed = TRUE)[[1L]]

  fmt <- "https://cran.r-project.org/web/checks/check_results_%s_at_%s.html"
  url <- sprintf(fmt, parts[[1L]], parts[[2L]])

  browseURL(url)

}

renv_cran_status_maintainer_email <- function(package = NULL) {

  mtr <- renv_package_description_field(
    package = package %||% "renv",
    field   = "Maintainer"
  )

  indices <- gregexpr("[<>]", mtr, perl = TRUE)[[1L]]
  substring(mtr, indices[[1L]] + 1L, indices[[2L]] - 1L)

}

# nocov end


# curl.R ---------------------------------------------------------------------


the$curl_valid <- new.env(parent = emptyenv())

renv_curl_exe <- function() {

  curl <- Sys.getenv("RENV_CURL_EXECUTABLE", unset = NA)
  if (is.na(curl))
    curl <- Sys.which("curl")

  if (!nzchar(curl))
    return(renv_curl_exe_missing(curl))

  renv_curl_validate(curl)

}

renv_curl_validate <- function(curl) {

  the$curl_valid[[curl]] <- the$curl_valid[[curl]] %||% {
    renv_curl_validate_impl(curl)
  }

}

renv_curl_validate_impl <- function(curl) {

  # make sure we can run this copy of curl
  # note that 'system2()' will give an error if curl isn't runnable at all
  output <- suppressWarnings(
    tryCatch(
      system2(
        command = curl,
        args = "--version",
        stdout = TRUE,
        stderr = TRUE
      ),
      error = identity
    )
  )

  if (!inherits(output, "error")) {
    status <- attr(output, "status") %||% 0L
    if (status == 0L)
      return(curl)
  }

  message <- if (inherits(output, "error"))
    conditionMessage(output)
  else
    output

  fmt <- "Error executing '%s --version': is your copy of curl functional?"
  footer <- sprintf(fmt, curl)
  all <- c("", header(paste(curl, "--version"), prefix = "$"), message, "", footer)

  defer(
    message(paste(all, collapse = "\n")),
    scope = renv_dynamic_envir()
  )

  return(curl)

}

renv_curl_exe_missing <- function(curl) {

  if (!once())
    return(invisible(curl))

  parts <- c(
    "curl does not appear to be installed; downloads will fail.",
    "See <https://rstudio.github.io/renv/articles/renv.html#downloads> for more information."
  )

  msg <- paste(parts, collapse = "\n")
  warning(msg, call. = FALSE)

  invisible(curl)

}


# data_frame.R ---------------------------------------------------------------


data_frame <- function(...) {
  as_data_frame(list(...))
}

as_data_frame <- function(data) {

  # split matrices into columns
  if (is.matrix(data)) {
    result <- vector("list", ncol(data))
    names(result) <- colnames(data)
    dimnames(data) <- NULL
    for (i in seq_len(ncol(data)))
      result[[i]] <- data[, i]
    data <- result
  }

  # convert other objects to lists
  if (!is.list(data))
    data <- as.list(data)

  # recycle columns
  n <- lengths(data, use.names = FALSE)
  nrow <- max(n)

  # start recycling
  for (i in seq_along(data)) {
    if (n[[i]] == 0L) {
      length(data[[i]]) <- nrow
    } else if (n[[i]] != nrow) {
      data[[i]] <- rep.int(data[[i]], nrow / n[[i]])
    }
  }

  # set attributes
  class(data) <- "data.frame"
  attr(data, "row.names") <- .set_row_names(nrow)

  # return data
  data

}


# dcf.R ----------------------------------------------------------------------


# similar to base::read.dcf(), but:
# - allows for whitespace between fields
# - allows for non-indented field continuations
# - always keeps whitespace
renv_dcf_read <- function(file, text = NULL, ...) {

  # read file
  contents <- text %||% renv_dcf_read_impl(file, ...)

  # split on newlines
  parts <- strsplit(contents, "\\r?\\n(?=\\S)", perl = TRUE)[[1L]]

  # remove embedded newlines
  parts <- gsub("\\r?\\n\\s*", " ", parts, perl = TRUE)

  # split into key / value pairs
  index <- regexpr(":", parts, fixed = TRUE)
  keys <- substring(parts, 1L, index - 1L)
  vals <- substring(parts, index + 1L)

  # trim whitespace
  vals <- trimws(vals)

  # return early if everything looks fine
  ok <- nzchar(keys)
  if (all(ok)) {
    storage.mode(vals) <- "list"
    names(vals) <- keys
    return(vals)
  }

  # otherwise, fix up bad continuations
  starts <- which(ok)
  ends <- c(tail(starts - 1L, n = -1L), length(keys))
  vals <- .mapply(
    function(start, end) paste(vals[start:end], collapse = " "),
    list(starts, ends),
    NULL
  )

  # set up names
  names(vals) <- keys[ok]

  # done
  vals

}

renv_dcf_read_impl_encoding <- function(bytes) {

  # try to find encoding -- if none is declared, assume native encoding?
  start <- 0L
  while (TRUE) {

    # find 'Encoding'
    start <- grepRaw("Encoding:", bytes, fixed = TRUE, offset = start + 1L)
    if (length(start) == 0L)
      return(NULL)

    # check for preceding newline, or start of file
    if (start == 1L || bytes[[start - 1L]] == 0x0A) {
      start <- start + 9L
      break
    }

  }

  # find the end of the encoding field
  end <- grepRaw("\\r?\\n", bytes, offset = start + 1L)
  if (length(end) == 0L)
    end <- length(bytes)

  # pull it out
  field <- rawToChar(bytes[start:end])
  trimws(field)

}

renv_dcf_read_impl <- function(file, ...) {

  # suppress warnings in this scope
  renv_scope_options(warn = -1L)

  # first, read the file as bytes to get encoding
  # use a guess for the file size to avoid expensive lookup, but fallback
  # if necessary
  bytes <- readBin(file, what = "raw", n = 8192L)
  if (length(bytes) == 8192L) {
    n <- renv_file_size(file)
    bytes <- readBin(con = file, what = "raw", n = n)
  }

  # try to guess the encoding
  encoding <- renv_dcf_read_impl_encoding(bytes)

  # try a bunch of candidate encodings
  encodings <- c(encoding, "UTF-8", "latin1", "")
  for (encoding in unique(encodings)) {
    result <- iconv(list(bytes), from = encoding, to = "UTF-8")
    if (!is.na(result))
      return(result)
  }

  # all else fails, just pretend it's in the native encoding
  rawToChar(bytes)

}

renv_dcf_write <- function(x, file = "") {

  keep.white <- c("Description", "Authors@R", "Author", "Built", "Packaged")
  result <- write.dcf(as.list(x), file = file, indent = 4L, width = 80L, keep.white = keep.white)

  renv_filebacked_invalidate(file)

  invisible(result)

}


# deactivate.R ---------------------------------------------------------------

#' @rdname activate
#' @param clean If `TRUE`, will also remove the `renv/` directory and the
#'   lockfile.
#' @export
deactivate <- function(project = NULL, clean = FALSE) {

  renv_scope_error_handler()

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  renv_infrastructure_remove_rprofile(project)

  unload(project)

  if (clean) {
    unlink(file.path(project, "renv.lock"))
    unlink(file.path(project, "renv"), recursive = TRUE, force = TRUE)
  }

  renv_restart_request(project, reason = "renv deactivated")
  invisible(project)

}


# debuggify.R ----------------------------------------------------------------


debuggify <- function(expr) {
  withCallingHandlers(expr, interrupt = renv_debuggify_dump)
}

renv_debuggify_dump <- function(cnd) {

  # print a backtrace
  status <- sys.status()
  calls <- head(status$sys.calls, n = -2L)
  frames <- head(status$sys.frames, n = -2L)
  traceback <- renv_error_format(calls, frames)
  caution(traceback)

  # print information about each frame
  n <- length(calls)
  for (i in seq_along(calls)) {
    renv_debuggify_dump_impl(
      index  = n - i + 1,
      call   = calls[[i]],
      frame  = frames[[i]]
    )
  }

}

renv_debuggify_dump_impl <- function(index, call, frame) {
  writeLines(header(paste("Frame", index)))
  vars <- ls(envir = frame, all.names = TRUE)
  lapply(vars, renv_debuggify_dump_impl_one, call = call, frame = frame)
  writeLines("")
}

renv_debuggify_dump_impl_one <- function(var, call, frame) {

  if (var %in% c("expr"))
    return("<promise>")

  str(frame[[var]])

}


# defer.R --------------------------------------------------------------------


# environment hosting exit callbacks
the$defer_callbacks <- new.env(parent = emptyenv())

defer <- function(expr, scope = parent.frame()) {

  handler <- renv_defer_add(
    list(expr = substitute(expr), envir = parent.frame()),
    envir = scope
  )

  invisible(handler)

}

renv_defer_id <- function(envir) {
  format.default(envir)
}

renv_defer_get <- function(envir) {
  id <- renv_defer_id(envir)
  the$defer_callbacks[[id]]
}

renv_defer_set <- function(envir, handlers) {

  # get any previously-set handlers. if we don't see any handlers registered,
  # this must be our first time registering exit handlers on the environment,
  # and so we'll want to register an on.exit handler to call our handlers
  oldhandlers <- renv_defer_get(envir)
  if (is.null(oldhandlers)) {
    call <- as.call(list(renv_defer_execute, envir))
    do.call(base::on.exit, list(substitute(call), TRUE), envir = envir)
  }

  # register the newly-set handlers
  id <- renv_defer_id(envir)
  the$defer_callbacks[[id]] <- handlers

}

renv_defer_remove <- function(envir) {
  id <- renv_defer_id(envir)
  rm(list = id, envir = the$defer_callbacks)
}

renv_defer_execute <- function(envir = parent.frame()) {

  # check for handlers -- may be NULL if they were intentionally executed
  # early via a call to `renv_defer_execute()`
  handlers <- renv_defer_get(envir)
  if (is.null(handlers))
    return()

  # execute the existing handlers
  for (handler in handlers)
    tryCatch(eval(handler$expr, handler$envir), error = identity)

  # remove the handlers
  renv_defer_remove(envir)

}

renv_defer_add <- function(envir, handler) {
  handlers <- c(list(handler), renv_defer_get(envir))
  renv_defer_set(envir, handlers)
  handler
}


# dependencies.R -------------------------------------------------------------


#' Find R package dependencies in a project
#'
#' @description
#' `dependencies()` will crawl files within your project, looking for \R files
#' and the packages used within those \R files. This is done primarily by
#' parsing the code and looking for calls of the form `library(package)`,
#' `require(package)`, `requireNamespace("package")`, and `package::method()`.
#' renv also supports package loading with
#' [box](https://cran.r-project.org/package=box) (`box::use(...)`) and
#' [pacman](https://cran.r-project.org/package=pacman) (`pacman::p_load(...)`)
#' .
#'
#' For \R package projects, dependencies expressed in the `DESCRIPTION` file
#' will also be discovered.
#'
#' Note that the rmarkdown package is required in order to crawl dependencies
#' in R Markdown files.
#'
#' # Missing dependencies
#'
#' `dependencies()` uses static analysis to determine which packages are used
#' by your project. This means that it inspects, but doesn't run, your
#' source. Static analysis generally works well, but is not 100% reliable in
#' detecting the packages required by your project. For example, renv is
#' unable to detect this kind of usage:
#'
#' ```{r eval=FALSE}
#' for (package in c("dplyr", "ggplot2")) {
#'   library(package, character.only = TRUE)
#' }
#' ```
#'
#' It also can't generally tell if one of the packages you use, uses one of
#' its suggested packages. For example, `tidyr::separate_wider_delim()`
#' uses the stringr package which is only suggested, not required by tidyr.
#'
#' If you find that renv's dependency discovery misses one or more packages
#' that you actually use in your project, one escape hatch is to include a file
#' called `_dependencies.R` that includes straightforward library calls:
#'
#' ```
#' library(dplyr)
#' library(ggplot2)
#' library(stringr)
#' ```
#'
#' # Explicit dependencies
#'
#' Alternatively, you can suppress dependency discover and instead rely
#' on an explicit set of packages recorded by you in a project `DESCRIPTION` file.
#' Call `renv::settings$snapshot.type("explicit")` to enable "explicit" mode,
#' then enumerate your dependencies in a project `DESCRIPTION` file.
#'
#' In that case, your `DESCRIPTION` might look something like this:
#'
#' ```
#' Type: project
#' Description: My project.
#' Depends:
#'     tidyverse,
#'     devtools,
#'     shiny,
#'     data.table
#' ```
#'
#' # Ignoring files
#'
#' By default, renv will read your project's `.gitignore`s (if present) to
#' determine whether certain files or folders should be included when traversing
#' directories. If preferred, you can also create a `.renvignore` file (with
#' entries of the same format as a standard `.gitignore` file) to tell renv
#' which files to ignore within a directory. If both `.renvignore` and
#' `.gitignore` exist within a folder, the `.renvignore` will be used in lieu of
#' the `.gitignore`.
#'
#' See <https://git-scm.com/docs/gitignore> for documentation on the
#' `.gitignore` format. Some simple examples here:
#'
#' ```
#' # ignore all R Markdown files
#' *.Rmd
#'
#' # ignore all data folders
#' data/
#'
#' # ignore only data folders from the root of the project
#' /data/
#' ```
#'
#' Using ignore files is important if your project contains a large number
#' of files; for example, if you have a `data/` directory containing many
#' text files.

#' # Errors
#'
#' renv's attempts to enumerate package dependencies in your project can fail
#' -- most commonly, because of failures when attempting to parse your \R code.
#' You can use the `errors` argument to suppress these problems, but a
#' more robust solution is tell renv not to look at the problematic code.
#' As well as using `.renvignore`, as described above, you can also suppress errors
#' discovered within individual `.Rmd` chunks by including `renv.ignore=TRUE`
#' in the chunk header. For example:
#'
#'     ```{r chunk-label, renv.ignore=TRUE}
#'     # code in this chunk will be ignored by renv
#'     ```
#'
#' Similarly, if you'd like renv to parse a chunk that is otherwise ignored
#' (e.g. because it has `eval=FALSE` as a chunk header), you can set:
#'
#'     ```{r chunk-label, eval=FALSE, renv.ignore=FALSE}
#'     # code in this chunk will _not_ be ignored
#'     ```
#'
#' # Development dependencies
#'
#' renv has some support for distinguishing between development and run-time
#' dependencies. For example, your Shiny app might rely on
#' [ggplot2](https://ggplot2.tidyverse.org) (a run-time dependency) but while
#' you use [usethis](https://usethis.r-lib.org) during development, your app
#' doesn't need it to run (i.e. it's only a development dependency).
#'
#' You can record development dependencies by listing them in the `Suggests`
#' field of your project's `DESCRIPTION` file. Development dependencies will be installed by
#' [renv::install()] (when called without arguments) but will not be tracked in
#' the project snapshot. If you need greater control, you can also try project
#' profiles as discussed in `vignette("profiles")`.
#'
#' @inheritParams renv-params
#'
#' @param path The path to a `.R`, `.Rmd`, `.qmd`, `DESCRIPTION`, a directory
#'   containing such files, or an R function. The default uses all files
#'   found within the current working directory and its children.
#'
#' @param root The root directory to be used for dependency discovery.
#'   Defaults to the active project directory. You may need to set this
#'   explicitly to ensure that your project's `.renvignore`s (if any) are
#'   properly handled.
#'
#' @param quiet Boolean; be quiet while checking for dependencies?
#'   Setting `quiet = TRUE` is equivalent to setting `progress = FALSE`
#'   and `errors = "ignored"`, and overrides those options when not `NULL`.
#'
#' @param progress Boolean; report progress output while enumerating
#'   dependencies?
#'
#' @param errors How should errors that occur during dependency enumeration be
#'   handled?
#'
#'   * `"reported"` (the default): errors are reported to the user, but
#'      otherwise ignored.
#'   * `"fatal"`: errors are fatal and stop execution.
#'   *  `"ignored"`: errors are ignored and not reported to the user.
#'
#' @param dev Boolean; include development dependencies? These packages are
#'   typically required when developing the project, but not when running it
#'   (i.e. you want them installed when humans are working on the project but
#'   not when computers are deploying it).
#'
#'   Development dependencies include packages listed in the `Suggests` field
#'   of a `DESCRIPTION` found in the project root, and roxygen2 or devtools if
#'   their use is implied by other project metadata. They also include packages
#'   used in `~/.Rprofile` if `config$user.profile()` is `TRUE`.
#'
#' @return An \R `data.frame` of discovered dependencies, mapping inferred
#'   package names to the files in which they were discovered. Note that the
#'   `Package` field might name a package remote, rather than just a plain
#'   package name.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # find R package dependencies in the current directory
#' renv::dependencies()
#'
#' }
dependencies <- function(
  path = getwd(),
  root = NULL,
  ...,
  quiet = NULL,
  progress = TRUE,
  errors = c("reported", "fatal", "ignored"),
  dev = FALSE)
{
  renv_scope_error_handler()

  # special case: if 'path' is a function, parse its body for dependencies
  if (is.function(path))
    return(renv_dependencies_discover_r(expr = body(path)))

  renv_dependencies_impl(
    path     = path,
    root     = root,
    quiet    = quiet,
    progress = progress,
    errors   = errors,
    dev      = dev,
    ...
  )
}

renv_dependencies_impl <- function(
  path = getwd(),
  ...,
  root = NULL,
  field = NULL,
  quiet = NULL,
  progress = FALSE,
  errors = c("reported", "fatal", "ignored"),
  dev = FALSE)
{
  renv_dots_check(...)

  path <- renv_path_normalize(path, mustWork = TRUE)
  root <- root %||% renv_dependencies_root(path)

  # handle 'quiet' parameter
  if (quiet %||% FALSE) {
    progress <- FALSE
    errors <- "ignored"
  }

  # ignore errors when testing, unless explicitly asked for
  if (renv_tests_running() && missing(errors))
    errors <- "ignored"

  # resolve errors
  errors <- match.arg(errors)

  before <- Sys.time()
  renv_dependencies_scope(root = root)
  files <- renv_dependencies_find(path, root)
  deps <- renv_dependencies_discover(files, progress, errors)
  after <- Sys.time()
  elapsed <- difftime(after, before, units = "secs")

  renv_condition_signal("renv.dependencies.elapsed_time", elapsed)

  renv_dependencies_report(errors)

  deps <- if (empty(deps) || nrow(deps) == 0L) {
    renv_dependencies_list_empty()
  } else {
    # drop NAs, and only keep 'dev' dependencies if requested
    rows(deps, deps$Dev %in% c(dev, FALSE))
  }

  take(deps, field)
}

renv_dependencies_root <- function(path = getwd()) {

  path <- renv_path_normalize(path, mustWork = TRUE)

  project <- renv_project_get(default = NULL)
  if (!is.null(project) && all(renv_path_within(path, project)))
    return(project)

  roots <- uapply(path, renv_dependencies_root_impl)
  if (empty(roots))
    return(NULL)

  uniroot <- unique(roots)
  if (length(uniroot) > 1)
    return(NULL)

  uniroot

}

renv_dependencies_root_impl <- function(path) {

  renv_file_find(path, function(parent) {
    anchors <- c("DESCRIPTION", ".git", ".Rproj.user", "renv.lock", "renv")
    for (anchor in anchors)
      if (file.exists(file.path(parent, anchor)))
        return(parent)
  })

}

renv_dependencies_callback <- function(path) {

  # user .Rprofile
  if (renv_path_same(path, Sys.getenv("R_PROFILE_USER", unset = "~/.Rprofile"))) {
    return(function(path) renv_dependencies_discover_r(path, dev = TRUE))
  }

  cbname <- list(
    ".Rprofile"     = function(path) renv_dependencies_discover_r(path),
    "DESCRIPTION"   = function(path) renv_dependencies_discover_description(path),
    "_bookdown.yml" = function(path) renv_dependencies_discover_bookdown(path),
    "_pkgdown.yml"  = function(path) renv_dependencies_discover_pkgdown(path),
    "_quarto.yml"   = function(path) renv_dependencies_discover_quarto(path),
    "renv.lock"     = function(path) renv_dependencies_discover_renv_lock(path),
    "rsconnect"     = function(path) renv_dependencies_discover_rsconnect(path)
  )

  cbext <- list(
    ".rproj"       = function(path) renv_dependencies_discover_rproj(path),
    ".r"           = function(path) renv_dependencies_discover_r(path),
    ".qmd"         = function(path) renv_dependencies_discover_multimode(path, "qmd"),
    ".rmd"         = function(path) renv_dependencies_discover_multimode(path, "rmd"),
    ".rmarkdown"   = function(path) renv_dependencies_discover_multimode(path, "rmd"),
    ".rnw"         = function(path) renv_dependencies_discover_multimode(path, "rnw"),
    ".ipynb"       = function(path) renv_dependencies_discover_ipynb(path)
  )

  name <- basename(path)
  ext  <- tolower(fileext(path))

  callback <- cbname[[name]] %||% cbext[[ext]]
  if (!is.null(callback))
    return(callback)

  # for files without an extension, check if those might be executable by R
  if (!nzchar(ext)) {
    shebang <- renv_file_shebang(path)
    if (grepl("\\b(?:R|r|Rscript)\\b", shebang))
      return(function(path) renv_dependencies_discover_r(path))
  }

}

renv_dependencies_find_extra <- function(root) {

  # if we don't have a root, we don't have a project
  if (is.null(root))
    return(NULL)

  # only run for root-level dependency checks
  project <- renv_project_resolve()
  if (!renv_path_same(root, project))
    return(NULL)

  # only run if we have a custom profile
  profile <- renv_profile_get()
  if (is.null(profile))
    return(NULL)

  # look for dependencies in the associated 'renv' folder
  path <- renv_paths_renv(project = project)
  renv_dependencies_find_impl(path, root, 0L)

}

renv_dependencies_find <- function(path = getwd(), root = getwd()) {
  files <- lapply(path, renv_dependencies_find_impl, root = root, depth = 0)
  extra <- renv_dependencies_find_extra(root)

  if (config$user.profile()) {
    rprofile_path <- Sys.getenv("R_PROFILE_USER", unset = "~/.Rprofile")
    if (file.exists(rprofile_path)) {
      extra <- c(extra, rprofile_path)
    }
  }

  unlist(c(files, extra), recursive = TRUE, use.names = FALSE)
}

renv_dependencies_find_impl <- function(path, root, depth) {

  # check file type
  info <- renv_file_info(path)

  # the file might have been removed after listing -- if so, just ignore it
  if (is.na(info$isdir))
    return(NULL)

  # if this is a directory, recurse
  if (info$isdir)
    return(renv_dependencies_find_dir(path, root, depth))

  path
}

renv_dependencies_find_dir <- function(path, root, depth) {

  # check if this path should be ignored
  excluded <- renv_renvignore_exec(path, root, path)
  if (excluded)
    return(character())

  # check if we've already scanned this directory
  # (necessary to guard against recursive symlinks)
  if (!renv_platform_windows()) {
    norm <- renv_path_normalize(path)
    state <- renv_dependencies_state()
    if (visited(norm, state$scanned))
      return(character())
  }

  # list children
  children <- renv_dependencies_find_dir_children(path, root, depth)

  # notify about number of children
  renv_condition_signal("renv.dependencies.count", list(path = path, count = length(children)))

  # find recursive dependencies
  depth <- depth + 1
  paths <- map(children, renv_dependencies_find_impl, root = root, depth = depth)

  # explicitly include rsconnect folder
  # (so we can infer a dependency on rsconnect when appropriate)
  rsconnect <- file.path(path, "rsconnect")
  if (file.exists(rsconnect))
    paths <- c(rsconnect, paths)

  paths

}

# return the set of files / subdirectories within a directory that should be
# crawled for dependencies
renv_dependencies_find_dir_children <- function(path, root, depth) {

  # list files in the folder
  children <- renv_file_list(path, full.names = TRUE)

  # skip if this contains too many files
  # https://github.com/rstudio/renv/issues/1186
  count <- length(children)
  if (count >= config$dependencies.limit()) {
    relpath <- renv_path_relative(path, dirname(root))
    renv_dependencies_find_dir_children_overload(relpath, count)
  }

  # remove files which are broken symlinks
  children <- children[file.exists(children)]

  # remove hard-coded ignores
  # (only keep DESCRIPTION files at the top level)
  ignored <- c("packrat", "renv", "revdep", "vendor", if (depth) "DESCRIPTION")
  children <- children[!basename(children) %in% ignored]

  # compute exclusions
  excluded <- renv_renvignore_exec(path, root, children)

  # keep only non-excluded children
  children[!excluded]

}

renv_dependencies_find_dir_children_overload <- function(path, count) {

  # check for missing state (e.g. if internal method called directly)
  state <- renv_dependencies_state()
  if (is.null(state))
    return()

  fmt <- "directory contains %s; consider ignoring this directory"
  msg <- sprintf(fmt, nplural("file", count))
  error <- simpleError(message = msg)

  path <- path %||% state$path
  problem <- list(file = path, error = error)
  state$problems$push(problem)

}

renv_dependencies_discover <- function(paths, progress, errors) {

  if (!renv_dependencies_discover_preflight(paths, errors))
    return(invisible(list()))

  # short path if we're not showing progress
  if (identical(progress, FALSE))
    return(bapply(paths, renv_dependencies_discover_impl))

  # otherwise, run with progress reporting

  # nocov start
  printf("Finding R package dependencies ... ")
  callback <- renv_progress_callback(renv_dependencies_discover_impl, length(paths))
  deps <- lapply(paths, callback)
  writef("Done!")

  bind(deps)
  # nocov end

}

renv_dependencies_discover_impl <- function(path) {

  callback <- renv_dependencies_callback(path)
  if (is.null(callback)) {
    return(NULL)
  }

  tryCatch(
    filebacked("dependencies", path, callback),
    error = function(cnd) {
      warning(cnd)
      NULL
    }
  )

}

renv_dependencies_discover_preflight <- function(paths, errors) {

  if (identical(errors, "ignored"))
    return(TRUE)

  if (length(paths) < config$dependencies.limit())
    return(TRUE)

  lines <- c(
    "A large number of files (%i in total) have been discovered.",
    "It may take renv a long time to crawl these files for dependencies.",
    "Consider using .renvignore to ignore irrelevant files.",
    "See `?renv::dependencies` for more information.",
    "Set `options(renv.config.dependencies.limit = Inf)` to disable this warning.",
    ""
  )
  writef(lines, length(paths))

  if (identical(errors, "reported"))
    return(TRUE)

  cancel_if(interactive() && !proceed())

  TRUE

}

renv_dependencies_discover_renv_lock <- function(path) {
  renv_dependencies_list(path, "renv")
}

renv_dependencies_discover_description_fields <- function(path, project = NULL) {

  # most callers don't pass in project so we need to get it from global state
  project <- project %||%
    renv_dependencies_state(key = "root") %||%
    renv_restore_state(key = "root") %||%
    renv_project_resolve()

  # by default, respect fields defined in settings
  fields <- settings$package.dependency.fields(project = project)

  # if this appears to be the DESCRIPTION associated with the active project,
  # and an explicit set of dependencies was provided in install, then use those
  if (renv_path_same(file.path(project, "DESCRIPTION"), path)) {
    default <- the$install_dependency_fields %||% c(fields, "Suggests")
    profile <- sprintf("Config/renv/profiles/%s/dependencies", renv_profile_get())
    fields <- c(default, profile)
  }

  fields

}

renv_dependencies_discover_description <- function(path,
                                                   fields = NULL,
                                                   subdir = NULL,
                                                   project = NULL)
{
  dcf <- catch(renv_description_read(path = path, subdir = subdir))
  if (inherits(dcf, "error"))
    return(renv_dependencies_error(path, error = dcf))

  # resolve the dependency fields to be used
  fields <- fields %||% renv_dependencies_discover_description_fields(path, project)

  # make sure dependency fields are expanded
  fields <- renv_description_dependency_fields_expand(fields)

  data <- map(
    fields,
    renv_dependencies_discover_description_impl,
    dcf  = dcf,
    path = path
  )

  # if this is a bioconductor package, add their implicit dependencies
  if ("biocViews" %in% names(dcf)) {
    data[[length(data) + 1L]] <- renv_dependencies_list(
      source = path,
      packages = c(renv_bioconductor_manager(), "BiocVersion")
    )
  }

  bind(data)

}

renv_dependencies_discover_description_impl <- function(dcf, field, path) {

  # read field
  contents <- dcf[[field]]
  if (!is.character(contents))
    return(list())

  # split on commas
  parts <- strsplit(dcf[[field]], "\\s*,\\s*")[[1]]

  # drop any empty fields
  x <- parts[nzchar(parts)]

  # match to split on remote, version
  pattern <- paste0(
    "([^,\\([:space:]]+)",                    # remote name
    "(?:\\s*\\(([><=]+)\\s*([0-9.-]+)\\))?"   # optional version specification
  )

  m <- regexec(pattern, x)
  matches <- regmatches(x, m)
  if (empty(matches))
    return(list())

  # create dependency list
  renv_dependencies_list(
    path,
    extract_chr(matches, 2L),
    extract_chr(matches, 3L),
    extract_chr(matches, 4L),
    dev = field == "Suggests"
  )

}

renv_dependencies_discover_bookdown <- function(path) {
  # TODO: other dependencies to parse from bookdown?
  renv_dependencies_list(path, "bookdown")
}

renv_dependencies_discover_pkgdown <- function(path) {
  # TODO: other dependencies to parse from pkgdown?
  renv_dependencies_list(path, "pkgdown")
}

renv_dependencies_discover_quarto <- function(path) {
  # TODO: other dependencies to parse from quarto?
  #
  # NOTE: we previously inferred a dependency on the R 'quarto' package here,
  # but quarto is normally invoked directly (rather than via the package) and
  # so such a dependency is not strictly necessary.
  #
  # https://github.com/rstudio/renv/issues/995
  renv_dependencies_list_empty()
}

renv_dependencies_discover_rsconnect <- function(path) {
  renv_dependencies_list(path, "rsconnect")
}

renv_dependencies_discover_multimode <- function(path, mode) {

  # TODO: find in-line R code?
  deps <- stack()

  if (mode %in% c("rmd", "qmd"))
    deps$push(renv_dependencies_discover_rmd_yaml_header(path, mode))

  deps$push(renv_dependencies_discover_chunks(path, mode))

  bind(Filter(NROW, deps$data()))

}

renv_dependencies_discover_rmd_yaml_header <- function(path, mode) {

  deps <- stack(mode = "character")

  # R Markdown documents always depend on rmarkdown
  if (identical(mode, "rmd"))
    deps$push("rmarkdown")

  # try and read the document's YAML header
  contents <- renv_file_read(path)
  pattern <- "(?:^|\n)\\s*---\\s*(?:$|\n)"
  matches <- gregexpr(pattern, contents, perl = TRUE)[[1L]]

  # check that we have something that looks like a YAML header
  ok <- length(matches) > 1L && matches[[1L]] == 1L
  if (!ok)
    return(renv_dependencies_list(path, packages = deps$data()))

  # require yaml package for parsing YAML header
  name <- case(
    mode == "rmd" ~ "R Markdown",
    mode == "qmd" ~ "Quarto Markdown"
  )

  # validate that we actually have the yaml package available
  if (!renv_dependencies_require("yaml", name)) {
    packages <- deps$data()
    return(renv_dependencies_list(path, packages))
  }

  # extract YAML text
  yamltext <- substring(contents, matches[[1L]] + 4L, matches[[2L]] - 1L)
  yaml <- catch(renv_yaml_load(yamltext))
  if (inherits(yaml, "error"))
    return(renv_dependencies_error(path, error = yaml, packages = "rmarkdown"))

  # check for Shiny runtime
  runtime <- yaml[["runtime"]] %||% ""
  if (pstring(runtime) && grepl("shiny", runtime, fixed = TRUE))
    deps$push("shiny")

  server <- yaml[["server"]] %||% ""
  if (identical(server, "shiny"))
    deps$push("shiny")

  if (is.list(server) && identical(server[["type"]], "shiny"))
    deps$push("shiny")

  pattern <- renv_regexps_package_name()

  # check recursively for package usages of the form 'package::method'
  recurse(yaml, function(node, stack) {
    # look for keys of the form 'package::method'
    values <- c(names(node), if (pstring(node)) node)
    for (value in values) {
      call <- tryCatch(parse(text = value)[[1]], error = function(err) NULL)
      if (renv_call_matches(call, name = c("::", ":::"), n_args = 2)) {
        deps$push(as.character(call[[2L]]))
      }
    }

  })

  # check for dependency on bslib
  theme <- catchall(yaml[[c("output", "html_document", "theme")]])
  if (!inherits(theme, "error") && is.list(theme))
    deps$push("bslib")

  # check for parameterized documents
  status <- catch(renv_dependencies_discover_rmd_yaml_header_params(yaml, deps))
  if (inherits(status, "error"))
    renv_dependencies_error_push(path, status)

  # get list of dependencies
  packages <- deps$data()
  renv_dependencies_list(path, packages)

}

renv_dependencies_discover_rmd_yaml_header_params <- function(yaml, deps) {

  # check for declared params
  params <- yaml[["params"]]
  if (!is.list(params))
    return()

  # infer dependency on shiny
  deps$push("shiny")

  # iterate through params, parsing dependencies from R code
  for (param in params) {

    # check for r types
    type <- attr(param, "type", exact = TRUE)
    if (!identical(type, "r"))
      next

    # attempt to parse dependencies
    rdeps <- catch(renv_dependencies_discover_r(text = param))
    if (inherits(rdeps, "error"))
      next

    # add each dependency
    for (package in sort(unique(rdeps$Package)))
      deps$push(package)

  }

}

renv_dependencies_discover_chunks_ignore <- function(chunk) {

  # if renv.ignore is set, respect it
  ignore <- chunk$params[["renv.ignore"]]
  if (!is.null(ignore))
    return(truthy(ignore))

  # skip non-R chunks
  engine <- chunk$params[["engine"]]
  ok <- is.character(engine) && engine %in% c("r", "rscript")
  if (!ok)
    return(TRUE)

  # skip un-evaluated chunks
  if (!truthy(chunk$params[["eval"]], default = TRUE))
    return(TRUE)

  # skip learnr exercises
  if (truthy(chunk$params[["exercise"]], default = FALSE))
    return(TRUE)

  # skip chunks whose labels end in '-display'
  label <- chunk$params[["label"]] %||% ""
  if (grepl("-display$", label))
    return(TRUE)

  # ok, don't ignore this chunk
  FALSE

}

renv_dependencies_discover_chunks <- function(path, mode) {

  # figure out the appropriate begin, end patterns
  type <- tolower(file_ext(path))
  if (type %in% c("rmd", "qmd", "rmarkdown"))
    type <- "md"

  allpatterns <- renv_knitr_patterns()
  patterns <- allpatterns[[type]]
  if (is.null(patterns)) {
    condition <- simpleCondition("not a recognized multi-mode R document")
    return(renv_dependencies_error(path, error = condition))
  }

  # parse the chunks within
  # NOTE: we need to proceed line-by-line since the chunk end pattern might
  # end chunks not started by the chunk begin pattern (sad face)
  encoding <- if (type == "md") "UTF-8" else "unknown"
  contents <- readLines(path, warn = FALSE, encoding = encoding)
  ranges <- renv_dependencies_discover_chunks_ranges(path, contents, patterns)

  # extract chunk code from the used ranges
  chunks <- .mapply(function(lhs, rhs) {

    # parse params in header
    header <- contents[[lhs]]
    params <- renv_knitr_options_header(header, type)

    # extract chunk contents (preserve newlines for nicer error reporting)
    range <- seq.int(lhs + 1, length.out = rhs - lhs - 1)
    code <- rep.int("", length(contents))
    code[range] <- contents[range]

    # also parse chunk options
    params <- overlay(params, renv_knitr_options_chunk(code))

    # return list of outputs
    list(params = params, code = code)

  }, ranges, NULL)

  # iterate over chunks, and attempt to parse dependencies from each
  cdeps <- bapply(chunks, function(chunk) {

    # check whether this chunk should be ignored
    if (renv_dependencies_discover_chunks_ignore(chunk))
      return(character())

    # remove reused chunk placeholders
    pattern <- "<<[^>]+>>"
    code <- gsub(pattern, "", chunk$code)

    # okay, now we can discover deps
    deps <- catch(renv_dependencies_discover_r(path = path, text = code))
    if (inherits(deps, "error"))
      return(renv_dependencies_error(path, error = deps))

    deps

  })

  # check for dependencies in inline chunks as well
  ideps <- renv_dependencies_discover_chunks_inline(path, contents)

  # if this is a .qmd, infer a dependency on rmarkdown if we have any R chunks
  qdeps <- NULL
  if (mode %in% "qmd") {
    for (chunk in chunks) {
      engine <- chunk$params[["engine"]]
      if (is.character(engine) && engine %in% c("r", "rscript")) {
        qdeps <- renv_dependencies_list(path, "rmarkdown")
        break
      }
    }
  }

  # paste them all together
  deps <- bind(list(cdeps, ideps, qdeps))
  if (is.null(deps))
    return(deps)

  deps$Source <- path
  deps

}

renv_dependencies_discover_chunks_inline <- function(path, contents) {

  pasted <- paste(contents, collapse = "\n")
  matches <- gregexpr("`r ([^`]+)`", pasted, perl = TRUE)
  if (identical(c(matches[[1L]]), -1L))
    return(list())

  text <- unlist(regmatches(pasted, matches), use.names = FALSE, recursive = FALSE)
  code <- substring(text, 4L, nchar(text) - 1L)
  deps <- renv_dependencies_discover_r(path = path, text = code)
  if (inherits(deps, "error"))
    return(renv_dependencies_error(path, error = deps))

  deps

}

renv_dependencies_discover_chunks_ranges <- function(path, contents, patterns) {

  output <- list()

  chunk <- FALSE
  start <- 1; end <- 1
  for (i in seq_along(contents)) {

    line <- contents[[i]]

    if (chunk == FALSE && grepl(patterns$chunk.begin, line)) {
      chunk <- TRUE
      start <- i
      next
    }

    if (chunk == TRUE && grepl(patterns$chunk.begin, line)) {
      end <- i
      output[[length(output) + 1]] <- list(lhs = start, rhs = end)
      start <- i
      next
    }

    if (chunk == TRUE && grepl(patterns$chunk.end, line)) {
      chunk <- FALSE
      end <- i
      output[[length(output) + 1]] <- list(lhs = start, rhs = end)
      next
    }

  }

  if (chunk) {
    message <- sprintf("chunk starting on line %i is not closed", start)
    error <- simpleError(message)
    renv_dependencies_error(path, error = error)
  }

  bind(output)

}

renv_dependencies_discover_ipynb <- function(path) {

  json <- renv_json_read(path)
  if (!identical(json$metadata$kernelspec$language, "R"))
    return()

  deps <- stack()
  if (identical(json$metadata$kernelspec$name, "ir"))
    deps$push(renv_dependencies_list(path, "IRkernel"))

  for (cell in json$cells) {
    if (cell$cell_type != "code")
      next

    code <- paste0(cell$source, collapse = "")
    deps$push(renv_dependencies_discover_r(path, text = code))
  }

  bind(deps$data())

}

renv_dependencies_discover_rproj <- function(path) {

  props <- renv_properties_read(path)

  deps <- stack()
  if (identical(props$PackageUseDevtools, "Yes")) {
    deps$push("devtools")
    deps$push("roxygen2")
  }

  renv_dependencies_list(path, deps$data(), dev = TRUE)

}

renv_dependencies_discover_r <- function(path = NULL,
                                         text = NULL,
                                         expr = NULL,
                                         envir = NULL,
                                         dev = FALSE)
{
  expr <- case(
    is.function(expr)  ~ body(expr),
    is.language(expr)  ~ expr,
    is.character(expr) ~ catch(renv_parse_text(expr)),
    is.character(text) ~ catch(renv_parse_text(text)),
    is.character(path) ~ catch(renv_parse_file(path)),
    ~ stop("internal error")
  )

  if (inherits(expr, "error"))
    return(renv_dependencies_error(path, error = expr))

  # update current path
  state <- renv_dependencies_state()
  if (!is.null(state))
    renv_scope_binding(state, "path", path)

  methods <- c(
    renv_dependencies_discover_r_methods,
    renv_dependencies_discover_r_xfun,
    renv_dependencies_discover_r_library_require,
    renv_dependencies_discover_r_require_namespace,
    renv_dependencies_discover_r_colon,
    renv_dependencies_discover_r_pacman,
    renv_dependencies_discover_r_modules,
    renv_dependencies_discover_r_import,
    renv_dependencies_discover_r_box,
    renv_dependencies_discover_r_targets,
    renv_dependencies_discover_r_glue,
    renv_dependencies_discover_r_parsnip,
    renv_dependencies_discover_r_database
  )

  envir <- envir %||% new.env(parent = emptyenv())
  recurse(expr, function(node, stack) {

    # normalize calls (handle magrittr pipes)
    node <- renv_call_normalize(node, stack)

    # invoke methods on call objects
    if (is.call(node))
      for (method in methods)
        method(node, stack, envir)

    # return node
    node

  })

  packages <- ls(envir = envir, all.names = TRUE)
  renv_dependencies_list(path, packages, dev = dev)
}

renv_dependencies_discover_r_methods <- function(node, stack, envir) {

  node <- renv_call_expect(node, "methods", c("setClass", "setGeneric"))
  if (is.null(node))
    return(FALSE)

  envir[["methods"]] <- TRUE
  TRUE

}

renv_dependencies_discover_r_xfun <- function(node, stack, envir) {

  node <- renv_call_expect(node, "xfun", c("pkg_attach", "pkg_attach2"))
  if (is.null(node))
    return(FALSE)

  # attempt to match the call
  prototype <- function(..., install = FALSE, message = TRUE) {}
  matched <- catch(match.call(prototype, node, expand.dots = FALSE))
  if (inherits(matched, "error"))
    return(FALSE)

  # extract character vectors from `...`
  strings <- stack()
  recurse(matched[["..."]], function(node, stack) {
    if (is.character(node))
      strings$push(node)
  })

  # mark packages as known
  packages <- strings$data()
  if (empty(packages))
    return(FALSE)

  for (package in packages)
    envir[[package]] <- TRUE

  TRUE
}

renv_dependencies_discover_r_library_require <- function(node, stack, envir) {

  node <- renv_call_expect(node, "base", c("library", "require"))
  if (is.null(node))
    return(FALSE)

  # attempt to match the call
  matched <- catch(match.call(base::library, node))
  if (inherits(matched, "error"))
    return(FALSE)

  # if the 'package' argument is a character vector of length one, we're done
  if (is.character(matched$package) &&
      length(matched$package) == 1)
  {
    envir[[matched$package]] <- TRUE
    return(TRUE)
  }

  # if it's a symbol, double check character.only argument
  if (is.symbol(matched$package) &&
      identical(matched$character.only %||% FALSE, FALSE))
  {
    envir[[as.character(matched$package)]] <- TRUE
    return(TRUE)
  }

  FALSE

}

renv_dependencies_discover_r_require_namespace <- function(node, stack, envir) {

  node <- renv_call_expect(node, "base", c("requireNamespace", "loadNamespace"))
  if (is.null(node))
    return(FALSE)

  f <- get(as.character(node[[1]]), envir = .BaseNamespaceEnv, inherits = FALSE)
  matched <- catch(match.call(f, node))
  if (inherits(matched, "error"))
    return(FALSE)

  package <- matched$package
  if (is.character(package) && length(package == 1)) {
    envir[[package]] <- TRUE
    return(TRUE)
  }

  FALSE


}

renv_dependencies_discover_r_colon <- function(node, stack, envir) {

  ok <- renv_call_matches(node, name = c("::", ":::"), n_args = 2)

  if (!ok)
    return(FALSE)

  package <- node[[2L]]
  if (is.symbol(package))
    package <- as.character(package)

  if (!is.character(package) || length(package) != 1)
    return(FALSE)

  envir[[package]] <- TRUE
  TRUE

}

renv_dependencies_discover_r_pacman <- function(node, stack, envir) {

  node <- renv_call_expect(node, "pacman", "p_load")
  if (is.null(node) || length(node) < 2)
    return(FALSE)

  # check for character.only
  chonly <- node[["character.only"]] %||% FALSE

  # consider all unnamed arguments
  parts <- as.list(node[-1L])

  # consider packages passed to 'char' parameter
  char <- node[["char"]]

  # detect vector of packages passed as vector
  if (renv_call_matches(char, name = "c"))
    parts <- c(parts, as.list(char[-1L]))

  # detect plain old package name
  if (is.character(char))
    parts <- c(parts, as.list(char))

  # ensure names
  names(parts) <- names(parts) %||% rep.int("", length(parts))
  unnamed <- parts[!nzchar(names(parts))]

  # extract symbols / characters
  for (arg in unnamed) {

    # skip symbols if necessary
    if (chonly && is.symbol(arg))
      next

    # check for character or symbol
    ok <-
      length(arg) == 1 &&
      is.character(arg) ||
      is.symbol(arg)

    if (!ok)
      next

    # add it
    envir[[as.character(arg)]] <- TRUE

  }

  TRUE

}

renv_dependencies_discover_r_modules <- function(node, stack, envir) {

  # check for call of the form 'pkg::foo(a, b, c)'
  colon <- renv_call_matches(node[[1]], name = c("::", ":::"), n_args = 2)

  node <- renv_call_expect(node, "modules", c("import"))
  if (is.null(node))
    return(FALSE)

  ok <- FALSE
  if (colon) {
    # include if fully qualified call to modules::import
    ok <- TRUE
  } else {
    # otherwise only consider calls within a 'module' block
    # (to reduce confusion with reticulate::import)
    for (parent in stack) {
      parent <- renv_call_expect(parent, "modules", c("amodule", "module"))
      if (!is.null(parent)) {
        ok <- TRUE
        break
      }
    }
  }

  if (!ok)
    return(FALSE)

  # attempt to match the call
  prototype <- function(from, ..., attach = TRUE, where = parent.frame()) {}
  matched <- catch(match.call(prototype, node, expand.dots = FALSE))
  if (inherits(matched, "error"))
    return(FALSE)

  # extract character vector or symbol from `from`
  package <- matched[["from"]]
  if (empty(package))
    return(FALSE)

  # package could be symbols or character so call as.character
  # to be safe then mark packages as known
  envir[[as.character(package)]] <- TRUE

  TRUE
}

renv_dependencies_discover_r_import <- function(node, stack, envir) {

  node <- renv_call_expect(node, "import", c("from", "here", "into"))
  if (is.null(node))
    return(FALSE)

  # attempt to match the call
  name <- as.character(node[[1L]])
  matched <- if (name == "from") {
    catch(match.call(function(.from, ...) {}, node, expand.dots = FALSE))
  } else {
    catch(match.call(function(..., .from) {}, node, expand.dots = FALSE))
  }

  if (inherits(matched, "error"))
    return(FALSE)

  # the '.from' argument is the package name, either a character vector of length one or a symbol
  from <- matched$.from
  if (is.symbol(from))
    from <- as.character(from)

  ok <-
    is.character(from) &&
    length(from) == 1

  if (!ok)
    return(FALSE)

  envir[[from]] <- TRUE
  TRUE

}

renv_dependencies_discover_r_box <- function(node, stack, envir) {

  node <- renv_call_expect(node, "box", "use")
  if (is.null(node))
    return(FALSE)

  for (i in seq.int(2L, length.out = length(node) - 1L))
    renv_dependencies_discover_r_box_impl(node[[i]], stack, envir)

  TRUE

}

renv_dependencies_discover_r_box_impl <- function(node, stack, envir) {

  # if the call uses /, it's a path, not a package
  while (renv_call_matches(node, name = "/"))
    return(FALSE)

  # if the node is just a symbol, then it's the name of a package
  # otherwise, if it's a call to `[`, the first argument is the package name
  name <- if (is.symbol(node) && !identical(node, quote(expr = ))) {
    as.character(node)
  } else if (
    renv_call_matches(node, name = "[") &&
      length(node) > 1L &&
      is.symbol(node[[2L]])) {
    as.character(node[[2L]])
  }

  # the names `.` and `..` are special place holders and don't refer to packages
  if (is.null(name) || name == "." || name == "..")
    return(FALSE)

  envir[[name]] <- TRUE
  TRUE

}

renv_dependencies_discover_r_targets <- function(node, stack, envir) {

  node <- renv_call_expect(node, "targets", "tar_option_set")
  if (is.null(node))
    return(FALSE)

  envir[["targets"]] <- TRUE

  packages <- tryCatch(
    renv_dependencies_eval(node$packages),
    error = identity
  )

  # TODO: evaluation can fail for a multitude of reasons;
  # are any of these worth signalling to the user?
  if (inherits(packages, "error"))
    return(TRUE)

  if (is.character(packages))
    for (package in packages)
      envir[[package]] <- TRUE

  TRUE

}

renv_dependencies_discover_r_glue <- function(node, stack, envir) {

  node <- renv_call_expect(node, "glue", "glue")
  if (is.null(node))
    return(FALSE)

  # analyze all unnamed strings in the call
  args <- as.list(node)[-1L]
  nm <- names(args) %||% rep.int("", length(args))
  strings <- args[!nzchar(nm) & map_lgl(args, is.character)]

  # start iterating through the strings, looking for code chunks
  for (string in strings)
    renv_dependencies_discover_r_glue_impl(string, node, envir)

  TRUE

}

renv_dependencies_discover_r_glue_impl <- function(string, node, envir) {

  # get open, close delimiters
  ropen    <- charToRaw(node$.open    %||% "{")
  rclose   <- charToRaw(node$.close   %||% "}")
  rcomment <- charToRaw(node$.comment %||% "#")

  # constants
  rcomment   <- charToRaw("#")
  rbackslash <- charToRaw("\\")
  rquotes <- c(
    charToRaw("'"),
    charToRaw("\""),
    charToRaw("`")
  )

  # iterate through characters in string
  raw <- c(charToRaw(string), as.raw(0L))
  i <- 0L
  n <- length(raw)
  quote <- raw()

  # index for open delimiter match
  index <- 0L
  count <- 0L

  while (i < n) {

    # ensure we always advance index
    i <- i + 1L

    # handle quoted states
    if (length(quote)) {

      # skip escaped characters
      if (raw[[i]] == rbackslash) {
        i <- i + 1L
        next
      }

      # check for escape from quote
      if (raw[[i]] == quote) {
        quote <- raw()
        next
      }

    }

    # skip comments
    if (raw[[i]] == rcomment) {
      i <- grepRaw("(?:$|\n)", raw, i)
      next
    }

    # skip escaped characters
    if (raw[[i]] == rbackslash) {
      i <- i + 1L
      next
    }

    # check for quotes
    idx <- match(raw[[i]], rquotes, nomatch = 0L)
    if (idx > 0) {
      quote <- rquotes[[idx]]
      next
    }

    # check for open delimiter
    if (i %in% grepRaw(ropen, raw, i, fixed = TRUE)) {

      # check for duplicate (escape)
      j <- i + length(ropen)
      if (j %in% grepRaw(ropen, raw, j, fixed = TRUE)) {
        i <- j + length(ropen) - 1L
        next
      }

      # save index if we're starting a match
      if (count == 0L) {
        index <- i
      }

      # increment match count
      count <- count + 1L
      next

    }

    # check for close delimiter
    if (i %in% grepRaw(rclose, raw, i, fixed = TRUE)) {

      # check for duplicate (escape)
      j <- i + length(rclose)
      if (j %in% grepRaw(rclose, raw, j, fixed = TRUE)) {
        i <- j + length(rclose) - 1L
        next
      }

      if (count > 0L) {

        # decrement count if we have a match
        count <- count - 1L

        # check for match and parse dependencies within
        if (count == 0L) {

          # extract inner code
          lhs <- index + length(ropen)
          rhs <- i - 1L
          code <- rawToChar(raw[lhs:rhs])

          # parse dependencies
          renv_dependencies_discover_r(text = code, envir = envir)

        }

      }

    }

  }

}

renv_dependencies_discover_r_parsnip <- function(node, stack, envir) {

  node <- renv_call_expect(node, "parsnip", "set_engine")
  if (is.null(node))
    return(FALSE)

  matched <- catch(match.call(function(object, engine, ...) {}, node))
  if (inherits(matched, "error"))
    return(FALSE)

  engine <- matched$engine
  if (!is.character(engine) || length(engine) != 1L)
    return(FALSE)

  map <- getOption("renv.parsnip.engines", default = list(
    glm    = "stats",
    glmnet = "glmnet",
    keras  = "keras",
    kknn   = "kknn",
    nnet   = "nnet",
    rpart  = "rpart",
    spark  = "sparklyr",
    stan   = "rstanarm"
  ))

  packages <- if (is.function(map))
    tryCatch(map(engine), error = function(e) NULL)
  else
    map[[engine]]

  if (is.null(packages))
    return(FALSE)

  for (package in packages)
    envir[[package]] <- TRUE

  # TODO: a number of model routines appear to depend on dials;
  # should we just assume it's required by default? or should
  # users normally be using tidymodels instead of parsnip directly?
  TRUE

}

renv_dependencies_discover_r_database <- function(node, stack, envir) {

  found <- FALSE

  db <- renv_dependencies_database()
  enumerate(db, function(package, dependencies) {
    enumerate(dependencies, function(method, requirements) {

      expect <- renv_call_expect(node, package, method)
      if (is.null(expect))
        return(FALSE)

      for (requirement in requirements)
        envir[[requirement]] <- TRUE

      found <<- TRUE
      TRUE

    })
  })

  found

}

renv_dependencies_database <- function() {
  dynamic(
    key   = list(),
    value = renv_dependencies_database_impl()
  )
}

renv_dependencies_database_impl <- function() {
  db <- getOption("renv.dependencies.database", default = list())
  db$ggplot2$geom_hex <- "hexbin"
  db
}

renv_dependencies_list <- function(source,
                                   packages,
                                   require = "",
                                   version = "",
                                   dev = FALSE)
{
  if (empty(packages))
    return(renv_dependencies_list_empty())

  source <- source %||% rep.int(NA_character_, length(packages))

  data_frame(
    Source  = as.character(source),
    Package = as.character(packages),
    Require = require,
    Version = version,
    Dev     = dev
  )

}

renv_dependencies_list_empty <- function() {

  data_frame(
    Source  = character(),
    Package = character(),
    Require = character(),
    Version = character(),
    Dev     = logical()
  )

}

renv_dependencies_require <- function(package, type = NULL) {

  if (requireNamespace(package, quietly = TRUE))
    return(TRUE)

  if (once()) {

    fmt <- lines(
      "The '%1$s' package is required to parse dependencies within %2$s",
      "Consider installing it with `install.packages(\"%1$s\")`."
    )

    within <- if (is.null(type)) "this project" else paste(type, "files")
    warningf(fmt, package, within)

  }

  return(FALSE)

}

the$dependencies_state <- NULL

renv_dependencies_state <- function(key = NULL) {
  state <- the$dependencies_state
  if (is.null(key)) state else state[[key]]
}

renv_dependencies_scope <- function(root = NULL, scope = parent.frame()) {
  state <- env(root = root, scanned = env(), problems = stack())
  the$dependencies_state <- state
  defer(the$dependencies_state <- NULL, scope = scope)
}

renv_dependencies_error_push <- function(path = NULL, error = NULL) {

  state <- renv_dependencies_state()
  if (is.null(state))
    return()

  path <- path %||% state$path
  problem <- list(file = path, error = error)
  state$problems$push(problem)

}

renv_dependencies_error <- function(path, error = NULL, packages = NULL) {

  # if no error, return early
  if (is.null(error))
    return(renv_dependencies_list(path, packages))

  # push the error report
  renv_dependencies_error_push(path, error)

  # return dependency list
  renv_dependencies_list(path, packages)

}

renv_dependencies_report <- function(errors) {

  if (identical(errors, "ignored"))
    return(FALSE)

  state <- renv_dependencies_state()
  if (is.null(state))
    return(FALSE)

  problems <- state$problems$data()
  if (empty(problems))
    return(TRUE)

  # bind into list
  bound <- bapply(problems, function(problem) {
    fields <- c(renv_path_aliased(problem$file), problem$line, problem$column)
    header <- paste(fields, collapse = ":")
    message <- conditionMessage(problem$error)
    c(file = problem$file, header = header, message = message)
  })

  # split based on header (group errors from same file)
  splat <- split(bound, bound$file)

  # emit messages
  lines <- enumerate(splat, function(file, problem) {
    messages <- paste("Error", problem$message, sep = ": ", collapse = "\n\n")
    paste(c(header(file), messages, ""), collapse = "\n")
  })

  caution_bullets(
    "WARNING: One or more problems were discovered while enumerating dependencies.",
    c("", lines),
    "Please see `?renv::dependencies` for more information.",
    bullets = FALSE
  )

  if (identical(errors, "fatal")) {
    fmt <- "one or more problems were encountered while enumerating dependencies"
    stopf(fmt)
  }

  renv_condition_signal("renv.dependencies.problems", problems)
  TRUE

}

renv_dependencies_eval <- function(expr) {

  # create environment with small subset of "safe" symbols, that
  # are commonly used for chunk expressions
  syms <- c(
    "list", "c", "T", "F",
    "{", "(", "[", "[[",
    "::", ":::", "$", "@",
    ":",
    "+", "-", "*", "/",
    "<", ">", "<=", ">=", "==", "!=",
    "!",
    "&", "&&", "|", "||"
  )

  vals <- mget(syms, envir = baseenv())
  envir <- list2env(vals, parent = emptyenv())

  # evaluate in that environment
  eval(expr, envir = envir)

}


# description.R --------------------------------------------------------------


renv_description_read <- function(path = NULL,
                                  package = NULL,
                                  subdir = NULL,
                                  field = NULL,
                                  ...)
{
  # if given a package name, construct path to that package
  path <- path %||% find.package(package)

  # normalize non-absolute paths
  if (!renv_path_absolute(path))
    path <- renv_path_normalize(path)

  # if 'path' refers to a directory, try to resolve the DESCRIPTION file
  if (dir.exists(path)) {
    components <- c(path, if (nzchar(subdir %||% "")) subdir, "DESCRIPTION")
    path <- paste(components, collapse = "/")
  }

  # if the DESCRIPTION file doesn't exist, bail
  if (!file.exists(path))
    stopf("DESCRIPTION file %s does not exist", renv_path_pretty(path))

  # read value with filebacked cache
  description <- filebacked(
    context  = "renv_description_read",
    path     = path,
    callback = renv_description_read_impl,
    subdir   = subdir,
    ...
  )

  if (!is.null(field))
    return(description[[field]])

  description

}

renv_description_read_impl <- function(path = NULL, subdir = NULL, ...) {

  # if we have an archive, attempt to unpack the DESCRIPTION
  type <- renv_archive_type(path)
  if (type != "unknown") {

    # list files within the archive
    files <- renv_archive_list(path)

    # find the DESCRIPTION file. note that for some source tarballs (e.g.
    # those from GitHub) the first entry may not be the package name, so
    # just consume everything up to the first slash
    subdir <- subdir %||% ""
    parts <- c("^[^/]+", if (nzchar(subdir)) subdir, "DESCRIPTION$")
    pattern <- paste(parts, collapse = "/")

    descs <- grep(pattern, files, value = TRUE)
    if (empty(descs)) {
      fmt <- "archive '%s' does not appear to contain a DESCRIPTION file"
      stopf(fmt, renv_path_aliased(path))
    }

    # choose the shortest DESCRPITION file matching
    # unpack into tempdir location
    file <- descs[[1]]
    exdir <- renv_scope_tempfile("renv-description-")
    renv_archive_decompress(path, files = file, exdir = exdir)

    # update path to extracted DESCRIPTION
    path <- file.path(exdir, file)

  }

  # read DESCRIPTION as dcf
  dcf <- renv_dcf_read(path, ...)
  if (empty(dcf))
    stopf("DESCRIPTION file at '%s' is empty", path)

  dcf

}

renv_description_path <- function(path) {
  childpath <- file.path(path, "DESCRIPTION")
  indirect <- file.exists(childpath)
  path[indirect] <- childpath[indirect]
  path
}

# parse the dependency requirements normally presented in
# Depends, Imports, Suggests, and so on
renv_description_parse_field <- function(field) {

  # check for invalid / unexpected inputs
  if (is.null(field) || is.na(field) || !nzchar(field))
    return(NULL)

  pattern <- paste0(
    "([a-zA-Z0-9._]+)",                      # package name
    "(?:\\s*\\(([><=]+)\\s*([0-9.-]+)\\))?"  # optional version specification
  )

  # split on commas
  parts <- strsplit(field, "\\s*,\\s*")[[1]]

  # drop any empty fields
  x <- parts[nzchar(parts)]

  # match to split on package name, version
  m <- regexec(pattern, x)
  matches <- regmatches(x, m)
  if (empty(matches))
    return(NULL)

  data_frame(
    Package = extract_chr(matches, 2L),
    Require = extract_chr(matches, 3L),
    Version = extract_chr(matches, 4L)
  )

}

renv_description_resolve <- function(path) {

  case(
    is.list(path)      ~ path,
    is.character(path) ~ renv_description_read(path = path)
  )

}

renv_description_built_version <- function(desc = NULL) {

  desc <- renv_description_resolve(desc)

  built <- desc[["Built"]]
  if (is.null(built))
    return(NA)

  substring(built, 3L, regexpr(";", built, fixed = TRUE) - 1L)
}

renv_description_dependency_fields_expand <- function(fields) {

  expanded <- map(fields, function(field) {

    case(

      identical(field, FALSE)
      ~ NULL,

      identical(field, "strong") || is.na(field)
      ~ c("Depends", "Imports", "LinkingTo"),

      identical(field, "most") || identical(field, TRUE)
      ~ c("Depends", "Imports", "LinkingTo", "Suggests"),

      identical(field, "all") ~
        c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"),

      field

    )

  })

  unique(unlist(expanded, recursive = FALSE, use.names = FALSE))

}

renv_description_dependency_fields <- function(fields, project) {
  fields <- fields %||% settings$package.dependency.fields(project = project)
  renv_description_dependency_fields_expand(fields)
}

renv_description_remotes <- function(path) {

  desc <- catch(renv_description_read(path))
  if (inherits(desc, "error"))
    return(list())

  profile <- renv_profile_get()
  field <- if (is.null(profile))
    "Remotes"
  else
    sprintf("Config/renv/profiles/%s/remotes", profile)

  remotes <- desc[[field]]
  if (is.null(remotes))
    return(list())

  splat <- strsplit(remotes, "[[:space:]]*,[[:space:]]*")[[1]]
  resolved <- lapply(splat, renv_remotes_resolve)
  names(resolved) <- extract_chr(resolved, "Package")
  resolved

}



# diagnostics.R --------------------------------------------------------------


#' Print a diagnostics report
#'
#' Print a diagnostics report, summarizing the state of a project using renv.
#' This report can occasionally be useful when diagnosing issues with renv.
#'
#' @inheritParams renv-params
#'
#' @return This function is normally called for its side effects.
#'
#' @export
diagnostics <- function(project = NULL) {

  renv_scope_error_handler()

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  if (renv_file_type(project, symlinks = FALSE) != "directory") {
    fmt <- "project %s is not a directory"
    stopf(fmt, renv_path_pretty(project))
  }

  renv_scope_options(renv.verbose = TRUE)

  reporters <- list(
    renv_diagnostics_session,
    renv_diagnostics_project,
    renv_diagnostics_status,
    renv_diagnostics_packages,
    renv_diagnostics_abi,
    renv_diagnostics_profile,
    renv_diagnostics_settings,
    renv_diagnostics_options,
    renv_diagnostics_envvars,
    renv_diagnostics_path,
    renv_diagnostics_cache
  )

  fmt <- "Diagnostics Report [renv %s]"
  title <- sprintf(fmt, renv_metadata_version_friendly())
  lines <- paste(rep.int("=", nchar(title)), collapse = "")
  writef(c(title, lines, ""))

  for (reporter in reporters) {
    tryCatch(reporter(project), error = renv_error_handler)
    writef()
  }

}

renv_diagnostics_session <- function(project) {
  writef(header("Session Info"))
  renv_scope_options(width = 80)
  print(sessionInfo())
}

renv_diagnostics_project <- function(project) {
  writef(header("Project"))
  writef("Project path: %s", renv_path_pretty(project))
}

renv_diagnostics_status <- function(project) {
  writef(header("Status"))
  status(project = project)
}

renv_diagnostics_packages <- function(project) {

  writef(header("Packages"))

  # collect state of lockfile, library, dependencies
  lockfile <- renv_diagnostics_packages_lockfile(project)
  libstate <- renv_diagnostics_packages_library(project)
  used <- unique(renv_diagnostics_packages_dependencies(project)$Package)

  # collect recursive package dependencies
  recdeps <- renv_package_dependencies(
    packages = used,
    project  = project
  )

  # bundle together
  all <- c(
    names(lockfile$Packages),
    names(libstate$Packages),
    names(recdeps),
    used
  )

  # sort
  all <- csort(unique(all))

  # check which packages are direct, indirect requirements
  deps <- rep.int(NA_character_, length(all))
  names(deps) <- all
  deps[names(recdeps)] <- "indirect"
  deps[used] <- "direct"

  # build libpaths for installed packages
  libpaths <- dirname(map_chr(all, renv_package_find))

  # use short form
  flibpaths <- factor(libpaths, levels = .libPaths())

  # construct integer codes (to be reported in data output)
  libcodes <- as.integer(flibpaths)
  libcodes[!is.na(libcodes)] <- sprintf("[%i]", libcodes[!is.na(libcodes)])

  # add in packages in library
  data <- data_frame(
    Library    = renv_diagnostics_packages_version(libstate, all),
    Source     = renv_diagnostics_packages_sources(libstate, all),
    Lockfile   = renv_diagnostics_packages_version(lockfile, all),
    Source     = renv_diagnostics_packages_sources(lockfile, all),
    Path       = libcodes,
    Dependency = deps
  )

  # we explicitly want to use rownames here
  row.names(data) <- names(deps)

  # print it out
  renv_scope_options(width = 9000)
  print(data, max = 10000)

  # print library codes
  fmt <- "[%s]: %s"
  writef()
  writef(fmt, format(seq_along(levels(flibpaths))), format(levels(flibpaths)))

}

renv_diagnostics_packages_version <- function(lockfile, all) {

  data <- rep.int(NA_character_, length(all))
  names(data) <- all

  formatted <- map_chr(lockfile$Packages, `[[`, "Version")
  data[names(formatted)] <- formatted

  data

}

renv_diagnostics_packages_sources <- function(lockfile, all) {

  data <- rep.int(NA_character_, length(all))
  names(data) <- all

  sources <- map_chr(lockfile$Packages, function(record) {
    record$Repository %||% record$Source %||% "<unknown>"
  })

  data[names(sources)] <- sources
  data

}

renv_diagnostics_packages_lockfile <- function(project) {

  lockpath <- renv_lockfile_path(project = project)
  if (!file.exists(lockpath)) {
    writef("This project has not yet been snapshotted: 'renv.lock' does not exist.")
    return(list())
  }

  renv_lockfile_read(lockpath)

}

renv_diagnostics_packages_library <- function(project) {

  library <- renv_paths_library(project = project)
  if (!file.exists(library)) {
    fmt <- "The project library %s does not exist."
    writef(fmt, renv_path_pretty(library))
  }

  snapshot(project = project, lockfile = NULL, type = "all")

}

renv_diagnostics_packages_dependencies <- function(project) {

  renv_dependencies_impl(
    project,
    errors = "reported",
    dev = TRUE
  )

}

renv_diagnostics_abi <- function(project) {

  writef(header("ABI"))
  tryCatch(
    renv_abi_check(),
    error = function(e) {
      writef(conditionMessage(e))
    }
  )

}

renv_diagnostics_profile <- function(project) {

  writef(header("User Profile"))

  userprofile <- "~/.Rprofile"
  if (!file.exists(userprofile))
    return(writef("[no user profile detected]"))

  deps <- renv_dependencies_impl(
    userprofile,
    errors = "reported",
    dev = TRUE
  )

  if (empty(deps))
    return(writef("[no R packages referenced in user profile"))

  renv_scope_options(width = 200)
  print(deps)

}

renv_diagnostics_settings <- function(project) {
  writef(header("Settings"))
  str(renv_settings_get(project))
}

renv_diagnostics_options <- function(project) {

  writef(header("Options"))

  keys <- c(
    "defaultPackages",
    "download.file.method",
    "download.file.extra",
    "install.packages.compile.from.source",
    "pkgType",
    "repos",
    grep("^renv[.]", names(.Options), value = TRUE)
  )

  vals <- .Options[keys]
  names(vals) <- keys

  str(vals)

}

renv_diagnostics_envvars <- function(project) {

  writef(header("Environment Variables"))

  envvars <- convert(as.list(Sys.getenv()), "character")

  useful <- c(
    "R_LIBS_USER", "R_LIBS_SITE", "R_LIBS",
    "HOME", "LANG", "MAKE",
    grep("^RENV_", names(envvars), value = TRUE)
  )

  matches <- envvars[useful]
  if (empty(matches))
    return(writef("[no renv environment variables available]"))

  names(matches) <- useful
  matches[is.na(matches)] <- "<NA>"
  matches <- matches[order(names(matches))]

  keys <- names(matches)
  vals <- matches
  formatted <- paste(format(keys), vals, sep = " = ")
  writef(formatted)

}

renv_diagnostics_path <- function(project) {
  writef(header("PATH"))
  path <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]]
  writef(paste("-", path))
}

renv_diagnostics_cache <- function(project) {

  writef(header("Cache"))

  fmt <- "There are a total of %s installed in the renv cache."
  cachelist <- renv_cache_list()
  writef(fmt, nplural("package", length(cachelist)))
  writef("Cache path: %s", renv_path_pretty(renv_paths_cache()))

}


# difftime.R -----------------------------------------------------------------


renv_difftime_format <- function(time, digits = 2L) {

  if (is_testing())
    return("XXXX seconds")

  units <- attr(time, "units") %||% ""
  if (units == "secs" && time < 0.1) {
    time  <- time * 1000
    units <- "milliseconds"
  }

  units <- switch(
    units,
    secs  = "seconds",
    mins  = "minutes",
    hours = "hours",
    days  = "days",
    weeks = "weeks",
    units
  )

  elapsed <- format(unclass(signif(time, digits = digits)))
  if (elapsed %in% c("1", "1.0"))
    units <- substring(units, 1L, nchar(units) - 1L)

  paste(elapsed, units)

}

renv_difftime_format_short <- function(time, digits = 2L) {

  if (is_testing())
    return("XXs")

  units <- attr(time, "units") %||% ""
  if (units == "secs" && time < 0.1) {
    time  <- time * 1000
    units <- "ms"
  }

  elapsed <- signif(time, digits = digits)
  if (nchar(elapsed) == 1L)
    elapsed <- paste(elapsed, ".0", sep = "")

  units <- switch(
    attr(time, "units"),
    secs  = "s",
    mins  = "m",
    hours = "h",
    days  = "d",
    weeks = "w",
    units
  )

  paste(elapsed, units, sep = "")

}


# dots.R ---------------------------------------------------------------------


renv_dots_check <- function(...) {

  dots <- list(...)
  parent <- parent.frame()

  # accept 'bioc' as an alias for 'bioconductor'
  bioc <- dots[["bioc"]]
  if (!is.null(bioc) && exists("bioconductor", envir = parent)) {
    if (is.null(parent$bioconductor)) {
      assign("bioconductor", bioc, envir = parent)
      dots[["bioc"]] <- NULL
    }
  }

  # allow 'confirm' as an alias for 'prompt'
  confirm <- dots[["confirm"]]
  if (!is.null(confirm) && exists("prompt", envir = parent)) {
    assign("prompt", confirm, envir = parent)
    dots[["confirm"]] <- NULL
  }

  # check for empty dots
  if (length(dots) == 0)
    return(TRUE)

  call <- sys.call(sys.parent())
  func <- sys.function(sys.parent())
  matched <- match.call(func, call, expand.dots = FALSE)

  dotcall <- format(matched["..."])
  start <- regexpr("(", dotcall, fixed = TRUE)
  end <- nchar(dotcall) - 2L
  args <- substring(dotcall, start, end)
  n <- length(matched[["..."]])

  message <- paste("unused", plural("argument", n), args)
  stop(simpleError(message = message, call = call))

}


# download.R -----------------------------------------------------------------


# download a file from 'url' to file 'destfile'. the 'type'
# argument tells us the remote type, which is used to motivate
# what form of authentication is appropriate; the 'quiet'
# argument is used to display / suppress output. use 'headers'
# (as a named character vector) to supply additional headers
download <- function(url,
                     destfile,
                     preamble = NULL,
                     type = NULL,
                     quiet = FALSE,
                     headers = NULL)
{
  # allow for user-defined overrides
  override <- getOption("renv.download.override")
  if (is.function(override)) {

    result <- catch(
      override(
        url      = url,
        destfile = destfile,
        quiet    = quiet,
        mode     = "wb",
        headers  = headers
      )
    )

    if (inherits(result, "error"))
      renv_download_error(result, "%s", conditionMessage(result))

    if (!file.exists(destfile))
      renv_download_error(url, "%s does not exist", renv_path_pretty(destfile))

    return(destfile)

  }

  if (quiet)
    renv_scope_options(renv.verbose = FALSE)

  # normalize separators (file URIs should normally use forward
  # slashes, even on Windows where the native separator is backslash)
  url      <- chartr("\\", "/", url)
  destfile <- chartr("\\", "/", destfile)

  # notify user we're about to try downloading
  preamble <- preamble %||% sprintf("- Downloading '%s' ... ", url)
  printf(preamble)

  # add custom headers as appropriate for the URL
  custom <- renv_download_custom_headers(url)
  headers[names(custom)] <- custom

  # handle local files by just copying the file
  if (renv_download_local(url, destfile, headers))
    return(destfile)

  # on Windows, try using our local curl binary if available
  renv_scope_downloader()

  # if the file already exists, compare its size with
  # the server's reported size for that file
  info <- renv_file_info(destfile)
  if (identical(info$isdir, FALSE)) {
    size <- renv_download_size(url, type, headers)
    if (info$size == size) {
      writef("OK [file is up to date]")
      return(destfile)
    }
  }

  # back up a pre-existing file if necessary
  callback <- renv_file_backup(destfile)
  defer(callback())

  # form path to temporary file
  tempfile <- renv_scope_tempfile(tmpdir = dirname(destfile))

  # request the download
  before <- Sys.time()

  status <- renv_download_impl(
    url      = url,
    destfile = tempfile,
    type     = type,
    request  = "GET",
    headers  = headers
  )

  after <- Sys.time()

  # check for failure
  if (inherits(status, "condition"))
    renv_download_error(url, "%s", conditionMessage(status))

  if (status != 0L)
    renv_download_error(url, "error code %i", status)

  if (!file.exists(tempfile))
    renv_download_error(url, "%s", "unknown reason")

  # double-check archives are readable
  status <- renv_download_check_archive(tempfile)
  if (inherits(status, "error"))
    renv_download_error(url, "%s", "archive cannot be read")

  # everything looks ok: report success
  elapsed <- difftime(after, before, units = "auto")
  renv_download_report(elapsed, tempfile)

  # move the file to the requested location
  renv_file_move(tempfile, destfile)

  # one final sanity check
  if (!file.exists(destfile)) {
    fmt <- "could not move %s to %s"
    msg <- sprintf(fmt, renv_path_pretty(tempfile), renv_path_pretty(destfile))
    renv_download_error(url, msg)
  }

  # and return path to successfully retrieved file
  destfile
}

# NOTE: only 'GET' and 'HEAD' are supported
#
# each downloader should return 0 on success
renv_download_impl <- function(url, destfile, type = NULL, request = "GET", headers = NULL) {

  # normalize separators (file URIs should normally use forward
  # slashes, even on Windows where the native separator is backslash)
  url      <- chartr("\\", "/", url)
  destfile <- chartr("\\", "/", destfile)

  # check that the destination file is writable
  if (!renv_file_writable(destfile)) {
    fmt <- "destination path '%s' is not writable; cannot proceed"
    stopf(fmt, renv_path_pretty(destfile))
  }

  # select the appropriate downloader
  downloader <- switch(
    renv_download_method(),
    curl = renv_download_curl,
    wget = renv_download_wget,
    renv_download_default
  )

  # run downloader, catching errors and warnings
  catchall(downloader(url, destfile, type, request, headers))

}

renv_download_default_mode <- function(url, method) {

  mode <- "wb"

  fixup <-
    renv_platform_windows() &&
    identical(method, "wininet") &&
    substring(url, 1L, 5L) == "file:"

  if (fixup)
    mode <- "w+b"

  mode

}

renv_download_default <- function(url, destfile, type, request, headers) {

  # custom request types are not supported with the default downloader
  if (request != "GET")
    stopf("the default downloader does not support %s requests", request)

  # try and ensure headers are set for older versions of R
  auth <- renv_download_auth(url, type)
  headers[names(auth)] <- auth
  renv_download_default_agent_scope(headers)

  # on Windows, prefer 'wininet' as most users will have already configured
  # authentication etc. to work with this protocol
  methods <- c(
    Sys.getenv("RENV_DOWNLOAD_METHOD", unset = NA),
    Sys.getenv("RENV_DOWNLOAD_FILE_METHOD", unset = NA),
    if (renv_platform_windows()) "wininet" else "auto"
  )

  method <- Find(Negate(is.na), methods)

  # headers _must_ be NULL rather than zero-length character
  if (length(headers) == 0)
    headers <- NULL

  mode <- renv_download_default_mode(url, method)

  # handle absence of 'headers' argument in older versions of R
  args <- list(url      = url,
               destfile = destfile,
               method   = method,
               headers  = headers,
               mode     = mode,
               quiet    = TRUE)

  fmls <- formals(download.file)
  args <- keep(args, names(fmls))

  renv_download_trace_begin(url, method)
  if (renv_download_trace())
    str(args)

  do.call(download.file, args)

}

renv_download_default_agent_scope <- function(headers, scope = parent.frame()) {

  if (empty(headers))
    return(FALSE)

  if (getRversion() >= "3.6.0")
    return(FALSE)

  renv_download_default_agent_scope_impl(headers, scope)
}

renv_download_default_agent_scope_impl <- function(headers, scope = parent.frame()) {

  utils <- asNamespace("utils")
  makeUserAgent <- utils$makeUserAgent
  ok <-
    is.function(makeUserAgent) &&
    identical(formals(makeUserAgent), pairlist(format = TRUE))

  if (!ok)
    return(FALSE)

  agent <- makeUserAgent(FALSE)
  all <- c("User-Agent" = agent, headers)
  headertext <- paste0(names(all), ": ", all, "\r\n", collapse = "")

  renv_scope_binding(utils, "makeUserAgent", function(format = TRUE) {
    if (format) headertext else agent
  }, scope = scope)

  return(TRUE)

}

renv_download_curl <- function(url, destfile, type, request, headers) {

  renv_download_trace_begin(url, "curl")

  configfile <- renv_scope_tempfile("renv-download-config-")

  fields <- c(
    "user-agent" = renv_http_useragent(),
    "url"        = url,
    "output"     = destfile
  )

  # set connect timeout
  timeout <- config$connect.timeout()
  if (is.numeric(timeout))
    fields[["connect-timeout"]] <- timeout

  # set number of retries
  retries <- config$connect.retry()
  if (is.numeric(retries))
    fields[["retry"]] <- retries

  # set up authentication headers
  auth <- renv_download_auth(url, type)
  if (length(auth)) {
    authtext <- paste(names(auth), auth, sep = ": ")
    names(authtext) <- "header"
    fields <- c(fields, authtext)
  }

  # add other custom headers
  if (length(headers)) {
    lines <- paste(names(headers), headers, sep = ": ")
    names(lines) <- "header"
    fields <- c(fields, lines)
  }

  # join together
  keys <- names(fields)
  vals <- renv_json_quote(fields)
  text <- paste(keys, vals, sep = " = ")

  # add in stand-along flags
  flags <- c("location", "fail", "silent", "show-error")
  if (request == "HEAD")
    flags <- c(flags, "head", "include")

  # put it all together
  text <- c(flags, text)

  writeLines(text, con = configfile)
  renv_download_trace_request(text)

  # generate the arguments to be passed to 'curl'
  args <- stack()

  # include anything provided explicitly in 'download.file.extra' here
  if (identical(getOption("download.file.method"), "curl")) {
    extra <- getOption("download.file.extra")
    if (length(extra))
      args$push(extra)
  }

  # honor R_LIBCURL_SSL_REVOKE_BEST_EFFORT
  # https://github.com/wch/r-source/commit/f1ec503e986593bced6720a5e9099df58a4162e7
  if (Sys.getenv("R_LIBCURL_SSL_REVOKE_BEST_EFFORT") %in% c("T", "t", "TRUE", "true"))
    args$push("--ssl-revoke-best-effort")

  # add in any user configuration files
  userconfig <- getOption(
    "renv.curl.config",
    renv_download_curl_config()
  )

  for (entry in userconfig)
    if (file.exists(entry))
      args$push("--config", renv_shell_path(entry))

  # add in our own config file (the actual request)
  args$push("--config", renv_shell_path(configfile))

  # perform the download
  curl <- renv_curl_exe()
  output <- suppressWarnings(
    system2(curl, args$data(), stdout = TRUE, stderr = TRUE)
  )

  renv_download_trace_result(output)

  # report non-zero status as warning
  status <- attr(output, "status", exact = TRUE) %||% 0L
  if (status != 0L)
    warning(output, call. = FALSE)

  status

}

renv_download_curl_config <- function() {

  rc <- if (renv_platform_windows()) "_curlrc" else ".curlrc"

  homes <- c(
    Sys.getenv("CURL_HOME"),
    Sys.getenv("HOME"),
    Sys.getenv("R_USER"),
    path.expand("~/")
  )

  # nocov start
  if (renv_platform_windows()) {
    extra <- c(
      Sys.getenv("APPDATA"),
      file.path(Sys.getenv("USERPROFILE"), "Application Data"),
      dirname(Sys.which("curl"))
    )
    homes <- c(homes, extra)
  }
  # nocov end

  homes <- Filter(nzchar, homes)

  for (home in homes) {
    path <- file.path(home, rc)
    if (file.exists(path))
      return(path)
  }

  NULL

}

# nocov start

renv_download_wget <- function(url, destfile, type, request, headers) {

  renv_download_trace_begin(url, "wget")

  configfile <- renv_scope_tempfile("renv-download-config-")

  fields <- c(
    "user-agent" = renv_http_useragent(),
    "quiet"      = "on"
  )

  auth <- renv_download_auth(url, type)
  if (length(auth)) {
    authtext <- paste(names(auth), auth, sep = ": ")
    names(authtext) <- "header"
    fields <- c(fields, authtext)
  }

  if (length(headers)) {
    lines <- paste(names(headers), headers, sep = ": ")
    names(lines) <- "header"
    fields <- c(fields, lines)
  }

  keys <- names(fields)
  vals <- unlist(fields)
  text <- paste(keys, vals, sep = " = ")

  writeLines(text, con = configfile)
  renv_download_trace_request(text)

  args <- stack()

  if (identical(getOption("download.file.method"), "wget")) {
    extra <- getOption("download.file.extra")
    if (length(extra))
      args$push(extra)
  }

  args$push("--config", renv_shell_path(configfile))

  # NOTE: '-O' does not write headers to file; we need to manually redirect
  # in that case
  status <- if (request == "HEAD") {
    args$push("--server-response", "--spider")
    args$push(">", renv_shell_path(destfile), "2>&1")
    cmdline <- paste("wget", paste(args$data(), collapse = " "))
    return(suppressWarnings(system(cmdline)))
  }

  args$push("-O", renv_shell_path(destfile))
  args$push(renv_shell_quote(url))

  output <- suppressWarnings(
    system2("wget", args$data(), stdout = TRUE, stderr = TRUE)
  )

  renv_download_trace_result(output)

  status <- attr(output, "status", exact = TRUE) %||% 0L
  if (status != 0L)
    warning(output, call. = FALSE)

  status

}

# nocov end

renv_download_auth_type <- function(url) {

  github_hosts <- c(
    "https://api.github.com/",
    "https://raw.githubusercontent.com/"
  )

  for (host in github_hosts)
    if (startswith(url, host))
      return("github")

  gitlab_hosts <- c(
    "https://gitlab.com/"
  )

  for (host in gitlab_hosts)
    if (startswith(url, host))
      return("gitlab")

  bitbucket_hosts <- c(
    "https://api.bitbucket.org/",
    "https://bitbucket.org/"
  )

  for (host in bitbucket_hosts)
    if (startswith(url, host))
      return("bitbucket")

  "unknown"

}

renv_download_auth <- function(url, type) {

  type <- tolower(type %||% renv_download_auth_type(url))
  switch(
    type,
    bitbucket = renv_download_auth_bitbucket(),
    github = renv_download_auth_github(),
    gitlab = renv_download_auth_gitlab(),
    character()
  )

}

renv_download_auth_bitbucket <- function() {

  user <-
    Sys.getenv("BITBUCKET_USER", unset = NA) %NA%
    Sys.getenv("BITBUCKET_USERNAME", unset = NA)

  pass <-
    Sys.getenv("BITBUCKET_PASS", unset = NA) %NA%
    Sys.getenv("BITBUCKET_PASSWORD", unset = NA)

  if (is.na(user) || is.na(pass))
    return(character())

  userpass <- paste(user, pass, sep = ":")
  c("Authorization" = paste("Basic", renv_base64_encode(userpass)))

}

renv_download_auth_github <- function() {

  pat <- renv_download_auth_github_pat()
  if (is.null(pat))
    return(character())

  c("Authorization" = paste("token", pat))

}

renv_download_auth_github_pat <- function() {

  pat <- Sys.getenv("GITHUB_PAT", unset = NA)
  if (!is.na(pat))
    return(pat)

  token <- tryCatch(gitcreds::gitcreds_get(), error = function(e) NULL)
  if (!is.null(token))
    return(token$password)

}

renv_download_auth_gitlab <- function() {

  pat <- Sys.getenv("GITLAB_PAT", unset = NA)
  if (is.na(pat))
    return(character())

  c("Private-Token" = pat)

}

renv_download_headers <- function(url, type, headers) {

  # check for compatible download method
  method <- renv_download_method()
  if (!method %in% c("libcurl", "curl", "wget"))
    return(list())

  # perform the download
  file <- renv_scope_tempfile("renv-headers-")

  status <- renv_download_impl(
    url      = url,
    destfile = file,
    type     = type,
    request  = "HEAD",
    headers  = headers
  )

  # check for failure
  failed <-
    inherits(status, "error") ||
    !identical(status, 0L) ||
    !file.exists(file)

  if (failed) {
    unlink(file)
    return(list())
  }

  # read the downloaded headers
  contents <- read(file)

  # if redirects were required, each set of headers will
  # be reported separately, so just report the final set
  # of headers (ie: ignore redirects)
  splat <- strsplit(contents, "\n\n", fixed = TRUE)[[1]]
  text <- strsplit(splat[[length(splat)]], "\n", fixed = TRUE)[[1]]

  # keep only header lines
  lines <- grep(":", text, fixed = TRUE, value = TRUE)
  headers <- catch(renv_properties_read(text = lines))
  names(headers) <- tolower(names(headers))
  if (inherits(headers, "error"))
    return(list())

  headers

}

renv_download_size <- function(url, type = NULL, headers = NULL) {

  memoize(
    key   = url,
    value = renv_download_size_impl(url, type, headers)
  )

}

renv_download_size_impl <- function(url, type = NULL, headers = NULL) {

  headers <- catch(renv_download_headers(url, type, headers))
  if (inherits(headers, "error"))
    return(-1L)

  size <- headers[["x-gitlab-size"]]
  if (!is.null(size))
    return(as.numeric(size))

  size <- headers[["content-length"]]
  if (!is.null(size))
    return(as.numeric(size))

  return(-1L)

}

# select an appropriate download file method. we prefer curl
# when available as it's the most user-customizable of all the
# download methods; when not available, we fall back to libcurl
# and wget (in that order). note that we don't want to use the
# internal or wininet downloaders as we cannot set custom headers
# with those methods. users can force a method with the
# RENV_DOWNLOAD_FILE_METHOD environment variable but we generally
# want to override a user-specified 'download.file.method'
renv_download_method <- function() {

  method <- Sys.getenv("RENV_DOWNLOAD_METHOD", unset = NA)
  if (!is.na(method))
    return(method)

  method <- Sys.getenv("RENV_DOWNLOAD_FILE_METHOD", unset = NA)
  if (!is.na(method))
    return(method)

  # prefer curl if available
  if (nzchar(Sys.which("curl")))
    return("curl")

  # if curl is not available, use libcurl if available
  libcurl <- capabilities("libcurl")
  if (length(libcurl) && libcurl)
    return("libcurl")

  # on windows, just use wininet here
  if (renv_platform_windows())
    return("wininet")

  # if neither curl nor libcurl is available, prefer wget
  if (nzchar(Sys.which("wget")))
    return("wget")

  # all else fails, use the internal downloader
  "internal"

}

renv_download_report <- function(elapsed, file) {

  if (!renv_verbose())
    return()

  info <- renv_file_info(file)
  size <- if (is_testing())
    "XXXX bytes"
  else
    structure(info$size, class = "object_size")

  renv_report_ok(
    message = format(size, units = "auto"),
    elapsed = elapsed
  )

}

renv_download_check_archive <- function(destfile) {

  # validate the file exists
  if (!file.exists(destfile))
    return(FALSE)

  # validate archive type
  type <- renv_archive_type(destfile)
  if (type == "unknown")
    return(FALSE)

  # try listing files in the archive
  tryCatch({renv_archive_list(destfile); TRUE}, error = identity)

}

renv_download_local <- function(url, destfile, headers) {

  # only ever used for downloads from file URIs and server URIs
  ok <-
    grepl("^file:", url) ||
    !grepl("^[a-zA-Z]+://", url)

  if (!ok)
    return(FALSE)

  methods <- list(
    renv_download_local_copy,
    renv_download_local_default
  )

  for (method in methods) {

    # perform the copy
    before <- Sys.time()
    status <- catch(method(url, destfile, headers))
    after  <- Sys.time()

    # check for success
    if (!identical(status, TRUE))
      next

    # report download summary
    elapsed <- difftime(after, before, units = "auto")
    renv_download_report(elapsed, destfile)

    return(TRUE)

  }

  FALSE

}

renv_download_local_copy <- function(url, destfile, headers) {

  # remove file prefix (to get path to local / server file)
  url <- case(
    startswith(url, "file:///") ~ substring(url, 8L),
    startswith(url, "file://")  ~ substring(url, 6L),
    startswith(url, "file:")    ~ substring(url, 6L),
    TRUE                        ~ url
  )

  # fix up file URIs to local paths on Windows
  if (renv_platform_windows()) {
    badpath <- grepl("^/[a-zA-Z]:", url)
    if (badpath)
      url <- substring(url, 2L)
  }

  # attempt to copy
  ensure_parent_directory(destfile)
  status <- catchall(renv_file_copy(url, destfile, overwrite = TRUE))
  if (!identical(status, TRUE))
    return(FALSE)

  TRUE

}

renv_download_local_default <- function(url, destfile, headers) {

  status <- renv_download_impl(
    url      = url,
    destfile = destfile,
    headers  = headers
  )

  identical(status, 0L)

}

renv_download_custom_headers <- function(url) {
  renv_bootstrap_download_custom_headers(url)
}

renv_download_available <- function(url) {

  # normalize separators (file URIs should normally use forward
  # slashes, even on Windows where the native separator is backslash)
  url <- chartr("\\", "/", url)

  # on Windows, try using our local curl binary if available
  renv_scope_downloader()

  # if we're not using curl, then use fallback method
  method <- renv_download_method()
  if (!identical(method, "curl"))
    return(renv_download_available_fallback(url))

  # otherwise, try a couple candidate methods
  methods <- list(
    renv_download_available_headers,
    renv_download_available_range
  )

  for (method in methods) {
    result <- catch(method(url))
    if (identical(result, TRUE))
      return(TRUE)
  }

  FALSE

}

renv_download_available_headers <- function(url) {

  status <- catchall(
    renv_download_headers(
      url     = url,
      type    = NULL,
      headers = renv_download_custom_headers(url)
    )
  )

  if (inherits(status, "condition"))
    return(FALSE)

  is.list(status) && length(status)

}

renv_download_available_range <- function(url) {

  destfile <- renv_scope_tempfile("renv-download-")

  # instruct curl to request only first byte
  extra <- c(
    if (identical(getOption("download.file.method"), "curl"))
      getOption("download.file.extra"),
    "-r 0-0"
  )

  renv_scope_options(download.file.extra = paste(extra, collapse = " "))

  # perform the download
  status <- catchall(
    renv_download_curl(
      url      = url,
      destfile = destfile,
      type     = NULL,
      request  = "GET",
      headers  = renv_download_custom_headers(url)
    )
  )

  if (inherits(status, "condition"))
    return(FALSE)

  # check for success
  identical(status, 0L)

}

renv_download_available_fallback <- function(url) {

  destfile <- renv_scope_tempfile("renv-download-")

  # just try downloading the requested URL
  status <- catchall(
    renv_download_impl(
      url      = url,
      destfile = destfile,
      type     = NULL,
      request  = "GET",
      headers  = renv_download_custom_headers(url)
    )
  )

  if (inherits(status, "condition"))
    return(FALSE)

  identical(status, 0L)

}

renv_download_error <- function(url, fmt, ...) {
  msg <- sprintf(fmt, ...)
  writef("\tERROR [%s]", msg)
  stopf("error downloading '%s' [%s]", url, msg, call. = FALSE)
}

renv_download_trace <- function() {
  getOption("renv.download.trace", default = FALSE)
}

renv_download_trace_begin <- function(url, type) {

  if (!renv_download_trace())
    return()

  fmt <- "Downloading '%s' [%s]"
  msg <- sprintf(fmt, url, type)

  title <- header(msg, n = 78L)
  writef(c("", title, ""))

}

renv_download_trace_request <- function(text) {

  if (!renv_download_trace())
    return()

  title <- header("Request", n = 78L, prefix = "##")
  writef(c(title, text, ""))

}

renv_download_trace_result <- function(output) {

  if (!renv_download_trace())
    return()

  title <- header("Output", prefix = "##", n = 78L)
  text <- if (empty(output)) "[no output generated]" else output
  all <- c(title, text, "")
  writef(all)

  status <- attr(output, "status", exact = TRUE) %||% 0L
  title <- header("Status", prefix = "##", n = 78L)
  all <- c(title, status, "")
  writef(all)

}


# dynamic.R ------------------------------------------------------------------


#
# Tools for so-called 'dynamic' values. These are values which are computed
# once, and then memoized for the rest of the currently-executing call.
#
# An exit handler placed in the top-most (renv) environment is then responsible
# for cleaning up any objects cached for the duration of that frame.
#
# This is a useful way to cache results for repeatedly-computed values
# that one can reasonably expect not to change in the duration of a
# particular call.
#

the$dynamic_envir <- NULL
the$dynamic_objects <- new.env(parent = emptyenv())

dynamic <- function(key, value, envir = NULL) {

  # allow opt-out just in case
  enabled <- getOption("renv.dynamic.enabled", default = TRUE)
  if (!enabled)
    return(value)

  # get a unique id for the scope where this function was invoked
  caller <- sys.call(sys.parent())[[1L]]
  if (renv_call_matches(caller, name = ":::"))
    caller <- caller[[3L]]

  # handle cases like FUN
  if (is.null(the$envir_self[[as.character(caller)]])) {
    if (!renv_tests_running()) {
      fmt <- "internal error: dynamic() received unexpected call '%s'"
      stopf(fmt, stringify(sys.call(sys.parent())))
    }
  }

  # just return value if this isn't a valid dynamic scope
  if (!is.symbol(caller)) {
    dlog("dynamic", "invalid dynamic scope '%s'", stringify(sys.call(sys.parent())))
    return(value)
  }

  # make sure we have a dynamic scope active
  the$dynamic_envir <- the$dynamic_envir %||% renv_dynamic_envir(envir)

  # resolve key from variables in the parent frame
  key <- paste(
    names(key),
    map_chr(key, stringify),
    sep = " = ",
    collapse = ", "
  )

  # put it together
  id <- sprintf("%s(%s)", as.character(caller), key)

  # memoize the result of the expression
  the$dynamic_objects[[id]] <- the$dynamic_objects[[id]] %||% {
    dlog("dynamic", "memoizing dynamic value for '%s'", id)
    value
  }

}

renv_dynamic_envir <- function(envir = NULL) {

  envir <- envir %||% renv_dynamic_envir_impl()
  defer(renv_dynamic_reset(), scope = envir)

  dlog("dynamic", "using dynamic environment '%s'", format(envir))
  envir
}

renv_dynamic_envir_impl <- function() {

  for (envir in sys.frames())
    if (identical(parent.env(envir), the$envir_self))
      return(envir)

  stop("internal error: no renv frame available for dynamic call")

}

renv_dynamic_reset <- function() {
  dlog("dynamic", "resetting dynamic objects")
  the$dynamic_envir <- NULL
  renv_envir_clear(the$dynamic_objects)
}


# embed.R --------------------------------------------------------------------

#' Capture and re-use dependencies within a `.R` or `.Rmd`
#'
#' @description
#' Together, `embed()` and `use()` provide a lightweight way to specify and
#' restore package versions within a file. `use()` is a lightweight lockfile
#' specification that `embed()` can automatically generate and insert into a
#' script or document.
#'
#' Calling `embed()` inspects the dependencies of the specified document then
#' generates and inserts a call to `use()` that looks something like this:
#'
#' ```R
#' renv::use(
#'   "digest@0.6.30",
#'   "rlang@0.3.4"
#' )
#' ```
#'
#' Then, when you next run your R script or render your `.Rmd`, `use()` will:
#'
#' 1. Create a temporary library path.
#'
#' 1. Install the requested packages and their recursive dependencies into that
#'    library.
#'
#' 1. Activate the library, so it's used for the rest of the script.
#'
#' ## Manual usage
#'
#' You can also create calls to `use()` yourself, either specifying the
#' packages needed by hand, or by supplying the path to a lockfile,
#' `renv::use(lockfile = "/path/to/renv.lock")`.
#'
#' This can be useful in projects where you'd like to associate different
#' lockfiles with different documents, as in a blog where you want each
#' post to capture the dependencies at the time of writing. Once you've
#' finished writing each, the post, you can use
#' `renv::snapshot(lockfile = "/path/to/renv.lock")`
#' to "save" the state that was active while authoring that bost, and then use
#' `renv::use(lockfile = "/path/to/renv.lock")` in that document to ensure the
#' blog post always uses those dependencies onfuture renders.
#'
#' `renv::use()` is inspired in part by the [groundhog](https://groundhogr.com/)
#' package, which also allows one to specify a script's \R package requirements
#' within that same \R script.
#'
#' @inherit renv-params
#'
#' @param path
#'   The path to an \R or R Markdown script. The default will use the current
#'   document, if running within RStudio.
#'
#' @param lockfile
#'   The path to an renv lockfile. When `NULL` (the default), the project
#'   lockfile will be read (if any); otherwise, a new lockfile will be generated
#'   from the current library paths.
#'
#' @export
embed <- function(path = NULL,
                  ...,
                  lockfile = NULL,
                  project = NULL)
{
  path <- path %||% renv_embed_path()

  ext <- tolower(fileext(path))
  method <- case(
    ext == ".r"   ~ renv_embed_r,
    ext == ".rmd" ~ renv_embed_rmd
  )

  if (is.null(method)) {
    fmt <- "don't know how to embed lockfile into file %s"
    stopf(fmt, renv_path_pretty(path))
  }

  method(
    path     = path,
    lockfile = lockfile,
    project  = project,
    ...
  )

}

renv_embed_path <- function() {

  tryCatch(
    renv_embed_path_impl(),
    error = function(e) NULL
  )

}

renv_embed_path_impl <- function() {
  rstudio <- as.environment("tools:rstudio")
  rstudio$.rs.api.documentPath()
}

renv_embed_create <- function(path = NULL,
                              lockfile = NULL,
                              project = NULL)
{
  # generate lockfile
  project <- renv_project_resolve(project)
  lockfile <- renv_embed_lockfile_resolve(lockfile, project)

  # figure out recursive package dependencies
  deps <- renv_dependencies_impl(path)
  packages <- sort(unique(deps$Package))
  all <- renv_package_dependencies(packages)

  # keep only matched records
  lockfile$Packages <- keep(lockfile$Packages, c("renv", names(all)))

  # write compact use statement
  renv_lockfile_compact(lockfile)
}

renv_embed_r <- function(path, ..., lockfile = NULL, project = NULL) {

  # resolve project
  project <- renv_project_resolve(project)

  # read file contents
  contents <- readLines(path, warn = FALSE, encoding = "UTF-8")

  # generate embed
  embed <- renv_embed_create(
    path     = path,
    lockfile = lockfile,
    project  = project
  )

  # check for existing 'renv::use' statement
  pattern <- "^\\s*(?:renv:{2,3})?use\\(\\s*$"
  index <- grep(pattern, contents, perl = TRUE)

  # if we don't have an index, just insert at start
  if (empty(index)) {
    contents <- c(embed, "", contents)
    writeLines(contents, con = path)
    return(TRUE)
  }

  # otherwise, try to replace an existing embedded lockfile
  start <- index

  # find the end of the block
  n <- length(contents)
  lines <- grep("^\\s*\\)\\s*$", contents, perl = TRUE)
  end <- min(lines[lines > start], n + 1L)

  # inject new lockfile
  contents <- c(
    head(contents, n = start - 1L),
    embed,
    tail(contents, n = n - end)
  )

  writeLines(contents, con = path)
  return(TRUE)

}

renv_embed_create_rmd <- function(path = NULL,
                                  lockfile = NULL,
                                  project = NULL)
{
  # create lockfile
  project  <- renv_project_resolve(project)
  lockfile <- renv_embed_lockfile_resolve(lockfile, project)

  # create embed
  embed <- renv_embed_create(
    path     = path,
    lockfile = lockfile,
    project  = project
  )

  # return embed
  c("```{r lockfile, include=FALSE}", embed, "```")

}


renv_embed_rmd <- function(path,
                           ...,
                           lockfile = NULL,
                           project = NULL)
{
  # resolve project
  project <- renv_project_resolve(project)

  # read file contents
  contents <- readLines(path, warn = FALSE, encoding = "UTF-8")

  # generate embed
  embed <- renv_embed_create_rmd(
    path     = path,
    lockfile = lockfile,
    project  = project
  )

  # check for existing renv.lock in file
  # if it exists, we'll want to replace at this location;
  # otherwise, insert at end of document
  header <- "^\\s*```{r lockfile"
  footer <- "```"
  start <- grep(header, contents, perl = TRUE)

  # if we don't have an index, insert after YAML header (if any)
  if (empty(start)) {
    bounds <- which(trimws(contents) == "---")

    all <- if (length(bounds) >= 2) {
      index <- bounds[[2L]]
      c(
        head(contents, n = index),
        "",
        embed,
        "",
        tail(contents, n = length(contents) - index)
      )
    } else {
      c(embed, "", contents)
    }

    writeLines(all, con = path)
    return(TRUE)
  }

  # otherwise, try to replace an existing embedded lockfile
  ends <- which(contents == footer)
  end <- min(ends[ends > start])

  # inject new lockfile
  contents <- c(
    head(contents, n = start - 1L),
    embed,
    tail(contents, n = length(contents) - end)
  )

  writeLines(contents, con = path)
  return(TRUE)

}

renv_embed_lockfile_resolve <- function(lockfile, project) {

  # if lockfile is character, assume it's the path to a lockfile
  if (is.character(lockfile))
    return(renv_lockfile_read(lockfile))

  # if lockfile is not NULL, assume lockfile object
  if (!is.null(lockfile))
    return(lockfile)

  # check for lockfile in project
  path <- renv_lockfile_path(project)
  if (file.exists(path))
    return(renv_lockfile_read(path))

  # no lockfile available; just snapshot
  snapshot(project = project, lockfile = NULL)

}


# encoding.R -----------------------------------------------------------------


renv_encoding_mark <- function(x, encoding = "UTF-8") {
  Encoding(x) <- "UTF-8"
  x
}


# ensure.R -------------------------------------------------------------------


ensure_existing_path <- function(path) {
  if (!file.exists(path))
    stopf("no file at path '%s'", path)
  invisible(path)
}

ensure_existing_file <- function(path) {
  info <- renv_file_info(path)
  if (is.na(info$isdir))
    stopf("no file at path '%s'", path)
  else if (identical(info$isdir, TRUE))
    stopf("file '%s' exists but is a directory")
  invisible(path)
}

ensure_directory <- function(paths, umask = NULL) {

  # handle zero-path case
  if (empty(paths))
    return(invisible(paths))

  # set umask if necessary
  if (!is.null(umask))
    renv_scope_umask("0")

  # for each path, try to either create the directory, or assert that
  # the directory already exists. this should also help handle cases
  # where 'dir.create()' fails because another process created the
  # directory at the same time we attempted to do so
  for (path in paths) {

    ok <-
      dir.create(path, recursive = TRUE, showWarnings = FALSE) ||
      dir.exists(path)

    if (!ok)
      stopf("failed to create directory at path '%s'", path)

  }

  # return the paths
  invisible(paths)

}

ensure_parent_directory <- function(path) {
  ensure_directory(unique(dirname(path)))
}



# envir.R --------------------------------------------------------------------


renv_envir_self <- function() {
  parent.env(environment())
}

renv_envir_clear <- function(envir) {
  vars <- ls(envir = envir, all.names = TRUE)
  rm(list = vars, envir = envir, inherits = FALSE)
}

renv_envir_unwrap <- function(envir) {
  eapply(envir, function(node) {
    if (is.environment(node))
      renv_envir_unwrap(node)
    else
      node
  })
}


# envvar.R -------------------------------------------------------------------


renv_envvar_path_add <- function(envvar, value, prepend = TRUE) {

  old <- Sys.getenv(envvar, unset = "")
  old <- strsplit(old, .Platform$path.sep)[[1]]

  parts <- if (prepend) union(value, old) else union(old, value)
  new <- paste(parts, collapse = .Platform$path.sep)

  names(new) <- envvar
  do.call(Sys.setenv, as.list(new))

  new

}

renv_envvar_exists <- function(key) {
  !is.na(Sys.getenv(key, unset = NA))
}


# envvars.R ------------------------------------------------------------------


renv_envvars_list <- function() {
  c(
    "R_PROFILE", "R_PROFILE_USER",
    "R_ENVIRON", "R_ENVIRON_USER",
    "R_LIBS_USER", "R_LIBS_SITE", "R_LIBS"
  )
}

renv_envvars_save <- function() {

  # save the common set of environment variables
  keys <- renv_envvars_list()
  vals <- Sys.getenv(keys, unset = "<NA>")

  # check for defaults that have already been set
  defkeys <- paste("RENV_DEFAULT", keys, sep = "_")
  defvals <- Sys.getenv(defkeys, unset = NA)
  if (any(!is.na(defvals)))
    return(FALSE)

  # prepare defaults
  env <- vals
  names(env) <- defkeys
  do.call(Sys.setenv, as.list(env))

  TRUE

}

renv_envvars_restore <- function() {

  # read defaults
  keys <- renv_envvars_list()
  defkeys <- paste("RENV_DEFAULT", renv_envvars_list(), sep = "_")
  defvals <- Sys.getenv(defkeys, unset = "<NA>")

  # remove previously-unset environment variables
  missing <- defvals == "<NA>"
  Sys.unsetenv(keys[missing])

  # restore old values for envvars
  existing <- as.list(defvals[!missing])
  if (length(existing)) {
    names(existing) <- sub("^RENV_DEFAULT_", "", names(existing))
    do.call(Sys.setenv, existing)
  }

  # remove saved RENV_DEFAULT values
  Sys.unsetenv(defkeys)
  TRUE

}

renv_envvars_init <- function() {
  renv_envvars_normalize()
}

renv_envvars_normalize <- function() {

  Sys.setenv(R_LIBS_SITE = .expand_R_libs_env_var(Sys.getenv("R_LIBS_SITE")))
  Sys.setenv(R_LIBS_USER = .expand_R_libs_env_var(Sys.getenv("R_LIBS_USER")))

  keys <- c(
    "RENV_PATHS_ROOT",
    "RENV_PATHS_LIBRARY",
    "RENV_PATHS_LIBRARY_ROOT",
    "RENV_PATHS_LIBRARY_STAGING",
    "RENV_PATHS_LOCAL",
    "RENV_PATHS_CELLAR",
    "RENV_PATHS_SOURCE",
    "RENV_PATHS_BINARY",
    "RENV_PATHS_CACHE",
    "RENV_PATHS_RTOOLS",
    "RENV_PATHS_EXTSOFT",
    "RENV_PATHS_MRAN"
  )

  envvars <- as.list(keep(Sys.getenv(), keys))
  if (empty(envvars))
    return()

  args <- lapply(envvars, renv_path_normalize)
  do.call(Sys.setenv, args)

}


# equip-macos.R --------------------------------------------------------------


renv_equip_macos_specs <- function() {

  list(

    "4.0" = list(
      url = "https://cran.r-project.org/bin/macosx/tools/clang-8.0.0.pkg",
      dst = "/usr/local/clang8"
    ),

    "3.7" = list(
      url = "https://cran.r-project.org/bin/macosx/tools/clang-8.0.0.pkg",
      dst = "/usr/local/clang8"
    ),

    "3.6" = list(
      url = "https://cran.r-project.org/bin/macosx/tools/clang-7.0.0.pkg",
      dst = "/usr/local/clang7"
    ),

    "3.5" = list(
      url = "https://cran.r-project.org/bin/macosx/tools/clang-6.0.0.pkg",
      dst = "/usr/local/clang6"
    )

  )

}

renv_equip_macos_spec <- function(version = getRversion()) {
  renv_equip_macos_specs()[[renv_version_maj_min(version)]]
}

renv_equip_macos <- function() {

  renv_equip_macos_sdk()
  renv_equip_macos_toolchain()

}

renv_equip_macos_sdk <- function() {

  sdk <- "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk"
  if (file.exists(sdk) || file.exists("/usr/include"))
    return(TRUE)

  system("/usr/bin/xcode-select --install")

  # give the user some time to respond to the dialog)
  Sys.sleep(5)

}

renv_equip_macos_toolchain <- function() {

  if (getRversion() >= "4.1.0")
    return()

  spec <- renv_equip_macos_spec()
  if (is.null(spec)) {
    fmt <- "no known toolchain recorded in renv for R %s"
    warningf(fmt, getRversion())
    return(FALSE)
  }

  url <- spec$url
  dst <- spec$dst

  clang <- file.path(dst, "bin/clang")
  if (file.exists(clang)) {
    fmt <- "- LLVM toolchain for R %s is already installed at %s."
    writef(fmt, getRversion(), shQuote(dst))
    return(TRUE)
  }

  destfile <- file.path(tempdir(), basename(url))
  download(url, destfile = destfile)

  if (renv_equip_macos_rstudio(spec, destfile))
    return(TRUE)

  command <- paste("sudo /usr/sbin/installer -pkg", shQuote(destfile), "-target /")
  caution_bullets(
    "The R LLVM toolchain has been successfully downloaded. Please execute:",
    command,
    "in a separate terminal to complete installation."
  )

  TRUE

}

renv_equip_macos_rstudio <- function(spec, destfile) {

  rstudio <-
    renv_rstudio_available() &&
    requireNamespace("rstudioapi", quietly = TRUE)

  if (!rstudio)
    return(FALSE)

  command <- paste("sudo -kS /usr/sbin/installer -pkg", shQuote(destfile), "-target /")
  prompt <- paste(
    "Installation of the R LLVM toolchain requires sudo.",
    "Please enter your account password.",
    sep = "\n"
  )

  installed <- local({

    password <- rstudioapi::askForPassword(prompt)
    if (is.null(password))
      return(FALSE)

    status <- system(command, input = password)
    if (status != 0L)
      return(FALSE)

    TRUE

  })

  if (!installed)
    return(FALSE)

  caution_bullets(
    "The R LLVM toolchain has been downloaded and installed to:",
    spec$dst,
    "This toolchain will be used by renv when installing packages from source."
  )

  return(TRUE)

}


# equip.R --------------------------------------------------------------------


#' Install required system libraries
#'
#' Equip your system with libraries commonly-used during compilation of
#' base and recommended \R packages. This was previously useful with older
#' versions of R on windows, but is no longer terribly helpful.
#'
#' @return This function is normally called for its side effects.
#' @export
#' @keywords internal
#' @examples
#' \dontrun{
#'
#' # download useful build tools
#' renv::equip()
#'
#' }
equip <- function() {

  renv_scope_error_handler()

  case(
    renv_platform_windows() ~ renv_equip_windows(),
    renv_platform_macos()   ~ renv_equip_macos(),
    renv_platform_linux()   ~ renv_equip_linux()
  )

  invisible(NULL)

}

renv_equip_windows <- function() {
  invisible(renv_extsoft_install() && renv_extsoft_use())
}

renv_equip_linux <- function() {
  stopf("renv::equip() not yet implemented for Linux")
}


# errors.R -------------------------------------------------------------------


renv_error_format_srcref <- function(call, srcref) {

  srcfile <- attr(srcref, "srcfile", exact = TRUE)

  if (inherits(srcfile, c("srcfilecopy", "srcfilealias"))) {
    start <- srcref[7L]
    end   <- srcref[8L]
  } else {
    start <- srcref[1L]
    end   <- srcref[3L]
  }

  srclines <- getSrcLines(srcfile, start, end)
  index <- regexpr("[^[:space:]]", srclines)
  indent <- min(index)
  code <- substring(srclines, indent)

  if (length(code) >= 8L) {
    simplified <- renv_error_simplify(call)
    if (!identical(simplified, call))
      code <- format(simplified)
  }

  n <- length(code)
  postfix <- sprintf("at %s#%i", basename(srcfile$filename), srcref[1L])
  code[n] <- paste(code[n], postfix)

  code

}

renv_error_simplify <- function(object) {

  case(
    is.function(object)  ~ renv_error_simplify_function(object),
    is.recursive(object) ~ renv_error_simplify_recursive(object),
    TRUE                 ~ object
  )

}

renv_error_simplify_function <- function(object) {
  f <- function() {}
  formals(f) <- formals(object)
  body(f) <- quote({ ... })
  f
}

renv_error_simplify_recursive <- function(object) {

  longcall <- renv_call_matches(object, name = "{") && length(object) >= 8

  if (longcall)
    return(quote(...))

  for (i in seq_along(object))
    if (!is.null(object[[i]]))
      object[[i]] <- renv_error_simplify(object[[i]])

  object

}

renv_error_format <- function(calls, frames) {

  # first, format calls
  formatted <- lapply(calls, function(call) {

    srcref <- attr(call, "srcref", exact = TRUE)
    if (!is.null(srcref)) {
      formatted <- catch(renv_error_format_srcref(call, srcref))
      if (!inherits(formatted, "error"))
        return(formatted)
    }

    if (is.function(call[[1]]))
      return("<condition-handler>(...)")

    format(renv_error_simplify(call))

  })

  # compute prefixes
  numbers <- format(seq_along(formatted))
  prefixes <- sprintf("%s: ", rev(numbers))

  # generate indent
  indent <- paste(rep.int(" ", min(nchar(prefixes))), collapse = "")

  # attach prefixes + indent
  annotated <- uapply(seq_along(formatted), function(i) {
    code <- formatted[[i]]
    prefix <- c(prefixes[[i]], rep.int(indent, length(code) - 1L))
    paste(prefix, code, sep = "")
  })

  header <- "Traceback (most recent calls last):"
  c(header, annotated)

}

renv_error_find <- function(calls, frames) {

  for (i in rev(seq_along(frames))) {

    fn <- sys.function(which = i)
    if (!identical(fn, stop))
      next

    frame <- frames[[i]]
    args <- frame[["args"]]
    if (is.null(args) || empty(args))
      next

    first <- args[[1L]]
    if (!inherits(first, "condition"))
      next

    return(first)

  }

}

renv_error_handler <- function(...) {

  calls <- head(sys.calls(), n = -1L)
  frames <- head(sys.frames(), n = -1L)

  error <- renv_error_find(calls, frames)
  if (identical(error$traceback, FALSE))
    return(character())

  formatted <- renv_error_format(calls, frames)
  caution(formatted)

  formatted

}

the$traceback <- NULL

renv_error_capture <- function(e) {
  calls <- head(sys.calls(), n = -2L)
  frames <- head(sys.frames(), n = -2L)
  traceback <- renv_error_format(calls, frames)
  the$traceback <- traceback
}

renv_error_tag <- function(e) {
  e$traceback <- the$traceback
  e
}

renv_error_handler_call <- function() {
  as.call(list(renv_error_handler))
}


# extsoft.R ------------------------------------------------------------------


renv_extsoft_curl_version <- function() {
  Sys.getenv("RENV_EXTSOFT_CURL_VERSION", unset = "7.77.0")
}

renv_extsoft_install <- function(quiet = FALSE) {

  extsoft <- renv_paths_extsoft()

  ensure_directory(extsoft)
  ensure_directory(file.path(extsoft, "lib/i386"))
  ensure_directory(file.path(extsoft, "lib/x64"))

  root <- "https://s3.amazonaws.com/rstudio-buildtools/extsoft"

  files <- c(
    sprintf("curl-%s-win32-mingw.zip", renv_extsoft_curl_version()),
    "glpk32.zip",
    "glpk64.zip",
    "local323.zip",
    "nlopt-2.4.2.zip",
    "spatial324.zip"
  )

  # check for missing installs
  files <- Filter(renv_extsoft_install_required, files)
  if (empty(files)) {
    if (!quiet) writef("- External software is up to date.")
    return(TRUE)
  }

  if (interactive()) {

    caution_bullets(
      "The following external software tools will be installed:",
      files,
      sprintf("Tools will be installed into %s.", renv_path_pretty(extsoft))
    )

    cancel_if(!proceed())

  }

  for (file in files) {

    # download the file
    url <- file.path(root, file)
    destfile <- renv_scope_tempfile("renv-archive-", fileext = ".zip")
    download(url, destfile = destfile, quiet = quiet)

    # write manifest
    manifest <- renv_extsoft_manifest_path(file)
    ensure_parent_directory(manifest)

    before <- list.files(extsoft, recursive = TRUE)

    # unpack archive
    if (file == "glpk32.zip") {

      unzip(destfile, files = "include/glpk.h", exdir = extsoft)
      unzip(destfile, exdir = file.path(extsoft, "lib/i386"), junkpaths = TRUE)

    } else if (file == "glpk64.zip") {

      unzip(destfile, files = "include/glpk.h", exdir = extsoft)
      unzip(destfile, exdir = file.path(extsoft, "lib/x64"), junkpaths = TRUE)

    } else if (file == "nlopt-2.4.2.zip") {

      unzip(destfile, exdir = extsoft)

      file.copy(file.path(extsoft, "nlopt-2.4.2/include"), extsoft, recursive = TRUE)
      file.copy(file.path(extsoft, "nlopt-2.4.2/lib"), extsoft, recursive = TRUE)
      unlink(file.path(extsoft, "nlopt-2.4.2"), recursive = TRUE)


    } else {

      unzip(destfile, exdir = extsoft)

    }

    after <- list.files(extsoft, recursive = TRUE)
    writeLines(setdiff(after, before), con = manifest)

  }

  writef("- External software successfully updated.")
  TRUE

}

renv_extsoft_install_required <- function(file) {

  manifest <- renv_extsoft_manifest_path(file)
  if (!file.exists(manifest))
    return(TRUE)

  files <- catch(readLines(manifest, warn = FALSE))
  if (inherits(files, "error"))
    return(FALSE)

  paths <- renv_paths_extsoft(files)
  !all(file.exists(paths))

}

renv_extsoft_use <- function(quiet = FALSE) {

  extsoft <- renv_paths_extsoft()
  path <- "~/.R/Makevars"

  ensure_parent_directory(path)
  original <- if (file.exists(path))
    readLines(path, warn = FALSE)
  else
    character()

  contents <- original

  localsoft <- paste("LOCAL_SOFT", extsoft, sep = " = ")
  contents <- inject(contents, "^#?LOCAL_SOFT", localsoft)

  localcpp <- "LOCAL_CPPFLAGS = -I\"$(LOCAL_SOFT)/include\""
  contents <- inject(contents, "^#?LOCAL_CPPFLAGS", localcpp)

  locallibs <- "LOCAL_LIBS = -L\"$(LOCAL_SOFT)/lib$(R_ARCH)\" -L\"$(LOCAL_SOFT)/lib\""
  contents <- inject(contents, "^#?LOCAL_LIBS", locallibs)

  libxml <- paste("LIB_XML", extsoft, sep = " = ")
  contents <- inject(contents, "^#?LIB_XML", libxml)

  if (identical(original, contents))
    return(TRUE)

  if (interactive()) {

    caution_bullets(
      "The following entries will be added to ~/.R/Makevars:",
      c(localsoft, libxml, localcpp, locallibs),
      "These tools will be used when compiling R packages from source."
    )

    cancel_if(!proceed())

  }

  if (!quiet) writef("- '%s' has been updated.", path)
  writeLines(contents, con = path)
  TRUE

}

renv_extsoft_manifest_path <- function(file) {
  name <- paste(file, "manifest", sep = ".")
  renv_paths_extsoft("manifests", name)
}


# filebacked.R ---------------------------------------------------------------


# tools for caching values read from a file, and invalidating those values if
# the file mtime changes. use `renv_filebacked_set()` to associate some value
# with a file at a particular point in time; `renv_filebacked_get()` will return
# that value, or NULL of the file mtime has changed
the$filebacked_cache <- new.env(parent = emptyenv())

renv_filebacked_clear <- function(context, path = NULL) {

  # get cache associated with this context
  envir <- renv_filebacked_envir(context)

  # list all available cached results
  existing <- ls(envir = envir, all.names = TRUE)

  # if path is set, use it; otherwise remove everything
  path <- path %||% existing

  # validate the requested paths exist in the environment
  removable <- renv_vector_intersect(path, existing)

  # remove them
  rm(list = removable, envir = envir)
}

renv_filebacked_set <- function(context, path, value) {

  # validate the path
  stopifnot(renv_path_absolute(path))

  # create our cache entry
  info <- renv_file_info(path)
  entry <- list(value = value, info = info)

  # store it
  envir <- renv_filebacked_envir(context)
  assign(path, entry, envir = envir)
  invisible(value)

}

renv_filebacked_get <- function(context, path) {

  # validate the path
  if (!renv_path_absolute(path))
    stopf("internal error: '%s' is not an absolute path", path)

  # get contextd sub-environment
  envir <- renv_filebacked_envir(context)

  # check for entry in the cache
  entry <- envir[[path]]
  if (is.null(entry))
    return(NULL)

  # extract pieces of interest
  value   <- entry$value
  oldinfo <- entry$info
  newinfo <- renv_file_info(path)

  # if the file didn't exist when we set the entry,
  # check and see if it's still not there
  if (is.na(oldinfo$isdir) && is.na(newinfo$isdir))
    return(value)

  # compare on fields of interest
  fields <- c("size", "isdir", "mtime")
  if (!identical(oldinfo[fields], newinfo[fields]))
    return(NULL)

  # looks good
  value

}

renv_filebacked_envir <- function(context) {
  the$filebacked_cache[[context]] <-
    the$filebacked_cache[[context]] %||%
    new.env(parent = emptyenv())
}

filebacked <- function(context, path, callback, ...) {

  # don't use filebacked cache when disabled
  config <- config$filebacked.cache()
  if (identical(config, FALSE))
    return(callback(path, ...))

  # check for cache entry -- if available, use it
  cache <- renv_filebacked_get(context, path)
  if (!is.null(cache))
    return(cache)

  # otherwise, generate our value and cache it
  result <- callback(path, ...)
  renv_filebacked_set(context, path, result)

  result

}

renv_filebacked_invalidate <- function(path) {
  renv_scope_options(warn = -1L)
  eapply(the$filebacked_cache, function(context) {
    rm(list = path, envir = context)
  })
}


# files.R --------------------------------------------------------------------


# NOTE: all methods here should either return TRUE if they were able to
# operate successfully, or throw an error if not
#
# TODO: some of these operations are a bit racy
renv_file_preface <- function(source, target, overwrite) {

  callback <- function() {}
  if (!renv_file_exists(source))
    stopf("source file '%s' does not exist", source)

  if (overwrite)
    callback <- renv_file_backup(target)

  if (renv_file_exists(target))
    stopf("target file '%s' already exists", target)

  callback

}

renv_file_copy <- function(source, target, overwrite = FALSE) {

  if (renv_file_same(source, target))
    return(TRUE)

  callback <- renv_file_preface(source, target, overwrite)
  defer(callback())

  # check to see if we're copying a plain file -- if so, things are simpler
  if (dir.exists(source))
    renv_file_copy_dir(source, target)
  else
    renv_file_copy_file(source, target)

}

renv_file_copy_file <- function(source, target) {

  # copy to temporary path
  tmpfile <- renv_scope_tempfile(".renv-copy-", tmpdir = dirname(target))
  status <- catchall(file.copy(source, tmpfile))
  if (inherits(status, "condition"))
    stop(status)

  # move from temporary path to final target
  status <- catchall(renv_file_move(tmpfile, target))
  if (inherits(status, "condition"))
    stop(status)

  # validate that the target file exists
  if (!renv_file_exists(target)) {
    fmt <- "attempt to copy file %s to %s failed (unknown reason)"
    stopf(fmt, renv_path_pretty(source), renv_path_pretty(target))
  }

  invisible(TRUE)

}

renv_file_copy_dir_robocopy <- function(source, target) {
  renv_robocopy_copy(source, target)
}

# TODO: the version of rsync distributed with macOS
# does not reliably copy file modified times, etc.
renv_file_copy_dir_rsync <- function(source, target) {
  source <- sub("/*$", "/", source)
  flags <- if (renv_platform_macos()) "-aAX" else "-a"
  args <- c(flags, renv_shell_path(source), renv_shell_path(target))
  renv_system_exec("rsync", args, action = "copying directory")
}

renv_file_copy_dir_cp <- function(source, target) {

  # ensure 'source' ends with a single trailing slash
  source <- sub("/*$", "/", source)

  # ensure tildes are path-expanded
  source <- path.expand(source)
  target <- path.expand(target)

  # build 'cp' arguments
  args <- c("-pPR", renv_shell_path(source), renv_shell_path(target))

  # execute command
  renv_system_exec("cp", args, action = "copying directory")

}

renv_file_copy_dir_r <- function(source, target) {

  # create sub-directory to host copy attempt
  tempdir <- renv_scope_tempfile(".renv-copy-", tmpdir = dirname(target))
  ensure_directory(tempdir)

  # attempt to copy to generated folder
  status <- catchall(
    file.copy(
      source,
      tempdir,
      recursive = TRUE,
      copy.mode = TRUE,
      copy.date = TRUE
    )
  )

  if (inherits(status, "error"))
    stop(status)

  # R will copy the directory to a sub-directory in the
  # requested folder with the same filename as the source
  # folder, so peek into that folder to grab it and rename
  tempfile <- file.path(tempdir, basename(source))
  status <- catchall(renv_file_move(tempfile, target))
  if (inherits(status, "condition"))
    stop(status)

}

renv_file_copy_dir_impl <- function(source, target) {

  methods <- list(
    cp       = renv_file_copy_dir_cp,
    r        = renv_file_copy_dir_r,
    robocopy = renv_file_copy_dir_robocopy,
    rsync    = renv_file_copy_dir_rsync
  )

  copy <- config$copy.method()
  if (is.function(copy))
    return(copy(source, target))

  method <- methods[[tolower(copy)]]
  if (!is.null(method))
    return(method(source, target))

  if (renv_platform_windows())
    renv_file_copy_dir_robocopy(source, target)
  else if (renv_platform_unix())
    renv_file_copy_dir_cp(source, target)
  else
    renv_file_copy_dir_r(source, target)

  file.exists(target)

}

renv_file_copy_dir <- function(source, target) {

  # create temporary sub-directory
  tmpdir <- dirname(target)
  ensure_directory(tmpdir)
  tempdir <- renv_scope_tempfile(".renv-copy-", tmpdir = tmpdir)

  # copy to that directory
  status <- catchall(renv_file_copy_dir_impl(source, tempdir))
  if (inherits(status, "condition"))
    stop(status)

  # move directory to final location
  status <- catchall(renv_file_move(tempdir, target))
  if (inherits(status, "condition"))
    stop(status)

  # validate that the target file exists
  if (!renv_file_exists(target)) {
    fmt <- "attempt to copy directory %s to %s failed (unknown reason)"
    stopf(fmt, renv_path_pretty(source), renv_path_pretty(target))
  }

  invisible(TRUE)

}

renv_file_move <- function(source, target, overwrite = FALSE) {

  if (renv_file_same(source, target))
    return(TRUE)

  callback <- renv_file_preface(source, target, overwrite)
  defer(callback())

  # first, attempt to do a plain rename
  # use catchall since this might fail for e.g. cross-device links
  # (note that junction points on Windows will be copies as-is)
  move <- catchall(file.rename(source, target))
  if (renv_file_exists(target))
    return(TRUE)

  # expand tildes
  source <- path.expand(source)
  target <- path.expand(target)

  # on unix, try using 'mv' command directly
  # (can handle cross-device copies / moves a bit more efficiently)
  if (renv_platform_unix()) {
    args <- c(renv_shell_path(source), renv_shell_path(target))
    status <- catchall(system2("mv", args, stdout = FALSE, stderr = FALSE))
    if (renv_file_exists(target))
      return(TRUE)
  }

  # on Windows, similarly try 'robocopy' command
  # (should be faster than 'move' for large directories)
  if (renv_platform_windows()) {
    status <- catchall(renv_robocopy_move(source, target))
    if (renv_file_exists(target))
      return(TRUE)
  }

  # nocov start
  # rename failed; fall back to copying
  # (and be sure to remove the source file / directory on success)
  copy <- catchall(renv_file_copy(source, target, overwrite = overwrite))
  if (identical(copy, TRUE) && file.exists(target)) {
    unlink(source, recursive = TRUE)
    return(TRUE)
  }

  # rename and copy both failed: inform the user
  fmt <- stack()
  fmt$push("could not copy / move file '%s' to '%s'")
  if (inherits(move, "condition"))
    fmt$push(paste("move:", conditionMessage(move)))
  if (inherits(copy, "condition"))
    fmt$push(paste("copy:", conditionMessage(copy)))

  text <- paste(fmt$data(), collapse = "\n")
  stopf(text, source, target)
  # nocov end

}

renv_file_link <- function(source, target, overwrite = FALSE) {

  if (renv_file_same(source, target))
    return(TRUE)

  callback <- renv_file_preface(source, target, overwrite)
  defer(callback())

  if (renv_platform_windows()) {

    # use junction points on Windows by default as symlinks
    # are unreliable / un-deletable in some circumstances
    status <- catchall(Sys.junction(source, target))
    if (identical(status, TRUE))
      return(TRUE)

    # if Sys.junction() fails, it may leave behind an empty
    # directory. this may occur if the source and target files
    # reside on different volumes. either way, remove an empty
    # left-behind directory on failure
    unlink(target, recursive = TRUE, force = TRUE)

  } else {

    # on non-Windows, we can try to create a symlink
    status <- catchall(file.symlink(source, target))
    if (identical(status, TRUE))
      return(TRUE)

  }

  # all else fails, just perform a copy
  renv_file_copy(source, target, overwrite = overwrite)

}

renv_file_junction <- function(source, target) {

  if (!renv_platform_windows())
    stopf("'renv_file_junction()' is only available on Windows")

  if (renv_file_exists(target))
    stopf("file '%s' already exists")

  status <- catchall(Sys.junction(source, target))
  if (inherits(status, "condition")) {
    unlink(target, recursive = TRUE, force = TRUE)
    stop(status)
  }

  TRUE

}

renv_file_same <- function(source, target) {

  # if the paths are the same, we can return early
  if (identical(source, target))
    return(TRUE)

  # check to see if they're equal after normalization
  # (e.g. for symlinks pointing to same file)
  source <- renv_path_normalize(source)
  target <- renv_path_normalize(target)
  if (identical(source, target))
    return(TRUE)

  # if either file is missing, return false
  if (!renv_file_exists(source) || !renv_file_exists(target))
    return(FALSE)

  # for hard links + junction points, it's difficult to detect
  # whether the two files point to the same object; use some
  # heuristics to guess (note that these aren't perfect)
  sinfo <- renv_file_info(source)
  tinfo <- renv_file_info(target)
  if (!identical(c(sinfo), c(tinfo)))
    return(FALSE)

  TRUE

}

# NOTE: returns a callback which should be used in e.g. an defer handler
# to restore the file if the attempt to update the file failed
renv_file_backup <- function(path) {

  # if no file exists then nothing to backup
  if (!renv_file_exists(path))
    return(function() {})

  # normalize the path (since the working directory could change
  # by the time the callback is invoked). note that the file may
  # be a broken symlink so construct the path by normalizing the
  # parent directory and building path relative to that
  parent <- renv_path_normalize(dirname(path), mustWork = TRUE)
  path <- file.path(parent, basename(path))

  # attempt to rename the file
  pattern <- sprintf(".renv-backup-%i-%s", Sys.getpid(), basename(path))
  tempfile <- tempfile(pattern, tmpdir = dirname(path))
  if (!renv_file_move(path, tempfile))
    return(function() {})

  # return callback that will restore if needed
  function() {

    if (!renv_file_exists(path))
      renv_file_move(tempfile, path)
    else
      unlink(tempfile, recursive = TRUE)

  }

}

renv_file_info <- function(paths, extra_cols = FALSE) {
  suppressWarnings(file.info(paths, extra_cols = extra_cols))
}

renv_file_mode <- function(paths) {
  suppressWarnings(file.mode(paths))
}

# NOTE: returns true for files that are broken symlinks
renv_file_exists <- function(path) {

  if (renv_platform_windows())
    renv_file_exists_win32(path)
  else
    renv_file_exists_unix(path)

}

renv_file_exists_win32 <- function(path) {
  file.exists(path)
}

renv_file_exists_unix <- function(path) {
  !is.na(Sys.readlink(path)) | file.exists(path)
}

renv_file_list <- function(path, full.names = TRUE) {

  # list files
  files <- renv_file_list_impl(path)

  # NOTE: paths may be marked with UTF-8 encoding;
  # if that's the case we need to use paste rather
  # than file.path to preserve the encoding
  if (full.names && length(files))
    files <- paste(path, files, sep = "/")

  files

}

renv_file_list_impl <- function(path) {
  if (renv_platform_unix())
    renv_file_list_impl_unix(path)
  else
    renv_file_list_impl_win32(path)
}

renv_file_list_impl_unix <- function(path) {
  list.files(path, all.files = TRUE, no.. = TRUE)
}

# nocov start
renv_file_list_impl_win32 <- function(path) {

  # first, try a plain list.files to see if we can get away with that
  files <- list.files(path, all.files = TRUE, no.. = TRUE)
  if (!any(grepl("?", files, fixed = TRUE)))
    return(files)

  # otherwise, try some madness ...
  #
  # change working directory (done just to avoid encoding issues
  # when submitting path to cmd shell)
  renv_scope_wd(path)

  # NOTE: a sub-shell is required here in some contexts; e.g. when running
  # tests non-interactively or building in the RStudio pane
  command <- paste(comspec(), "/U /C dir /B")
  conn <- pipe(command, open = "rb", encoding = "native.enc")
  defer(close(conn))

  # read binary output from connection
  output <- stack()

  while (TRUE) {

    data <- readBin(conn, what = "raw", n = 1024L)
    if (empty(data))
      break

    output$push(data)

  }

  # join into single raw vector
  encoded <- unlist(output$data(), recursive = FALSE, use.names = FALSE)

  # convert raw data (encoded as UTF-16LE) to UTF-8
  converted <- iconv(list(encoded), from = "UTF-16LE", to = "UTF-8")

  # split on (Windows) newlines
  paths <- strsplit(converted, "\r\n", fixed = TRUE)[[1]]

  # just in case?
  paths[nzchar(paths)]

}
# nocov end

renv_file_type <- function(paths, symlinks = TRUE) {

  info <- renv_file_info(paths)

  types <- character(length(paths))
  types[info$isdir %in% FALSE] <- "file"
  types[info$isdir %in% TRUE ] <- "directory"

  if (symlinks && !renv_platform_windows()) {
    links <- Sys.readlink(paths)
    types[!is.na(links) & nzchar(links)] <- "symlink"
  }

  types

}

# nocov start
renv_file_edit <- function(path) {

  # https://github.com/rstudio/renv/issues/44
  dlls <- getLoadedDLLs()
  if (is.null(dlls[["(embedding)"]]))
    return(utils::file.edit(path))

  routines <- getDLLRegisteredRoutines("(embedding)")
  routine <- routines[[".Call"]][["rs_editFile"]]
  if (is.null(routine))
    return(utils::file.edit(path))

  do.call(.Call, list(routine, path, PACKAGE = "(embedding)"))

}
# nocov end

renv_file_find <- function(path, predicate) {

  # canonicalize path
  # (note: don't normalize as we don't want to follow symlinks)
  path <- renv_path_canonicalize(path)
  parent <- dirname(path)

  # compute number of slashes
  # (avoid searching beyond home directory, unless we're virtualized)
  virtualized <- renv_virtualization_type() != "native"
  slashes <- gregexpr("/", path, fixed = TRUE)[[1L]]
  n <- length(slashes) - if (virtualized) 0L else 2L

  for (i in 1:n) {

    if (file.exists(path)) {
      status <- predicate(path)
      if (!is.null(status))
        return(status)
    }

    path <- parent
    parent <- dirname(path)

  }

  predicate(path)

}

renv_file_read <- function(path) {
  renv_scope_options(warn = -1L)
  contents <- readLines(path, warn = FALSE, encoding = "UTF-8")
  paste(contents, collapse = "\n")
}

renv_file_shebang <- function(path) {

  # NOTE: we use 'condition' as a cheap way to capture both errors and warnings
  # since 'file()' may just report a warning rather than an error if it fails
  # to open a file due to inadequate permissions
  tryCatch(
    renv_file_shebang_impl(path),
    condition = function(e) ""
  )

}

renv_file_shebang_impl <- function(path) {

  renv_scope_options(warn = -1L)

  # open connection to file
  con <- file(path, open = "rb", encoding = "native.enc")
  defer(close(con))

  # validate file starts with '#!' -- read using 'raw' vector to avoid
  # issues which files that might start with null bytes
  bytes <- readBin(con, what = "raw", n = 2L)
  expected <- as.raw(c(0x23L, 0x21L))
  if (!identical(bytes, expected))
    return("")

  # read a single line from the connection
  readLines(con, n = 1L, warn = FALSE)

}

# here, 'broken' implies a file which is a link pointing to a file that
# doesn't exist, so only returns true if the file is "link"-y and the
# file it points to doesn't exist
renv_file_broken <- function(paths) {
  if (renv_platform_unix())
    renv_file_broken_unix(paths)
  else
    renv_file_broken_win32(paths)
}

renv_file_broken_unix <- function(paths) {
  # a symlink is broken if:
  # - the file is a symlink (tested via Sys.readlink)
  # - the file it points to does not exist (tested via file.exists)
  !is.na(Sys.readlink(paths)) & !file.exists(paths)
}

renv_file_broken_win32 <- function(paths) {
  # TODO: the behavior of file.exists() for a broken junction point
  # appears to have changed in the development version of R;
  # we have to be extra careful here...
  if (getRversion() < "4.2.0") {
    info <- renv_file_info(paths)
    (info$isdir %in% TRUE) & is.na(info$mtime)
  } else {
    file.access(paths, mode = 0L) == 0L & !file.exists(paths)
  }
}

renv_file_size <- function(path) {
  file.info(path, extra_cols = FALSE)$size
}

renv_file_remove <- function(paths) {
  if (renv_platform_windows())
    renv_file_remove_win32(paths)
  else
    renv_file_remove_unix(paths)
}

renv_file_remove_win32 <- function(paths) {
  for (path in paths) {
    command <- paste("rmdir /S /Q", renv_shell_path(path))
    shell(command)
  }
}

renv_file_remove_unix <- function(paths) {
  unlink(paths, recursive = TRUE, force = TRUE)
}

renv_file_writable <- function(path) {

  # allow users to opt-out just in case
  override <- getOption("renv.download.check_writable", default = TRUE)
  if (!identical(override, TRUE))
    return(TRUE)

  # if we're given the path to a file, use the parent directory instead
  info <- renv_file_info(path)
  if (!identical(info$isdir, TRUE))
    path <- dirname(path)

  # if we still don't have a directory, bail
  info <- renv_file_info(path)
  if (!identical(info$isdir, TRUE))
    return(FALSE)

  # try creating and removing a temporary file in this directory
  tempfile <- renv_scope_tempfile(".renv-write-test-", tmpdir = path)
  ok <- dir.create(tempfile, showWarnings = FALSE)

  # return ok if we succeeded
  ok

}


# git.R ----------------------------------------------------------------------


git <- function() {

  gitpath <- Sys.which("git")
  if (!nzchar(gitpath))
    stop("failed to find git executable on the PATH")

  gitpath

}


renv_git_preflight <- function() {
  if (!nzchar(Sys.which("git")))
    stopf("'git' is not available on the PATH")
}

renv_git_root <- function(project) {

  project <- renv_path_normalize(project)
  renv_file_find(project, function(parent) {
    gitroot <- file.path(parent, ".git")
    if (file.exists(gitroot))
      return(gitroot)
  })

}


# graph.R --------------------------------------------------------------------


#' Generate a Package Dependency Graph
#'
#' Generate a package dependency graph.
#'
#' @inheritParams renv-params
#'
#' @param root The top-most package dependencies of interest in the dependency graph.
#'
#' @param leaf The bottom-most package dependencies of interest in the dependency graph.
#'
#' @param suggests Should suggested packages be included within
#'   the dependency graph?
#'
#' @param enhances Should enhanced packages be included within
#'   the dependency graph?
#'
#' @param resolver An \R function accepting a package name, and returning the
#'   contents of its `DESCRIPTION` file (as an \R `data.frame` or `list`).
#'   When `NULL` (the default), an internal resolver is used.
#'
#' @param renderer Which package should be used to render the resulting graph?
#'
#' @param attributes An \R list of graphViz attributes, mapping node names to
#'   attribute key-value pairs. For example, to ask graphViz to prefer orienting
#'   the graph from left to right, you can use
#'   `list(graph = c(rankdir = "LR"))`. See <https://graphviz.org/doc/info/attrs.html>
#'   for a full list of the attributes supported by `graphViz`.
#'
#' @examples
#'
#' \dontrun{
#' # graph the relationship between devtools and rlang
#' graph(root = "devtools", leaf = "rlang")
#'
#' # figure out why a project depends on 'askpass'
#' graph(leaf = "askpass")
#' }
#'
#' @keywords internal
graph <- function(root = NULL,
                  leaf = NULL,
                  ...,
                  suggests   = FALSE,
                  enhances   = FALSE,
                  resolver   = NULL,
                  renderer   = c("DiagrammeR", "visNetwork"),
                  attributes = list(),
                  project    = NULL)
{
  renv_scope_error_handler()
  project <- renv_project_resolve(project)

  # figure out packages to try and read
  root <- root %||% renv_graph_roots(project)

  # resolve fields
  fields <- c(
    "Depends", "Imports", "LinkingTo",
    if (suggests) "Suggests",
    if (enhances) "Enhances"
  )

  # resolve renderer
  renderer <- renv_graph_renderer(renderer)

  # find dependencies
  envir <- new.env(parent = emptyenv())
  revdeps <- new.env(parent = emptyenv())
  for (package in root)
    renv_graph_build(package, fields, resolver, envir, revdeps)

  # prune the tree
  tree <- renv_graph_prune(root, leaf, envir, revdeps)

  # compute the graph
  graph <- enumerate(tree, function(package, dependencies) {

    enumerate(dependencies, function(field, packages) {
      attrs <- renv_graphviz_attrs(field, renderer)
      renv_graphviz_edge(package, packages, attrs)
    })

  })

  # figure out which packages remain part of the graph after pruning
  ok <- map_lgl(graph, function(items) {
    any(map_int(items, length) > 0)
  })

  remaining <- intersect(root, names(graph)[ok])
  if (empty(remaining)) {
    fmt <- "- Could not find any relationship between the requested packages."
    writef(fmt)
    return(invisible(NULL))
  }

  defaults <- renv_graphviz_defaults(renderer)
  attributes <- overlay(defaults, attributes)

  # render attributes
  attrtext <- renv_graphviz_render(attributes, TRUE)

  # fill package names which are top-level dependencies
  topattrs <- renv_graphviz_render(
    map(named(remaining), function(name) {
      list(
        style     = "filled",
        fillcolor = "#b3cde3"
      )
    }),
    asis = FALSE
  )

  botattrs <- renv_graphviz_render(
    map(named(leaf), function(name) {
      list(
        style     = "filled",
        fillcolor = "#ccebc5"
      )
    }),
    asis = FALSE
  )

  # collapse into text
  parts <- c(
    'digraph {', '',
    attrtext, '',
    topattrs, '',
    botattrs, '',
    unlist(graph), '',
    '}'
  )

  diagram <- paste(parts, collapse = "\n")

  renderer <- case(

    identical(renderer, "DiagrammeR") ~ function(dot) {
      DiagrammeR <- renv_namespace_load("DiagrammeR")
      DiagrammeR$grViz(diagram = dot)
    },

    identical(renderer, "visNetwork") ~ function(dot) {

      visNetwork <- renv_namespace_load("visNetwork")
      graph <- visNetwork$visNetwork(dot = dot)

      graph$x$options$edges$font$background <- "white"

      # TODO: allow hierarchical layout via option?
      # graph$x$options$layout = list(
      #   hierarchical = list(
      #     blockShifting   = TRUE,
      #     levelSeparation = 50,
      #     nodeSpacing     = 1,
      #     shakeTowards    = "roots",
      #     sortMethod      = "directed"
      #   )
      # )

      graph

    },

    is.function(renderer) ~ renderer,

    ~ stop("unrecognized renderer")

  )

  renderer(diagram)
}

renv_graph_build <- function(package, fields, resolver, envir, revdeps) {

  # check if we've already scanned this package
  if (exists(package, envir = envir))
    return()

  # read package dependencies
  deps <- renv_graph_dependencies(package, fields, resolver)

  # add dependencies to graph
  assign(package, deps, envir = envir)

  # recurse
  children <- sort(unique(unlist(deps)))
  for (child in children) {
    assign(child, c(package, revdeps[[child]]), envir = revdeps)
    renv_graph_build(child, fields, resolver, envir, revdeps)
  }

}

renv_graph_revdeps <- function(packages, revdeps) {

  envir <- new.env(parent = emptyenv())
  for (package in packages)
    renv_graph_revdeps_impl(package, envir, revdeps)

  ls(envir = envir)

}

renv_graph_revdeps_impl <- function(package, envir, revdeps) {

  if (visited(package, envir))
    return()

  for (child in revdeps[[package]])
    renv_graph_revdeps_impl(child, envir, revdeps)

}

renv_graph_roots <- function(project) {

  deps <- renv_dependencies_impl(project, errors = "ignored")
  sort(unique(deps$Package))

}

renv_graph_dependencies <- function(package, fields, resolver) {

  base <- installed_packages(priority = "base")

  desc <- local({

    # try using the resolver if supplied
    if (!is.null(resolver)) {
      desc <- catch(resolver(package))
      if (inherits(desc, "error"))
        warning(desc)
      else if (!is.null(desc))
        return(desc)
    }

    # check for (and prefer) a locally-installed package
    path <- renv_package_find(package)
    if (nzchar(path))
      return(renv_description_read(path))

    # otherwise, try and see if this is a known CRAN package
    as.list(renv_available_packages_entry(package))

  })

  # parse the fields
  values <- map(fields, function(field) {

    item <- desc[[field]]
    if (is.null(item))
      return(NULL)

    parsed <- renv_description_parse_field(item)
    packages <- parsed$Package

    setdiff(packages, c("R", base$Package))

  })

  names(values) <- fields
  values

}

renv_graph_prune <- function(root, leaf, envir, revdeps) {

  # grab all computed dependencies
  all <- as.list(envir)

  # if we don't have any leaves, then just return everything
  if (empty(leaf))
    return(all)

  # otherwise, find recursive dependencies of the requested packages
  rrd <- renv_graph_revdeps(leaf, revdeps)
  map(all, function(children) {
    map(children, intersect, rrd)
  })

}

renv_graphviz_node <- function(nodes, asis, attrs) {

  keys <- names(attrs)
  vals <- renv_json_quote(attrs)
  attrtext <- paste(keys, vals, sep = "=", collapse = ", ")

  fmt <- if (asis) '%s [%s]' else '"%s" [%s]'
  sprintf(fmt, nodes, attrtext)

}

renv_graphviz_edge <- function(lhs, rhs, attrs) {

  if (empty(lhs) || empty(rhs))
    return(character())

  keys <- names(attrs)
  vals <- renv_json_quote(attrs)
  attrtext <- paste(keys, vals, sep = "=", collapse = ", ")

  fmt <- '"%s" -> "%s" [%s]'
  sprintf(fmt, lhs, rhs, attrtext)

}

renv_graphviz_attrs <- function(field, renderer) {

  dil <- "#c0c0c0"

  defaults <- list(

    Depends = list(
      color = dil,
      style = "solid"
    ),

    Imports = list(
      color = dil,
      style = "solid"
    ),

    LinkingTo = list(
      color = dil,
      style = "dashed"
    ),

    Suggests = list(
      color = "darkgreen",
      style = "dashed"
    ),

    Enhances = list(
      color = "darkblue",
      style = "dashed"
    )

  )

  attrs <- defaults[[field]]
  if (identical(renderer, "visNetwork")) {

    extra <- c(
      font.align = "middle"
    )

    attrs <- c(attrs, extra)

  }

  attrs

}

renv_graphviz_defaults <- function(renderer) {

  case(
    identical(renderer, "visNetwork") ~ renv_graphviz_defaults_visnetwork(),
    identical(renderer, "DiagrammeR") ~ renv_graphviz_defaults_diagrammer(),
  )

}

renv_graphviz_defaults_visnetwork <- function() {

  list(

    node = list(
      style     = "filled",
      shape     = "ellipse",
      color     = "black",
      fillcolor = "#e5d8bd",
      fontname  = "Helvetica"
    )

  )

}

renv_graphviz_defaults_diagrammer <- function() {

  list(

    graph = list(
      nodesep = 0.10
    ),

    node = list(
      style     = "filled",
      shape     = "ellipse",
      fillcolor = "#e5d8bd",
      fontname  = "Helvetica"
    )

  )

}

renv_graphviz_render <- function(attributes, asis) {

  rendered <- enumerate(attributes, function(key, value) {

    if (is.null(names(value))) {
      lhs <- if (asis) key else renv_json_quote(key)
      rhs <- renv_graphviz_render_value(value)
      if (length(lhs) && length(rhs))
        paste(lhs, rhs, sep = " = ")
    } else {
      keys <- names(value)
      vals <- renv_graphviz_render_value(value)
      fmt <- if (asis) '%s [%s]' else '"%s" [%s]'
      sprintf(fmt, key, paste(keys, vals, sep = "=", collapse = ", "))
    }

  })

  unlist(rendered, recursive = TRUE, use.names = FALSE)

}

renv_graphviz_render_value <- function(value) {
  if (is.numeric(value))
    format(value)
  else if (is.logical(value))
    tolower(as.character(value))
  else
    renv_json_quote(value)
}

renv_graph_renderer <- function(renderer) {

  # allow functions as-is
  if (is.function(renderer))
    return(renderer)

  # otherwise, match
  renderer <- match.arg(renderer, choices = c("DiagrammeR", "visNetwork"))
  if (!renv_package_installed(renderer)) {
    fmt <- "package '%s' is required to render graphs but is not installed"
    stopf(fmt, renderer)
  }

  renderer

}



# hash.R ---------------------------------------------------------------------


renv_hash_text <- function(text) {
  renv_bootstrap_hash_text(text)
}

renv_hash_description <- function(path) {
  filebacked(
    context  = "renv_hash_description",
    path     = path,
    callback = renv_hash_description_impl
  )
}

renv_hash_description_impl <- function(path) {

  dcf <- case(
    is.character(path) ~ renv_description_read(path),
    is.list(path)      ~ path,
    ~ stop("unexpected path '%s'", path)
  )

  # include default fields
  fields <- c(
    "Package", "Version", "Title", "Author", "Maintainer", "Description",
    "Depends", "Imports", "Suggests", "LinkingTo"
  )

  # add remotes fields
  remotes <- renv_hash_description_remotes(dcf)

  # retrieve these fields
  subsetted <- dcf[renv_vector_intersect(c(fields, remotes), names(dcf))]

  # sort names (use C locale to ensure consistent ordering)
  ordered <- subsetted[csort(names(subsetted))]

  # write to tempfile (use binary connection to ensure unix-style
  # newlines for cross-platform hash stability)
  tempfile <- tempfile("renv-description-hash-")
  contents <- paste(names(ordered), ordered, sep = ": ", collapse = "\n")

  # remove whitespace -- it's possible that tools (e.g. Packrat) that
  # mutate a package's DESCRIPTION file may also inadvertently change
  # the structure of whitespace within some fields; that whitespace is
  # normally not semantically meaningful so we remove that so such
  # DESCRIPTIONS can obtain the same hash value. (this ultimately
  # arises as 'write.dcf()' allows both 'indent' and 'width' to be
  # configured based on the 'width' option)
  contents <- gsub("[[:space:]]", "", contents)

  # create the file connection (use binary so that unix newlines are used
  # across platforms, for more stable hashing)
  con <- file(tempfile, open = "wb")

  # write to the file
  writeLines(enc2utf8(contents), con = con, useBytes = TRUE)

  # flush to ensure we've written to file
  flush(con)

  # close the connection and remove the file
  close(con)

  # ready for hasing
  hash <- unname(tools::md5sum(tempfile))

  # remove the old file
  unlink(tempfile)

  # return hash
  invisible(hash)

}

renv_hash_description_remotes <- function(dcf) {

  type <- dcf[["RemoteType"]]
  if (is.null(type))
    return(character())

  if (type == "standard")
    return(character())

  grep("^Remote", names(dcf), value = TRUE)

}


# history.R ------------------------------------------------------------------


#' View and revert to a historical lockfile
#'
#' @description
#' `history()` uses your version control system to show prior versions of the
#' lockfile and `revert()` allows you to restore one of them.
#'
#' These functions are currently only implemented for projects that use git.
#'
#' @inherit renv-params
#'
#' @export
#'
#' @return `history()` returns a `data.frame` summarizing the commits in which
#'   `renv.lock` has been changed. `revert()` is usually called for its
#'   side-effect but also invisibly returns the `commit` used.
#'
#' @examples
#' \dontrun{
#'
#' # get history of previous versions of renv.lock in VCS
#' db <- renv::history()
#'
#' # choose an older commit
#' commit <- db$commit[5]
#'
#' # revert to that version of the lockfile
#' renv::revert(commit = commit)
#'
#' }
history <- function(project = NULL) {

  renv_scope_error_handler()

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  lockpath <- renv_lockfile_path(project)
  if (!file.exists(lockpath))
    return(data_frame())

  renv_git_preflight()

  renv_scope_wd(project)

  args <- c("log", "--pretty=format:%H\031%at\031%ct\031%s", renv_shell_path(lockpath))
  data <- renv_system_exec("git", args, action = "retrieving git log")

  parts <- strsplit(data, "\031", fixed = TRUE)
  tbl <- bind(parts, names = c("commit", "author_date", "committer_date", "subject"))
  tbl$author_date <- as.POSIXct(as.numeric(tbl$author_date), origin = "1970-01-01")
  tbl$committer_date <- as.POSIXct(as.numeric(tbl$committer_date), origin = "1970-01-01")

  tbl

}

#' @param commit The commit associated with a prior version of the lockfile.
#' @param ... Optional arguments; currently unused.
#' @export
#' @rdname history
revert <- function(commit = "HEAD", ..., project = NULL) {

  renv_scope_error_handler()
  renv_dots_check(...)

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  renv_git_preflight()

  renv_scope_wd(project)

  lockpath <- renv_lockfile_path(project = project)
  system2("git", c("checkout", commit, "--", renv_shell_path(lockpath)))
  system2("git", c("reset", "HEAD", renv_shell_path(lockpath)), stdout = FALSE, stderr = FALSE)
  system2("git", c("diff", "--", renv_shell_path(lockpath)))

  writef("- renv.lock from commit %s has been checked out.", commit)
  invisible(commit)

}


# homebrew.R -----------------------------------------------------------------


renv_homebrew_root <- function() {

  # allow override
  root <- Sys.getenv("RENV_HOMEBREW_ROOT", unset = NA)
  if (!is.na(root))
    return(root)

  # indirection for arm64 macOS
  if (renv_platform_macos() && renv_platform_machine() != "x86_64")
    return("/opt/homebrew")

  # default to /usr/local
  "/usr/local"

}


# http.R ---------------------------------------------------------------------


renv_http_useragent <- function() {
  agent <- getOption("renv.http.useragent", default = getOption("HTTPUserAgent"))
  agent %||% renv_http_useragent_default()
}

renv_http_useragent_default <- function() {
  version <- getRversion()
  platform <- with(R.version, paste(version, platform, arch, os))
  sprintf("R/%s R (%s)", version, platform)
}


# hydrate.R ------------------------------------------------------------------


#' Copy packages from user libraries to a project library
#'
#' @description
#' `hydrate()` installs missing packages from a user library into the project
#' library. `hydrate()` is called automatically by [init()], and it is rare
#' that you should need it otherwise, as it can easily get your project into
#' an inconsistent state.
#'
#' It may very occasionally be useful to call `hydate(update = "all")` if you
#' want to update project packages to match those installed in your global
#' library (as opposed to using [update()] which will get the latest versions
#' from CRAN). In this case, you should verify that your code continues to work,
#' then call [snapshot()] to record updated package versions in the lockfile.
#'
#' @inherit renv-params
#'
#' @param packages The set of \R packages to install. When `NULL`, the
#'   packages found by [dependencies()] are used.
#'
#' @param library The \R library to be hydrated. When `NULL`, the active
#'   library as reported by `.libPaths()` is used.
#'
#' @param repos The \R repositories to be used. If the project depends on any
#'   \R packages which cannot be found within the user library paths, then
#'   those packages will be installed from these repositories instead.
#'
#' @param update Boolean; should `hydrate()` attempt to update already-installed
#'   packages if the requested package is already installed in the project
#'   library? Set this to `"all"` if you'd like _all_ packages to be refreshed
#'   from the source library if possible.
#'
#' @param sources A vector of library paths where renv should look for packages.
#'   When `NULL` (the default), `hydrate()` will look in the system libraries
#'   (the user library, the site library and the default library) then the
#'   renv cache.
#'
#'   If a package is not found in any of these locations, `hydrate()`
#'   will try to install it from the active R repositories.
#'
#' @param prompt Boolean; prompt the user before taking any action? Ignored
#'   when `report = FALSE`.
#'
#' @param report Boolean; display a report of what packages will be installed
#'   by `renv::hydrate()`?
#'
#' @return A named \R list, giving the packages that were used for hydration
#'   as well as the set of packages which were not found.
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'
#' # hydrate the active library
#' renv::hydrate()
#'
#' }
hydrate <- function(packages = NULL,
                    ...,
                    library = NULL,
                    repos   = getOption("repos"),
                    update  = FALSE,
                    sources = NULL,
                    prompt  = interactive(),
                    report  = TRUE,
                    project = NULL)
{
  renv_scope_error_handler()
  renv_dots_check(...)

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)
  renv_scope_verbose_if(prompt)

  renv_activate_prompt("hydrate", library, prompt, project)

  renv_scope_options(repos = repos)
  library <- renv_path_normalize(library %||% renv_libpaths_active())
  packages <- packages %||% renv_hydrate_packages(project)

  # find packages used in this project, and the dependencies of those packages
  deps <- renv_hydrate_dependencies(project, packages, sources)

  # remove 'renv' since it's managed separately
  deps$renv <- NULL

  # remove base + missing packages
  base <- renv_packages_base()
  missing <- deps[!nzchar(deps)]
  packages <- deps[renv_vector_diff(names(deps), c(names(missing), base))]

  # figure out if we will copy or link
  linkable <- renv_cache_linkable(project = project, library = library)

  # get and construct path to library
  ensure_directory(library)

  # only hydrate with packages that are either not currently installed,
  # or (if update = TRUE) the version in the library is newer
  packages <- renv_hydrate_filter(packages, library, update)

  # inform user about changes
  if (report) {
    renv_hydrate_report(packages, missing, linkable)
    if (length(packages) || length(missing))
      cancel_if(prompt && !proceed())
  }

  # check for nothing to be done
  if (empty(packages) && empty(missing)) {
    if (report)
      writef("- No new packages were discovered in this project; nothing to do.")
    return(invisible(list(packages = list(), missing = list())))
  }

  # copy packages from user library to cache
  before <- Sys.time()
  if (length(packages)) {
    if (linkable)
      renv_hydrate_link_packages(packages, library, project)
    else
      renv_hydrate_copy_packages(packages, library, project)
  }
  after <- Sys.time()

  if (report) {
    time <- difftime(after, before, units = "auto")
    fmt <- "- Hydrated %s packages in %s."
    writef(fmt, length(packages), renv_difftime_format(time))
  }

  # attempt to install missing packages (if any)
  missing <- renv_hydrate_resolve_missing(project, library, missing)

  # we're done!
  result <- list(packages = packages, missing = missing)
  invisible(result)
}

renv_hydrate_filter <- function(packages, library, update) {

  # run filter
  keep <- enumerate(
    packages,
    renv_hydrate_filter_impl,
    library = library,
    update = update,
    FUN.VALUE = logical(1)
  )

  # filter based on kept packages
  packages[keep]

}

renv_hydrate_filter_impl <- function(package, path, library, update) {

  # if user has requested hydration of all packages, respect that
  if (identical(update, "all"))
    return(TRUE)

  # is the package already installed in the requested library?
  # if not, then we'll want to hydrate this package
  # if so, we'll want to compare the version first and
  # hydrate only if the requested version is newer than the current
  descpath <- file.path(library, package, "DESCRIPTION")
  if (file.exists(descpath)) {
    desc <- catch(renv_description_read(path = descpath))
    if (inherits(desc, "error"))
      return(TRUE)
  }

  # get the current package version
  current <- catch(numeric_version(desc[["Version"]]))
  if (inherits(current, "error"))
    return(TRUE)

  # if the package is already installed and we're not updating, stop here
  if (identical(update, FALSE))
    return(FALSE)

  # check to-be-copied package version
  requested <- catch({
    desc <- renv_description_read(path = path)
    numeric_version(desc[["Version"]])
  })

  # only hydrate with a newer version
  requested > current

}

renv_hydrate_packages <- function(project) {
  renv_snapshot_dependencies(project, dev = TRUE)
}

renv_hydrate_dependencies <- function(project,
                                      packages = NULL,
                                      libpaths = NULL)
{
  ignored <- renv_project_ignored_packages(project = project)
  packages <- renv_vector_diff(packages, ignored)
  libpaths <- libpaths %||% renv_hydrate_libpaths()
  renv_package_dependencies(packages, libpaths = libpaths, project = project)
}

# NOTE: we don't want to look in user / site libraries when testing
# on CRAN, as we may accidentally find versions of packages available
# on CRAN but not that we want to use during tests
renv_hydrate_libpaths <- function() {

  conf <- config$hydrate.libpaths()
  if (is.character(conf) && length(conf))
    conf <- unlist(strsplit(conf, ":", fixed = TRUE))

  libpaths <- case(
    renv_tests_running() ~ character(),
    length(conf) ~ conf,
    ~ c(
      renv_libpaths_default(),
      renv_libpaths_user(),
      renv_libpaths_site(),
      renv_libpaths_system()
    )
  )

  libpaths <- .expand_R_libs_env_var(libpaths)
  unique(renv_path_normalize(libpaths))

}

# takes a package called 'package' installed at location 'location',
# copies that package into the cache, and then links from the cache
# to the (private) library 'library'
renv_hydrate_link_package <- function(package, location, library) {

  # construct path to cache
  record <- catch(renv_snapshot_description(location))
  if (inherits(record, "error"))
    return(FALSE)

  cache <- renv_cache_find(record)
  if (!nzchar(cache))
    return(FALSE)

  # copy package into the cache
  if (!file.exists(cache)) {
    ensure_parent_directory(cache)
    renv_file_copy(location, cache)
  }

  # link package back from cache to library
  target <- file.path(library, package)
  ensure_parent_directory(target)
  renv_file_link(cache, target, overwrite = TRUE)

}

renv_hydrate_link_packages <- function(packages, library, project) {

  if (renv_path_same(library, renv_paths_library(project = project)))
    printf("- Linking packages into the project library ... ")
  else
    printf("- Linking packages into %s ... ", renv_path_pretty(library))

  callback <- renv_progress_callback(renv_hydrate_link_package, length(packages))
  cached <- enumerate(packages, callback, library = library)
  writef("Done!")
  cached

}

# takes a package called 'package' installed at location 'location',
# and copies it to the library 'library'
renv_hydrate_copy_package <- function(package, location, library) {
  target <- file.path(library, package)
  renv_file_copy(location, target, overwrite = TRUE)
}

renv_hydrate_copy_packages <- function(packages, library, project) {

  if (renv_path_same(library, renv_paths_library(project = project)))
    printf("- Copying packages into the project library ... ")
  else
    printf("- Copying packages into %s ... ", renv_path_pretty(library))

  callback <- renv_progress_callback(renv_hydrate_copy_package, length(packages))
  copied <- enumerate(packages, callback, library = library)
  writef("Done!")
  copied
}

renv_hydrate_resolve_missing <- function(project, library, na) {

  # make sure requested library is made active
  #
  # note that we only want to place the requested library on the library path;
  # we want to ensure that all required packages are hydrated into the
  # reqeusted library
  #
  # https://github.com/rstudio/renv/issues/1177
  ensure_directory(library)
  renv_scope_libpaths(library)

  # figure out which packages are missing (if any)
  packages <- names(na)
  installed <- installed_packages(lib.loc = library)
  if (all(packages %in% installed$Package))
    return()

  writef("- Resolving missing dependencies ... ")

  # define a custom error handler for packages which we cannot retrieve
  errors <- stack()
  handler <- function(package, action) {
    error <- catch(action)
    if (inherits(error, "error"))
      errors$push(list(package = package, error = error))
  }

  # perform the restore
  renv_scope_restore(
    project  = project,
    library  = library,
    packages = packages,
    handler  = handler
  )

  records <- retrieve(packages)
  renv_install_impl(records)

  # if we failed to restore anything, warn the user
  data <- errors$data()
  if (empty(data))
    return()

  if (renv_verbose()) {

    text <- map_chr(data, function(item) {
      package <- item$package
      message <- conditionMessage(item$error)
      short <- trunc(paste(message, collapse = ";"), 60L)
      sprintf("[%s]: %s", package, short)
    })

    caution_bullets(
      "The following package(s) were not installed successfully:",
      text,
      "You may need to manually download and install these packages."
    )

  }

  invisible(data)

}

renv_hydrate_report <- function(packages, na, linkable) {

  if (renv_bootstrap_tests_running())
    return()

  if (length(packages)) {

    # this is mostly a hacky way to get a list of records that the existing
    # record pretty-printer can handle in a clean way
    records <- enumerate(packages, function(package, library) {
      descpath <- file.path(library, "DESCRIPTION")
      record <- renv_snapshot_description(descpath)
      record$Repository <- NULL
      record$Source <- renv_path_aliased(dirname(library))
      record
    })

    preamble <- "The following packages were discovered:"
    postamble <- sprintf(
      "They will be %s into the project library.",
      if (linkable) "linked" else "copied"
    )

    formatter <- function(lhs, rhs) {
      renv_record_format_short(rhs, versioned = TRUE)
    }

    renv_pretty_print_records_pair(
      preamble = preamble,
      old = list(),
      new = records,
      postamble = postamble,
      formatter = formatter
    )

  }

  if (length(na)) {
    caution_bullets(
      "The following packages are used in this project, but not available locally:",
      csort(names(na)),
      "renv will attempt to download and install these packages."
    )
  }

}


# id.R -----------------------------------------------------------------------


renv_id_path <- function(project) {
  file.path(project, "renv/project-id")
}

renv_id_generate <- function() {

  methods <- list(
    renv_id_generate_r,
    renv_id_generate_kernel,
    renv_id_generate_uuidgen,
    renv_id_generate_cscript,
    renv_id_generate_powershell,
    renv_id_generate_csc
  )

  for (method in methods) {
    id <- catch(method())
    if (is.character(id) && length(id) == 1 && nzchar(id)) {
      id <- toupper(id)
      return(id)
    }
  }

  stop("could not generate project id for this system")

}

renv_id_generate_kernel <- function() {

  uuidpath <- "/proc/sys/kernel/random/uuid"
  if (!file.exists(uuidpath)) {
    fmt <- "%s does not exist on this operating system"
    stopf(fmt, renv_path_pretty(uuidpath))
  }

  readLines(uuidpath, n = 1L, warn = FALSE)

}

renv_id_generate_uuidgen <- function() {

  if (!nzchar(Sys.which("uuidgen"))) {
    fmt <- "program %s does not exist on this system"
    stopf(fmt, shQuote("uuidgen"))
  }

  system("uuidgen", intern = TRUE)

}

renv_id_generate_cscript <- function() {

  if (!renv_platform_windows()) {
    fmt <- "this method is only available on Windows"
    stopf(fmt)
  }

  if (!nzchar(Sys.which("cscript.exe"))) {
    fmt <- "could not find cscript.exe"
    stopf(fmt)
  }

  # create temporary directory
  dir <- renv_scope_tempfile("renv-id-")
  dir.create(dir)

  # move to it
  renv_scope_wd(dir)

  # write helper script
  script <- c(
    "set object = CreateObject(\"Scriptlet.TypeLib\")",
    "WScript.StdOut.WriteLine object.GUID"
  )

  # invoke it
  writeLines(script, con = "uuid.vbs")
  args <- c("//NoLogo", "uuid.vbs")
  id <- renv_system_exec("cscript.exe", args, "generating UUID")

  # remove braces
  gsub("(?:^\\{|\\}$)", "", id)

}

renv_id_generate_powershell <- function() {

  if (!renv_platform_windows()) {
    fmt <- "this method is only available on Windows"
    stopf(fmt)
  }

  if (!nzchar(Sys.which("powershell.exe"))) {
    fmt <- "could not find powershell.exe"
    stopf(fmt)
  }

  command <- "[guid]::NewGuid().ToString()"
  args <- c("-Command", shQuote(command))
  renv_system_exec("powershell.exe", args, "generating UUID")

}

renv_id_generate_r <- function() {

  if ("uuid" %in% loadedNamespaces())
    return(uuid::UUIDgenerate())

  libpaths <- c(
    .libPaths(),
    renv_libpaths_user(),
    renv_libpaths_site(),
    renv_libpaths_system()
  )

  if (!requireNamespace("uuid", lib.loc = libpaths, quietly = TRUE))
    stop("could not load package 'uuid'")

  id <- uuid::UUIDgenerate()
  catchall(unloadNamespace("uuid"))
  id

}

renv_id_generate_csc <- function() {

  csc <- local({

    csc <- Sys.which("csc.exe")
    if (nzchar(csc))
      return(csc)

    frameworks <- file.path(
      Sys.getenv("SYSTEMDRIVE", unset = "C:"),
      "Windows/Microsoft.NET",
      c("Framework", "Framework64")
    )

    versions <- list.files(frameworks, full.names = TRUE)
    candidates <- file.path(versions, "csc.exe")
    candidates[file.exists(candidates)]

  })

  if (empty(csc) || !file.exists(csc))
    stop("could not find csc.exe")


  code <- "
class GenerateUUID {
  static void Main(string[] args) {
    System.Console.WriteLine(System.Guid.NewGuid().ToString());
  }
}
"

  renv_scope_tempdir("renv-uuid-")
  writeLines(code, con = "program.cs")

  renv_system_exec(
    csc[[1]],
    c("/nologo", "/out:program.exe", "program.cs"),
    "compiling uuid helper"
  )

  renv_system_exec("program.exe", character(), "generating uuid")

}


# imbue.R --------------------------------------------------------------------


#' Imbue an renv Installation
#'
#' Imbue an renv installation into a project, thereby making the requested
#' version of renv available within.
#'
#' Normally, this function does not need to be called directly by the user; it
#' will be invoked as required by [init()] and [activate()].
#'
#' @inherit renv-params
#'
#' @param version The version of renv to install. If `NULL`, the version
#'   of renv currently installed will be used. The requested version of
#'   renv will be retrieved from the renv public GitHub repository,
#'   at <https://github.com/rstudio/renv>.
#'
#' @param quiet Boolean; avoid printing output during install of renv?
#'
imbue <- function(project = NULL,
                  version = NULL,
                  quiet   = FALSE)
{
  renv_scope_error_handler()
  project <- renv_project_resolve(project)

  renv_scope_options(renv.verbose = !quiet)

  vtext <- version %||% renv_metadata_version()
  writef("Installing renv [%s] ...", vtext)
  status <- renv_imbue_impl(project, version)
  writef("- Done! renv has been successfully installed.")

  invisible(status)

}

renv_imbue_impl <- function(project,
                            library = NULL,
                            version = NULL,
                            force = FALSE)
{
  # don't imbue during tests unless explicitly requested
  if (renv_tests_running() && !force)
    return(NULL)

  # resolve library path
  library <- library %||% renv_paths_library(project = project)
  ensure_directory(library)

  # NULL version means imbue this version of renv
  if (is.null(version))
    return(renv_imbue_self(project, library = library))

  # otherwise, try to download and install the requested version
  # of renv from GitHub
  remote <- paste("rstudio/renv", version %||% "main", sep = "@")
  record <- renv_remotes_resolve(remote)
  records <- list(renv = record)

  renv_scope_restore(
    project = project,
    library = library,
    records = records,
    packages = "renv",
    recursive = FALSE
  )

  records <- retrieve("renv")
  renv_install_impl(records)

  record <- records[["renv"]]
  invisible(record)
}

renv_imbue_self <- function(project,
                            library = NULL,
                            source = NULL)
{
  # construct source, target paths
  # (check if 'renv' is loaded to handle embedded case)
  source <- source %||% {
    if ("renv" %in% loadedNamespaces()) {
      renv_namespace_path("renv")
    } else {
      renv_package_find("renv")
    }
  }

  if (!file.exists(source))
    stop("internal error: could not find where 'renv' is installed")

  library <- library %||% renv_paths_library(project = project)
  target <- file.path(library, "renv")
  if (renv_file_same(source, target))
    return(TRUE)

  type <- renv_package_type(source, quiet = TRUE)
  case(
    type == "source" ~ renv_imbue_self_source(source, target),
    type == "binary" ~ renv_imbue_self_binary(source, target)
  )

}

renv_imbue_self_source <- function(source, target) {

  # if the package already exists, just skip
  if (file.exists(target))
    return(TRUE)

  # otherwise, install it
  library <- dirname(target)
  ensure_directory(library)
  r_cmd_install("renv", source, library)

}

renv_imbue_self_binary <- function(source, target) {
  ensure_parent_directory(target)
  renv_file_copy(source, target, overwrite = TRUE)
}


# imports.R ------------------------------------------------------------------


#' @importFrom tools
#'   file_ext pskill psnice write_PACKAGES
#'
#' @importFrom utils
#'   adist available.packages browseURL citation contrib.url download.file
#'   download.packages file.edit getCRANmirrors head help install.packages
#'   installed.packages modifyList old.packages packageDescription
#'   packageVersion read.table remove.packages Rprof sessionInfo summaryRprof
#'   str tail tar toBibtex untar update.packages unzip URLencode zip
NULL


# index.R --------------------------------------------------------------------


the$index <- new.env(parent = emptyenv())

index <- function(scope, key = NULL, value = NULL, limit = 3600L) {

  enabled <- renv_index_enabled(scope, key)
  if (!enabled)
    return(value)

  # resolve the root directory
  root <- renv_paths_index(scope)

  # make sure the directory we're indexing exists
  memoize(
    key   = root,
    value = ensure_directory(root, umask = "0")
  )

  # make sure the directory is readable / writable
  # otherwise, attempts to lock will fail
  # https://github.com/rstudio/renv/issues/1171
  if (!renv_index_writable(root))
    return(value)

  # resolve other variables
  key <- if (!is.null(key)) renv_index_encode(key)
  now <- as.integer(Sys.time())

  # acquire index lock
  lockfile <- file.path(root, "index.lock")
  renv_scope_lock(lockfile)

  # load the index file
  index <- tryCatch(renv_index_load(root, scope), error = identity)
  if (inherits(index, "error"))
    return(value)

  # return index as-is when key is NULL
  if (is.null(key))
    return(index)

  # check for an index entry, and return it if it exists
  item <- renv_index_get(root, scope, index, key, now, limit)
  if (!is.null(item))
    return(item)

  # otherwise, update the index
  renv_index_set(root, scope, index, key, value, now, limit)

}

renv_index_load <- function(root, scope) {

  filebacked(
    context  = "renv_index_load",
    path     = file.path(root, "index.json"),
    callback = renv_index_load_impl
  )

}

renv_index_load_impl <- function(path) {

  json <- tryCatch(
    withCallingHandlers(
      renv_json_read(path),
      warning = function(w) invokeRestart("muffleWarning")
    ),
    error = identity
  )

  if (inherits(json, "error")) {
    unlink(path)
    return(list())
  }

  json

}

renv_index_get <- function(root, scope, index, key, now, limit) {

  # check for index entry
  entry <- index[[key]]
  if (is.null(entry))
    return(NULL)

  # see if it's expired
  if (renv_index_expired(entry, now, limit))
    return(NULL)

  # check for in-memory cached value
  value <- the$index[[scope]][[key]]
  if (!is.null(value))
    return(value)

  # otherwise, try to read from disk
  data <- file.path(root, entry$data)
  if (!file.exists(data))
    return(NULL)

  # read data from disk
  value <- readRDS(data)

  # add to in-memory cache
  the$index[[scope]] <-
    the$index[[scope]] %||%
    new.env(parent = emptyenv())

  the$index[[scope]][[key]] <- value

  # return value
  value

}

renv_index_set <- function(root, scope, index, key, value, now, limit) {

  # force promises
  force(value)

  # files being written here should be shared
  renv_scope_umask("0")

  # write data into index
  data <- tempfile("data-", tmpdir = root, fileext = ".rds")
  ensure_parent_directory(data)
  saveRDS(value, file = data, version = 2L)

  # clean up stale entries
  index <- renv_index_clean(root, scope, index, now, limit)

  # add index entry
  index[[key]] <- list(time = now, data = basename(data))

  # update index file
  path <- file.path(root, "index.json")
  ensure_parent_directory(path)

  # write to tempfile and then copy to minimize risk of collisions
  tempfile <- tempfile(".index-", tmpdir = dirname(path), fileext = ".json")
  renv_json_write(index, file = tempfile)
  file.rename(tempfile, path)

  # return value
  value

}

renv_index_encode <- function(key) {
  key <- stringify(key)
  memoize(key, renv_hash_text(key))
}

renv_index_clean <- function(root, scope, index, now, limit) {

  # figure out what cache entries have expired
  ok <- enum_lgl(
    index,
    renv_index_clean_impl,
    root  = root,
    scope = scope,
    index = index,
    now   = now,
    limit = limit
  )

  # return the existing cache entries
  index[ok]

}

renv_index_clean_impl <- function(key, entry, root, scope, index, now, limit) {

  # check if cache entry has expired
  expired <- renv_index_expired(entry, now, limit)
  if (!expired)
    return(TRUE)

  # remove from in-memory cache
  cache <- the$index[[scope]]
  cache[[key]] <- NULL

  # remove from disk
  unlink(file.path(root, entry$data), force = TRUE)

  FALSE

}

renv_index_expired <- function(entry, now, limit) {
  now - entry$time >= limit
}

renv_index_enabled <- function(scope, key) {
  getOption("renv.index.enabled", default = TRUE)
}

renv_index_writable <- function(root) {
  memoize(
    key   = root,
    value = unname(file.access(root, 7L) == 0L)
  )
}

# in case of emergency, break glass
renv_index_reset <- function(root = NULL) {
  root <- root %||% renv_paths_index()
  lockfiles <- list.files(root, pattern = "^index\\.lock$", full.names = TRUE)
  unlink(lockfiles)
}


# infrastructure.R -----------------------------------------------------------


# tools for writing / removing renv-related infrastructure
renv_infrastructure_write <- function(project = NULL,
                                      profile = NULL,
                                      version = NULL)
{
  # don't do anything in embedded mode
  if (renv_metadata_embedded())
    return()

  project <- renv_project_resolve(project)

  renv_infrastructure_write_profile(project, profile = profile)
  renv_infrastructure_write_rprofile(project)
  renv_infrastructure_write_rbuildignore(project)
  renv_infrastructure_write_gitignore(project)
  renv_infrastructure_write_activate(project, version = version)
}

renv_infrastructure_write_profile <- function(project, profile = NULL) {

  path <- renv_paths_renv("profile", profile = FALSE, project = project)
  profile <- renv_profile_normalize(profile)
  if (is.null(profile))
    return(unlink(path))

  ensure_parent_directory(path)
  writeLines(profile, con = path)

}

renv_infrastructure_write_rprofile <- function(project) {

  if (!config$autoloader.enabled())
    return()

  # NOTE: intentionally leave project NULL to compute relative path
  path <- renv_paths_activate(project = NULL)
  add <- sprintf("source(%s)", renv_json_quote(path))

  renv_infrastructure_write_entry_impl(
    add    = add,
    remove = character(),
    file   = file.path(project, ".Rprofile"),
    create = TRUE
  )

}

renv_infrastructure_write_rbuildignore <- function(project) {

  lines <- c("^renv$", "^renv\\.lock$")

  if (file.exists(file.path(project, "requirements.txt")))
    lines <- c(lines, "^requirements\\.txt$")

  if (file.exists(file.path(project, "environment.yml")))
    lines <- c(lines, "^environment\\.yml$")

  renv_infrastructure_write_entry_impl(
    add    = lines,
    remove = character(),
    file   = file.path(project, ".Rbuildignore"),
    create = renv_project_type(project) == "package"
  )

}

renv_infrastructure_write_gitignore <- function(project) {

  if (!settings$vcs.manage.ignores())
    return()

  add    <- stack(mode = "character")
  remove <- stack(mode = "character")

  stk <- if (settings$vcs.ignore.library()) add else remove
  stk$push("library/")

  stk <- if (settings$vcs.ignore.local()) add else remove
  stk$push("local/")

  stk <- if (settings$vcs.ignore.cellar()) add else remove
  stk$push("cellar/")

  add$push("lock/", "python/", "sandbox/", "staging/")

  renv_infrastructure_write_entry_impl(
    add    = as.character(add$data()),
    remove = as.character(remove$data()),
    file   = renv_paths_renv(".gitignore", project = project),
    create = TRUE
  )

}

renv_infrastructure_write_activate <- function(project = NULL,
                                               version = NULL,
                                               create  = TRUE)
{
  project <- renv_project_resolve(project)
  version <- version %||% renv_activate_version(project)
  sha <- attr(version, "sha", exact = TRUE)

  source <- system.file("resources/activate.R", package = "renv")
  target <- renv_paths_activate(project = project)

  if (!create && !file.exists(target))
    return(FALSE)

  template <- renv_file_read(source)
  new <- renv_template_replace(
    text = template,
    replacements = list(
      version = stringify(as.character(version)),
      sha = stringify(sha)
    ),
    format = "..%s.."
  )

  if (file.exists(target)) {
    old <- renv_file_read(target)
    if (old == new)
      return(TRUE)
  }

  ensure_parent_directory(target)
  writeLines(new, con = target)
}


renv_infrastructure_write_entry_impl <- function(add, remove, file, create) {

  # check to see if file doesn't exist
  if (!file.exists(file)) {

    # if we're not forcing file creation, just bail
    if (!create)
      return(TRUE)

    # otherwise, write the file
    ensure_parent_directory(file)
    writeLines(add, con = file)
    return(TRUE)

  }

  # if the file already has the requested line, nothing to do
  before <- readLines(file, warn = FALSE)
  after <- before

  # add requested entries
  for (item in rev(add)) {

    # check to see if the requested line exists (either commented
    # or uncommented). if it exists, we'll attempt to uncomment
    # any commented lines
    cpattern <- sprintf("^\\s*#?\\s*\\Q%s\\E\\s*(?:#|\\s*$)", item)
    matches <- grepl(cpattern, after, perl = TRUE)
    if (any(matches))
      after[matches] <- gsub("^(\\s*)#\\s*", "\\1", after[matches])
    else
      after <- c(item, after)

  }

  # remove requested entries
  for (item in rev(remove)) {
    pattern <- sprintf("^\\s*\\Q%s\\E\\s*(?:#|\\s*$)", item)
    matches <- grepl(pattern, after, perl = TRUE)
    if (any(matches)) {
      replacement <- gsub("^(\\s*)", "\\1# ", after[matches], perl = TRUE)
      after[matches] <- replacement
    }
  }

  # write to file if we have changes
  if (!identical(before, after))
    writeLines(after, con = file)

  TRUE

}



renv_infrastructure_remove <- function(project = NULL) {
  project <- renv_project_resolve(project)

  renv_infrastructure_remove_rprofile(project)
  renv_infrastructure_remove_rbuildignore(project)

  unlink(file.path(project, "renv"), recursive = TRUE)
}


renv_infrastructure_remove_rprofile <- function(project) {

  # NOTE: intentionally leave project NULL to compute relative path
  path <- renv_paths_activate(project = NULL)
  line <- sprintf("source(%s)", renv_json_quote(path))

  renv_infrastructure_remove_entry_impl(
    line      = line,
    file      = file.path(project, ".Rprofile"),
    removable = TRUE
  )

}

renv_infrastructure_remove_rbuildignore <- function(project) {

  renv_infrastructure_remove_entry_impl(
    line      = "^renv$",
    file      = file.path(project, ".Rbuildignore"),
    removable = FALSE
  )

}

renv_infrastructure_remove_entry_impl <- function(line, file, removable) {

  # if the file doesn't exist, nothing to do
  if (!file.exists(file))
    return(TRUE)

  # find and comment out the line
  contents <- readLines(file, warn = FALSE)
  pattern <- sprintf("^\\s*\\Q%s\\E\\s*(?:#|\\s*$)", line)
  matches <- grepl(pattern, contents, perl = TRUE)

  # if this file is removable, check to see if we matched all non-blank
  # lines; if so, remove the file
  if (removable) {
    rest <- contents[!matches]
    if (all(grepl("^\\s*$", rest)))
      return(unlink(file))
  }

  # otherwise, just mutate the file
  replacement <- gsub("^(\\s*)", "\\1# ", contents[matches], perl = TRUE)
  contents[matches] <- replacement
  writeLines(contents, con = file)

  TRUE

}




# init.R ---------------------------------------------------------------------


the$init_running <- FALSE

#' Use renv in a project
#'
#' @description
#' Call `renv::init()` to start using renv in the current project. This will:
#'
#' 1. Set up project infrastructure (as described in [scaffold()]) including
#'    the project library and the `.Rprofile` that ensures renv will be
#'    used in all future sessions.
#'
#' 1. Discover the packages that you currently and install them into an
#'    project library (as described in [hydrate()]).
#'
#' 1. Create a lockfile that records the state of the project library so it
#'    can be restored by others (as described in [snapshot()]).
#'
#' 1. Restarts R (if running inside RStudio).
#'
#' If you call `init()` on a project that already uses renv, it will attempt
#' to do the right thing: it will restore the project library if it's missing,
#' or otherwise ask you what to do.
#'
#' # Repositories
#'
#' If the default \R repositories have not already been set, renv will use
#' the [Posit Public Package Manager](https://packagemanager.posit.co/) CRAN
#' mirror for package installation. The primary benefit to using this mirror is
#' that it can provide pre-built binaries for \R packages on a variety of
#' commonly-used Linux distributions. This behavior can be configured or
#' disabled if desired -- see the options in [renv::config()] for more details.
#'
#' @inherit renv-params
#'
#' @param project The project directory. When `NULL` (the default), the current
#'   working directory will be used. The \R working directory will be
#'   changed to match the requested project directory.
#'
#' @param settings A list of [settings] to be used with the newly-initialized
#'   project.
#'
#' @param bare Boolean; initialize the project without attempting to discover
#'   and install R package dependencies?
#'
#' @param force Boolean; force initialization? By default, renv will refuse
#'   to initialize the home directory as a project, to defend against accidental
#'   mis-usages of `init()`.
#'
#' @param repos The \R repositories to be used in this project.
#'   See **Repositories** for more details.
#'
#' @param bioconductor The version of Bioconductor to be used with this project.
#'   Setting this may be appropriate if renv is unable to determine that your
#'   project depends on a package normally available from Bioconductor. Set this
#'   to `TRUE` to use the default version of Bioconductor recommended by the
#'   BiocManager package.
#'
#' @param load Boolean; should the project be loaded after it is initialized?
#'
#' @param restart Boolean; attempt to restart the \R session after initializing
#'   the project? A session restart will be attempted if the `"restart"` \R
#'   option is set by the frontend embedding \R.
#'
#' @export
#'
#' @example examples/examples-init.R
init <- function(project = NULL,
                 ...,
                 profile      = NULL,
                 settings     = NULL,
                 bare         = FALSE,
                 force        = FALSE,
                 repos        = NULL,
                 bioconductor = NULL,
                 load         = TRUE,
                 restart      = interactive())
{
  renv_consent_check()
  renv_scope_error_handler()
  renv_dots_check(...)

  renv_scope_binding(the, "init_running", TRUE)

  project <- renv_path_normalize(project %||% getwd())
  renv_project_lock(project = project)

  # initialize profile
  if (!is.null(profile))
    renv_profile_set(profile)

  # normalize repos
  repos <- renv_repos_normalize(repos %||% renv_init_repos())

  # form path to lockfile, library
  library  <- renv_paths_library(project = project)
  lockfile <- renv_lockfile_path(project)

  # ask user what type of project this is
  type <- settings$snapshot.type %||% renv_init_type(project)
  settings$snapshot.type <- type

  # initialize bioconductor pieces
  biocver <- renv_init_bioconductor(bioconductor, project)
  if (!is.null(biocver)) {

    # make sure a Bioconductor package manager is installed
    renv_bioconductor_init(library = library)

    # retrieve bioconductor repositories appropriate for this project
    repos <- renv_bioconductor_repos(project = project, version = biocver)

    # notify user
    writef("- Using Bioconductor version '%s'.", biocver)
    settings[["bioconductor.version"]] <- biocver

  }

  # prepare and move into project directory
  renv_init_validate_project(project, force)
  renv_init_settings(project, settings)

  # for bare inits, just activate the project
  if (bare) {
    renv_imbue_impl(project)
    return(renv_init_fini(project, profile, load, restart))
  }

  # compute and cache dependencies to (a) reveal problems early and (b) compute once
  deps <- renv_snapshot_dependencies(project, type = type, dev = TRUE)

  # determine appropriate action
  action <- renv_init_action(project, library, lockfile, bioconductor)
  cancel_if(empty(action) || identical(action, "cancel"))

  # compute library paths for this project
  libpaths <- renv_init_libpaths(project = project)

  # perform the action
  if (action == "init") {
    renv_scope_options(renv.config.dependency.errors = "ignored")
    renv_imbue_impl(project, library = library)
    hydrate(library = library, repos = repos, prompt = FALSE, report = FALSE, project = project)
    snapshot(library = libpaths, repos = repos, prompt = FALSE, project = project)
  } else if (action == "restore") {
    ensure_directory(library)
    restore(project = project, library = libpaths, repos = repos, prompt = FALSE)
  }

  # activate the newly-hydrated project
  renv_init_fini(project, profile, load, restart)

}

renv_init_fini <- function(project, profile, load, restart) {

  renv_activate_impl(
    project = project,
    profile = profile,
    version = renv_metadata_version(),
    load    = load,
    restart = restart
  )

  invisible(project)

}

renv_init_action <- function(project, library, lockfile, bioconductor) {

  # if the user has asked for bioconductor, treat this as a re-initialization
  if (!is.null(bioconductor))
    return("init")

  # figure out appropriate action
  case(

    # if both the library and lockfile exist, ask user for intended action
    file.exists(lockfile)
      ~ renv_init_action_conflict_lockfile(project, library, lockfile),

    # if a private library exists but no lockfile, ask whether we should use it
    file.exists(library)
      ~ renv_init_action_conflict_library(project, library, lockfile),

    # otherwise, we just want to initialize the project
    ~ "init"

  )

}

renv_init_action_conflict_lockfile <- function(project, library, lockfile) {

  if (!interactive())
    return("nothing")

  title <- "This project already has a lockfile. What would you like to do?"
  choices <- c(
    restore = "Restore the project from the lockfile.",
    init    = "Discard the lockfile and re-initialize the project.",
    nothing = "Activate the project without snapshotting or installing any packages.",
    cancel  = "Abort project initialization."
  )

  selection <- tryCatch(
    utils::select.list(choices, title = title, graphics = FALSE),
    interrupt = identity
  )

  if (inherits(selection, "interrupt"))
    return(NULL)

  names(selection)

}

renv_init_action_conflict_library <- function(project, library, lockfile) {

  if (!interactive())
    return("nothing")

  title <- "This project already has a private library. What would you like to do?"
  choices <- c(
    nothing = "Activate the project and use the existing library.",
    init    = "Re-initialize the project with a new library.",
    cancel  = "Abort project initialization."
  )

  selection <- tryCatch(
    utils::select.list(choices, title = title, graphics = FALSE),
    interrupt = identity
  )

  if (inherits(selection, "interrupt"))
    return(NULL)

  names(selection)

}

renv_init_validate_project <- function(project, force) {

  # allow all project directories when force = TRUE
  if (force)
    return(TRUE)

  # disallow attempts to initialize renv in the home directory
  home <- path.expand("~/")
  msg <- if (renv_file_same(project, home))
    "refusing to initialize project in home directory"
  else if (renv_path_within(home, project))
    sprintf("refusing to initialize project in directory '%s'", project)

  if (!is.null(msg)) {
    msg <- paste(msg, "-- use renv::init(force = TRUE) to override")
    stopf(msg)
  }

}

renv_init_settings <- function(project, settings) {

  defaults <- renv_settings_get(project)
  merged <- renv_settings_merge(defaults, settings)
  renv_settings_persist(project, merged)
  invisible(merged)

}

renv_init_bioconductor <- function(bioconductor, project) {

  # if we're re-initializing a project that appears to depend
  # on Bioconductor, then use the latest Bioconductor release
  if (is.null(bioconductor)) {
    lockpath <- renv_paths_lockfile(project = project)
    if (file.exists(lockpath)) {
      lockfile <- renv_lockfile_read(lockpath)
      bioconductor <- !is.null(lockfile$Bioconductor)
    }
  }

  # resolve bioconductor argument
  case(
    is.character(bioconductor)     ~ bioconductor,
    identical(bioconductor, TRUE)  ~ renv_bioconductor_version(project, refresh = TRUE),
    identical(bioconductor, FALSE) ~ NULL
  )

}

renv_init_repos <- function() {

  # if PPM is disabled, just use default repositories
  repos <- convert(getOption("repos"), "list")
  if (!renv_ppm_enabled())
    return(repos)

  enabled <- config$ppm.default()
  if (!enabled)
    return(repos)

  # if we're using the global CDN from RStudio, use PPM instead
  rstudio <- attr(repos, "RStudio", exact = TRUE)
  if (identical(rstudio, TRUE)) {
    repos[["CRAN"]] <- config$ppm.url()
    return(repos)
  }

  # otherwise, check for some common 'default' CRAN settings
  cran <- repos[["CRAN"]]
  if (is.character(cran) && length(cran) == 1L) {
    cran <- sub("/*$", "", cran)
    defaults <- c(
      "@CRAN@",
      "https://cloud.R-project.org",
      "https://cran.rstudio.com",
      "https://cran.rstudio.org"
    )

    if (tolower(cran) %in% tolower(defaults)) {
      repos[["CRAN"]] <- config$ppm.url()
      return(repos)
    }

  }

  # repos appears to have been configured separately; just use it
  repos

}

renv_init_type <- function(project) {

  # check if the user has already requested a snapshot type
  type <- renv_settings_get(project, name = "snapshot.type", default = NULL)
  if (!is.null(type))
    return(type)

  # if we don't have a DESCRIPTION file, use the default
  if (!file.exists(file.path(project, "DESCRIPTION")))
    return(settings$snapshot.type(project = project))

  # otherwise, ask the user if they want to explicitly enumerate their
  # R package dependencies in the DESCRIPTION file
  choice <- menu(

    title = c(
      "This project contains a DESCRIPTION file.",
      "Which files should renv use for dependency discovery in this project?"
    ),

    choices = c(
      explicit = "Use only the DESCRIPTION file. (explicit mode)",
      implicit = "Use all files in this project. (implicit mode)"
    )

  )

  if (identical(choice, "cancel"))
    cancel()

  writef("- Using '%s' snapshot type. Please see `?renv::snapshot` for more details.\n", choice)
  choice

}


# install.R ------------------------------------------------------------------


# an explicitly-requested package type in a call to 'install()'
the$install_pkg_type <- NULL

# an explicitly-requested dependencies field in a call to 'install()'
the$install_dependency_fields <- NULL

# the formatted width of installation steps printed to the console
the$install_step_width <- 48L

#' Install packages
#'
#' @description
#' Install one or more \R packages, from a variety of remote sources.
#' `install()` uses the same machinery as [restore()] (i.e. it uses cached
#' packages where possible) but it does not respect the lockfile, instead
#' installing the latest versions available from CRAN.
#'
#' See `vignette("package-install")` for more details.
#'
#' # `Remotes`
#'
#' `install()` (called without arguments) will respect the `Remotes` field
#' of the `DESCRIPTION` file (if present). This allows you to specify places
#' to install a package other than the latest version from CRAN.
#' See <https://remotes.r-lib.org/articles/dependencies.html> for details.
#'
#' # Bioconductor
#'
#' Packages from Bioconductor can be installed by using the `bioc::` prefix.
#' For example,
#'
#' ```
#' renv::install("bioc::Biobase")
#' ```
#'
#' will install the latest-available version of Biobase from Bioconductor.
#'
#' renv depends on BiocManager (or, for older versions of \R, BiocInstaller)
#' for the installation of packages from Bioconductor. If these packages are
#' not available, renv will attempt to automatically install them before
#' fulfilling the installation request.
#'
#' @inherit renv-params
#' @param packages Either `NULL` (the default) to install all packages required
#'  by the project, or a character vector of packages to install. renv
#'  supports a subset of the remotes syntax used for package installation,
#'  e.g:
#'
#'  * `pkg`: install latest version of `pkg` from CRAN.
#'  * `pkg@version`: install specified version of `pkg` from CRAN.
#'  * `username/repo`: install package from GitHub
#'  * `bioc::pkg`: install `pkg` from Bioconductor.
#'
#'  See <https://remotes.r-lib.org/articles/dependencies.html> and the examples
#'  below for more details.
#'
#'  renv deviates from the remotes spec in one important way: subdirectories
#'  are separated from the main repository specification with a `:`, not `/`.
#'  So to install from the `subdir` subdirectory of GitHub package
#'  `username/repo` you'd use `"username/repo:subdir`.
#'
#' @return A named list of package records which were installed by renv.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # install the latest version of 'digest'
#' renv::install("digest")
#'
#' # install an old version of 'digest' (using archives)
#' renv::install("digest@@0.6.18")
#'
#' # install 'digest' from GitHub (latest dev. version)
#' renv::install("eddelbuettel/digest")
#'
#' # install a package from GitHub, using specific commit
#' renv::install("eddelbuettel/digest@@df55b00bff33e945246eff2586717452e635032f")
#'
#' # install a package from Bioconductor
#' # (note: requires the BiocManager package)
#' renv::install("bioc::Biobase")
#'
#' # install a package, specifying path explicitly
#' renv::install("~/path/to/package")
#'
#' # install packages as declared in the project DESCRIPTION file
#' renv::install()
#'
#' }
install <- function(packages = NULL,
                    ...,
                    library      = NULL,
                    type         = NULL,
                    rebuild      = FALSE,
                    repos        = NULL,
                    prompt       = interactive(),
                    dependencies = NULL,
                    project      = NULL)
{
  renv_consent_check()
  renv_scope_error_handler()

  # allow user to provide additional package names as part of '...'
  if (!missing(...)) {
    dots <- list(...)
    names(dots) <- names(dots) %||% rep.int("", length(dots))
    packages <- c(packages, dots[!nzchar(names(dots))])
  }

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)
  renv_scope_verbose_if(prompt)

  # handle 'dependencies'
  if (!is.null(dependencies)) {
    fields <- renv_description_dependency_fields(dependencies, project = project)
    renv_scope_binding(the, "install_dependency_fields", fields)
  }

  # set up library paths
  libpaths <- renv_libpaths_resolve(library)
  renv_scope_libpaths(libpaths)

  # check for explicitly-provided type -- we handle this specially for PPM
  if (!is.null(type)) {
    renv_scope_binding(the, "install_pkg_type", type)
    renv_scope_options(pkgType = type)
  }

  # override repositories if requested
  repos <- repos %||% config$repos.override()
  if (length(repos))
    renv_scope_options(repos = repos)

  # if users have requested the use of pak, delegate there
  if (config$pak.enabled() && !recursing()) {
    renv_pak_init()
    return(renv_pak_install(packages, libpaths, project))
  }

  # resolve remotes from explicitly-requested packages
  remotes <- if (length(packages)) {
    remotes <- map(packages, renv_remotes_resolve)
    names(remotes) <- map_chr(remotes, `[[`, "Package")
    remotes
  }

  # figure out which packages we should install
  packages <- names(remotes) %||% renv_snapshot_dependencies(project, dev = TRUE)
  if (empty(packages)) {
    writef("- There are no packages to install.")
    return(invisible(list()))
  }

  # add bioconductor packages if necessary
  if (renv_bioconductor_required(remotes)) {
    bioc <- c(renv_bioconductor_manager(), "BiocVersion")
    packages <- unique(c(packages, bioc))
  }

  # don't update renv unless it was explicitly requested
  if (!"renv" %in% names(remotes))
    packages <- setdiff(packages, "renv")

  # start building a list of records; they should be resolved this priority:
  #
  # 1. explicit requests from the user
  # 2. remotes declarations from the DESCRIPTION file
  # 3. existing version in library, if any
  # 4. fallback to package repositories
  #
  # we overlay 1 and 2 here, and then do 3 and 4 dynamically if required
  # during the retrieve + install stages
  records <- overlay(renv_project_remotes(project), remotes)

  # run install preflight checks
  if (!renv_install_preflight(project, libpaths, records))
    cancel_if(prompt && !proceed())

  # we're now ready to start installation
  renv_scope_restore(
    project  = project,
    library  = renv_libpaths_active(),
    packages = names(remotes),
    records  = records,
    rebuild  = rebuild
  )

  # retrieve packages
  records <- retrieve(packages)
  if (empty(records)) {
    writef("- There are no packages to install.")
    return(invisible(list()))
  }

  if (prompt || renv_verbose()) {
    renv_install_report(records, library = renv_libpaths_active())
    cancel_if(prompt && !proceed())
  }

  # install retrieved records
  before <- Sys.time()
  renv_install_impl(records)
  after <- Sys.time()

  time <- renv_difftime_format(difftime(after, before))
  n <- length(records)
  writef("Successfully installed %s in %s.", nplural("package", n), time)

  # check loaded packages and inform user if out-of-sync
  renv_install_postamble(names(records))

  invisible(records)
}

renv_install_impl <- function(records) {

  staged <- renv_config_install_staged()

  writef(header("Installing packages"))

  if (staged)
    renv_install_staged(records)
  else
    renv_install_default(records)

  invisible(TRUE)

}

renv_install_staged <- function(records) {

  # get current libpaths
  libpaths <- renv_libpaths_all()

  # set up a dummy library path for installation
  templib <- renv_install_staged_library_path()
  defer(unlink(templib, recursive = TRUE))
  renv_scope_libpaths(c(templib, libpaths))

  # perform the install
  renv_install_default(records)

  # migrate packages into true library
  library <- nth(libpaths, 1L)
  sources <- list.files(templib, full.names = TRUE)
  targets <- file.path(library, basename(sources))
  names(targets) <- sources
  enumerate(targets, renv_file_move, overwrite = TRUE)

  # clear filebacked cache entries
  descpaths <- file.path(targets, "DESCRIPTION")
  renv_filebacked_clear("renv_description_read", descpaths)
  renv_filebacked_clear("renv_hash_description", descpaths)

  invisible(targets)

}

renv_install_staged_library_path_impl <- function() {

  # get current library path
  libpath <- renv_libpaths_active()

  # retrieve current project, library path
  stagedlib <- local({

    # allow user configuration of staged library location
    override <- Sys.getenv("RENV_PATHS_LIBRARY_STAGING", unset = NA)
    if (!is.na(override))
      return(override)

    # if we have an active project, use that path
    project <- renv_project_get(default = NULL)
    if (!is.null(project))
      return(renv_paths_renv("staging", project = project))

    # otherwise, stage within library path
    file.path(libpath, ".renv")

  })

  # attempt to create it
  ok <- catch(ensure_directory(stagedlib))
  if (inherits(ok, "error"))
    return(tempfile("renv-staging-"))

  # resolve a unique staging directory in this path
  # we want to keep paths short just in case; it's easy to blow up the
  # path length limit (hence we don't use tempfile below)
  for (i in 1:100) {
    path <- file.path(stagedlib, i)
    if (dir.create(path, showWarnings = FALSE))
      return(path)
  }

  # all else fails, use tempfile
  tempfile("renv-staging-")

}

# NOTE: on Windows, installing packages into very long paths
# can fail, as R's internal unzip utility does not handle
# long Windows paths well. in addition, an renv project's
# library path tends to be long, exasperating the issue.
# for that reason, we try to use a shorter staging directory
#
# part of the challenge here is that the R temporary directory
# and R library path might reside on different mounts, and so
# we may want to try and avoid installing on one mount and then
# copying to another mount (as that could be slow).
#
# note that using the renv folder might be counter-productive,
# since users will want to use renv in projects sync'ed via
# OneDrive and friends, and we don't want those to lock files
# in the staging directory
renv_install_staged_library_path <- function() {

  # compute path
  path <- renv_install_staged_library_path_impl()

  # create library directory
  ensure_directory(path)

  # try to make sure it has the same permissions as the library itself
  if (!renv_platform_windows()) {
    libpath <- renv_libpaths_active()
    umask <- Sys.umask("0")
    defer(Sys.umask(umask))
    info <- renv_file_info(libpath)
    Sys.chmod(path, info$mode)
  }

  # return the created path
  return(path)

}

renv_install_default <- function(records) {
  state <- renv_restore_state()
  handler <- state$handler

  for (record in records) {
    package <- record$Package
    handler(package, renv_install_package(record))
  }
}

renv_install_package <- function(record) {

  # get active project (if any)
  state <- renv_restore_state()
  project <- state$project

  # figure out whether we can use the cache during install
  # use library path recorded in restore state as staged installs will have
  # mutated the library path, placing a staging library at the front
  library <- renv_restore_state("library")
  linkable <- renv_cache_linkable(project = project, library = library)
  linker <- if (linkable) renv_file_link else renv_file_copy

  cacheable <-
    renv_cache_config_enabled(project = project) &&
    renv_record_cacheable(record) &&
    !renv_restore_rebuild_required(record)

  if (cacheable) {

    # check for cache entry and install if there
    path <- renv_cache_find(record)
    if (renv_cache_package_validate(path))
      return(renv_install_package_cache(record, path, linker))

  }

  # install the package
  before <- Sys.time()
  withCallingHandlers(
    renv_install_package_impl(record),
    error = function(e) writef("FAILED")
  )
  after <- Sys.time()

  path <- record$Path
  type <- renv_package_type(path, quiet = TRUE)
  feedback <- renv_install_package_feedback(path, type)


  # link into cache
  if (renv_cache_config_enabled(project = project)) {
    renv_cache_synchronize(record, linkable = linkable)
    feedback <- paste0(feedback, " and cached")
  }

  elapsed <- difftime(after, before, units = "auto")
  renv_install_step_ok(feedback, elapsed = elapsed)

  invisible()

}

renv_install_package_feedback <- function(path, type) {

  if (identical(type, "source"))
    return("built from source")

  if (renv_file_type(path, symlinks = FALSE) == "directory")
    return("copied local binary")

  "installed binary"

}

renv_install_package_cache <- function(record, cache, linker) {

  if (renv_install_package_cache_skip(record, cache))
    return(TRUE)

  library <- renv_libpaths_active()
  target <- file.path(library, record$Package)

  # back up the previous installation if needed
  callback <- renv_file_backup(target)
  defer(callback())

  # report successful link to user
  renv_install_step_start("Installing", record$Package)

  before <- Sys.time()
  linker(cache, target)
  after <- Sys.time()

  type <- case(
    identical(linker, renv_file_copy) ~ "copied from cache",
    identical(linker, renv_file_link) ~ "linked from cache"
  )

  elapsed <- difftime(after, before, units = "auto")
  renv_install_step_ok(type, elapsed = elapsed)

  return(TRUE)

}

renv_install_package_cache_skip <- function(record, cache) {

  # don't skip if installation was explicitly requested
  if (record$Package %in% renv_restore_state("packages"))
    return(FALSE)

  # check for matching cache + target paths
  library <- renv_restore_state("library") %||% renv_libpaths_active()
  target <- file.path(library, record$Package)

  renv_file_same(cache, target)

}

renv_install_package_impl_prebuild <- function(record, path, quiet) {

  # check whether user wants us to build before install
  if (!identical(config$install.build(), TRUE))
    return(path)

  # if this package already appears to be built, nothing to do
  if (renv_package_built(path))
    return(path)

  # if this is an archive, we'll need to unpack it first
  info <- renv_file_info(path)
  if (identical(info$isdir, FALSE)) {

    # find the package directory
    files <- renv_archive_list(path)
    descpath <- grep("(?:^|/)DESCRIPTION$", files, value = TRUE)
    pkgpath <- dirname(descpath)[nchar(descpath) == min(nchar(descpath))]

    # extract to temporary directory
    exdir <- tempfile("renv-build-")
    ensure_directory(exdir)
    renv_archive_decompress(path, exdir = exdir)

    # update path to package
    path <- file.path(exdir, pkgpath)

    # and ensure we build in this directory
    renv_scope_wd(path)

  }

  # if this package depends on a VignetteBuilder that is not installed,
  # then we can't proceed
  descpath <- file.path(path, "DESCRIPTION")
  desc <- renv_description_read(descpath)
  builder <- desc[["VignetteBuilder"]]
  if (!is.null(builder) && !renv_package_installed(builder)) {
    fmt <- "Skipping package build: vignette builder '%s' is not installed"
    writef(fmt, builder)
    return(path)
  }

  renv_install_step_start("Building", record$Package)

  before <- Sys.time()
  package <- record$Package
  newpath <- r_cmd_build(package, path)
  after <- Sys.time()
  elapsed <- difftime(after, before, units = "auto")

  renv_install_step_ok("from source", elapsed = elapsed)

  newpath

}

renv_install_package_impl <- function(record, quiet = TRUE) {

  package <- record$Package

  # get path for package
  path <- record$Path

  # check if it's an archive (versus an unpacked directory)
  info <- renv_file_info(path)
  isarchive <- identical(info$isdir, FALSE)

  subdir <- record$RemoteSubdir %||% ""
  if (isarchive) {
    # re-pack archives if they appear to have their package
    # sources contained as part of a sub-directory
    path <- renv_package_unpack(package, path, subdir = subdir)
  } else if (nzchar(subdir)) {
    # for directories, we may need to use subdir to find the package path
    components <- c(path, subdir)
    path <- paste(components, collapse = "/")
  }

  # check whether we should build before install
  path <- renv_install_package_impl_prebuild(record, path, quiet)
  renv_install_step_start("Installing", record$Package)

  # run user-defined hooks before, after install
  options <- renv_install_package_options(package)
  before <- options$before.install %||% identity
  after  <- options$after.install %||% identity

  before(package)
  defer(after(package))

  # backup an existing installation of the package if it exists
  library <- renv_libpaths_active()
  destination <- file.path(library, package)
  callback <- renv_file_backup(destination)
  defer(callback())

  # normalize paths
  path <- renv_path_normalize(path, mustWork = TRUE)

  # get library path
  library <- renv_libpaths_active()

  # if a package already exists at that path, back it up first
  # this avoids problems with older versions of R attempting to
  # overwrite a pre-existing symlink
  #
  # https://github.com/rstudio/renv/issues/611
  installpath <- file.path(library, package)
  callback <- renv_file_backup(installpath)
  defer(callback())

  # if this failed for some reason, just remove it
  if (renv_file_broken(installpath))
    renv_file_remove(installpath)

  # if this is the path to an unpacked binary archive,
  # we can just copy the folder over
  isdir <- renv_file_type(path, symlinks = FALSE) == "directory"
  isbin <- renv_package_type(path, quiet = TRUE) == "binary"
  copyable <- isdir && isbin

  # shortcut via copying a binary directory if possible,
  # otherwise, install the package
  if (copyable)
    renv_file_copy(path, installpath, overwrite = TRUE)
  else
    r_cmd_install(package, path)

  # if we just installed a binary package, check that it can be loaded
  # (source packages are checked by default on install)
  withCallingHandlers(
    if (isbin) renv_install_test(package),
    error = function(err) unlink(installpath, recursive = TRUE)
  )

  # augment package metadata after install
  renv_package_augment(installpath, record)

  # return the path to the package
  invisible(installpath)

}

renv_install_test <- function(package) {

  # add escape hatch, just in case
  # (test binaries by default on Linux, but not Windows or macOS)
  enabled <- Sys.getenv("RENV_INSTALL_TEST_LOAD", unset = renv_platform_linux())
  if (!truthy(enabled))
    return(TRUE)

  # check whether we should skip installation testing
  opts <- r_cmd_install_option(package, c("install.opts", "INSTALL_opts"), FALSE)
  if (is.character(opts)) {
    flags <- unlist(strsplit(opts, "\\s+", perl = TRUE))
    if ("--no-test-load" %in% flags)
      return(TRUE)
  }

  # make sure we use the current library paths in the launched process
  rlibs <- paste(renv_libpaths_all(), collapse = .Platform$path.sep)
  renv_scope_envvars(R_LIBS = rlibs, R_LIBS_USER = "NULL", R_LIBS_SITE = "NULL")

  # also hide from user .Renviron files
  # https://github.com/wch/r-source/blob/1c0a2dc8ce6c05f68e1959ffbe6318a309277df3/src/library/tools/R/check.R#L273-L276
  renv_scope_envvars(R_ENVIRON_USER = "NULL")

  # make sure R_TESTS is unset here, just in case
  # https://github.com/wch/r-source/blob/1c0a2dc8ce6c05f68e1959ffbe6318a309277df3/src/library/tools/R/install.R#L76-L79
  renv_scope_envvars(R_TESTS = NULL)

  # the actual code we'll run in the other process
  # we use 'loadNamespace()' rather than 'library()' because some packages might
  # intentionally throw an error in their .onAttach() hooks
  # https://github.com/rstudio/renv/issues/1611
  code <- substitute({
    options(warn = 1L)
    loadNamespace(package)
  }, list(package = package))

  # write it to a tempfile
  script <- renv_scope_tempfile("renv-install-")
  writeLines(deparse(code), con = script)

  # check that the package can be loaded in a separate process
  renv_system_exec(
    command = R(),
    args    = c("--vanilla", "-s", "-f", renv_shell_path(script)),
    action  = sprintf("testing if '%s' can be loaded", package)
  )

  # return TRUE to indicate successful validation
  TRUE

}

renv_install_package_options <- function(package) {
  options <- getOption("renv.install.package.options")
  options[[package]]
}

# nocov start
renv_install_preflight_requirements <- function(records) {

  deps <- bapply(records, function(record) {
    renv_dependencies_discover_description(record$Path)
  }, index = "ParentPackage")

  splat <- split(deps, deps$Package)
  bad <- enumerate(splat, function(package, requirements) {

    # skip NULL records (should be handled above)
    record <- records[[package]]
    if (is.null(record))
      return(NULL)

    version <- record$Version

    # drop packages without explicit version requirement
    requirements <- requirements[nzchar(requirements$Require), ]
    if (nrow(requirements) == 0)
      return(NULL)

    # add in requested version
    requirements$RequestedVersion <- version

    # generate expressions to evaluate
    fmt <- "package_version('%s') %s package_version('%s')"
    code <- with(requirements, sprintf(fmt, RequestedVersion, Require, Version))
    parsed <- parse(text = code)
    ok <- map_lgl(parsed, eval, envir = baseenv())

    # return requirements that weren't satisfied
    requirements[!ok, ]

  })

  bad <- bind(unname(bad))
  if (empty(bad))
    return(TRUE)

  package  <- bad$ParentPackage
  requires <- sprintf("%s (%s %s)", bad$Package, bad$Require, bad$Version)
  actual   <- sprintf("%s %s", bad$Package, bad$RequestedVersion)

  fmt <- "Package '%s' requires '%s', but '%s' will be installed"
  text <- sprintf(fmt, format(package), format(requires), format(actual))
  if (renv_verbose()) {
    caution_bullets(
      "The following issues were discovered while preparing for installation:",
      text,
      "Installation of these packages may not succeed."
    )
  }

  if (interactive() && !proceed())
    return(FALSE)

  TRUE

}
# nocov end

renv_install_postamble <- function(packages) {

  # only diagnose packages currently loaded
  packages <- renv_vector_intersect(packages, loadedNamespaces())

  installed <- map_chr(packages, renv_package_version)
  loaded <- map_chr(packages, renv_namespace_version)

  caution_bullets(
    c("", "The following loaded package(s) have been updated:"),
    packages[installed != loaded],
    "Restart your R session to use the new versions."
  )

  TRUE

}

renv_install_preflight_unknown_source <- function(records) {
  renv_check_unknown_source(records)
}

renv_install_preflight_permissions <- function(library) {

  # try creating and deleting a directory in the library folder
  file <- renv_scope_tempfile(".renv-write-test-", tmpdir = library)
  dir.create(file, recursive = TRUE, showWarnings = FALSE)

  # check if we created the directory successfully
  info <- renv_file_info(file)
  if (identical(info$isdir, TRUE))
    return(TRUE)

  # nocov start
  if (renv_verbose()) {

    # construct header for message
    preamble <- "renv appears to be unable to access the requested library path:"

    # construct footer for message
    info <- as.list(Sys.info())
    fmt <- "Check that the '%s' user has read / write access to this directory."
    postamble <- sprintf(fmt, info$effective_user %||% info$user)

    # print it
    caution_bullets(
      preamble = preamble,
      values = library,
      postamble = postamble
    )

  }
  # nocov end

  FALSE

}

renv_install_preflight <- function(project, libpaths, records) {

  library <- nth(libpaths, 1L)

  all(
    renv_install_preflight_unknown_source(records),
    renv_install_preflight_permissions(library)
  )

}

renv_install_report <- function(records, library) {
  renv_pretty_print_records(
    "The following package(s) will be installed:",
    records,
    sprintf("These packages will be installed into %s.", renv_path_pretty(library))
  )
}

renv_install_step_start <- function(action, package) {
  message <- sprintf("- %s %s ... ", action, package)
  printf(format(message, width = the$install_step_width))
}

renv_install_step_ok <- function(..., elapsed = NULL) {
  renv_report_ok(
    message = paste(..., collapse = ""),
    elapsed = elapsed
  )
}


# installed-packages.R -------------------------------------------------------


installed_packages <- function(lib.loc = NULL,
                               priority = NULL,
                               field = NULL)
{
  lib.loc <- lib.loc %||% .libPaths()

  result <- dynamic(
    key = list(lib.loc = lib.loc, priority = priority),
    value = {
      packages <- installed.packages(lib.loc = lib.loc, priority = priority)
      as_data_frame(packages)
    }
  )

  take(result, field)

}


# isolate.R ------------------------------------------------------------------


#' Isolate a project
#'
#' Copy packages from the renv cache directly into the project library, so
#' that the project can continue to function independently of the renv cache.
#'
#' After calling `isolate()`, renv will still be able to use the cache on
#' future [install()]s and [restore()]s. If you'd prefer that renv copy
#' packages from the cache, rather than use symlinks, you can set the renv
#' configuration option:
#'
#' ```
#' options(renv.config.cache.symlinks = FALSE)
#' ```
#'
#' to force renv to copy packages from the cache, as opposed to symlinking
#' them. If you'd like to disable the cache altogether for a project, you can
#' use:
#'
#' ```
#' settings$use.cache(FALSE)
#' ```
#'
#' to explicitly disable the cache for the project.
#'
#' @inherit renv-params
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # isolate a project
#' renv::isolate()
#'
#' }
isolate <- function(project = NULL) {

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  if (renv_platform_windows())
    renv_isolate_windows(project)
  else
    renv_isolate_unix(project)

  invisible(project)

}

renv_isolate_unix <- function(project) {

  library <- renv_paths_library(project = project)
  targets <- list.files(library, full.names = TRUE)

  sources <- Sys.readlink(targets)
  islink <- !is.na(sources) & nzchar(sources)

  sources <- sources[islink]
  targets <- targets[islink]
  names(targets) <- sources

  if (length(targets)) {
    printf("- Copying packages into the private library ... ")
    unlink(targets)
    copy <- renv_progress_callback(renv_file_copy, length(targets))
    enumerate(targets, copy, overwrite = TRUE)
    writef("Done!")
  }

  writef("- This project has been isolated from the cache.")
  invisible(project)

}

renv_isolate_windows <- function(project) {

  library <- renv_paths_library(project = project)
  targets <- list.files(library, full.names = TRUE)

  sources <- map_chr(targets, renv_cache_path)
  names(targets) <- sources

  if (length(targets)) {
    printf("- Copying packages into the private library ... ")
    targets <- targets[file.exists(sources)]
    unlink(targets)
    copy <- renv_progress_callback(renv_file_copy, length(targets))
    enumerate(targets, copy, overwrite = TRUE)
    writef("Done!")
  }

  writef("- This project has been isolated from the cache.")
  invisible(project)

}


# json-read.R ----------------------------------------------------------------


renv_json_read <- function(file = NULL, text = NULL) {

  jlerr <- NULL

  # if jsonlite is loaded, use that instead
  if ("jsonlite" %in% loadedNamespaces()) {

    json <- catch(renv_json_read_jsonlite(file, text))
    if (!inherits(json, "error"))
      return(json)

    jlerr <- json

  }

  # otherwise, fall back to the default JSON reader
  json <- catch(renv_json_read_default(file, text))
  if (!inherits(json, "error"))
    return(json)

  # report an error
  if (!is.null(jlerr))
    stop(jlerr)
  else
    stop(json)

}

renv_json_read_jsonlite <- function(file = NULL, text = NULL) {
  text <- paste(text %||% read(file), collapse = "\n")
  jsonlite::fromJSON(txt = text, simplifyVector = FALSE)
}

renv_json_read_default <- function(file = NULL, text = NULL) {

  # find strings in the JSON
  text <- paste(text %||% read(file), collapse = "\n")
  pattern <- '["](?:(?:\\\\.)|(?:[^"\\\\]))*?["]'
  locs <- gregexpr(pattern, text, perl = TRUE)[[1]]

  # if any are found, replace them with placeholders
  replaced <- text
  strings <- character()
  replacements <- character()

  if (!identical(c(locs), -1L)) {

    # get the string values
    starts <- locs
    ends <- locs + attr(locs, "match.length") - 1L
    strings <- substring(text, starts, ends)

    # only keep those requiring escaping
    strings <- grep("[[\\]{}:]", strings, perl = TRUE, value = TRUE)

    # compute replacements
    replacements <- sprintf('"\032%i\032"', seq_along(strings))

    # replace the strings
    mapply(function(string, replacement) {
      replaced <<- sub(string, replacement, replaced, fixed = TRUE)
    }, strings, replacements)

  }

  # transform the JSON into something the R parser understands
  transformed <- replaced
  transformed <- gsub("{}", "`names<-`(list(), character())", transformed, fixed = TRUE)
  transformed <- gsub("[[{]", "list(", transformed, perl = TRUE)
  transformed <- gsub("[]}]", ")", transformed, perl = TRUE)
  transformed <- gsub(":", "=", transformed, fixed = TRUE)
  text <- paste(transformed, collapse = "\n")

  # parse it
  json <- parse(text = text, keep.source = FALSE, srcfile = NULL)[[1L]]

  # construct map between source strings, replaced strings
  map <- as.character(parse(text = strings))
  names(map) <- as.character(parse(text = replacements))

  # convert to list
  map <- as.list(map)

  # remap strings in object
  remapped <- renv_json_remap(json, map)

  # evaluate
  eval(remapped, envir = baseenv())

}

renv_json_remap <- function(json, map) {

  # fix names
  if (!is.null(names(json))) {
    lhs <- match(names(json), names(map), nomatch = 0L)
    rhs <- match(names(map), names(json), nomatch = 0L)
    names(json)[rhs] <- map[lhs]
  }

  # fix values
  if (is.character(json))
    return(map[[json]] %||% json)

  # handle true, false, null
  if (is.name(json)) {
    text <- as.character(json)
    if (text == "true")
      return(TRUE)
    else if (text == "false")
      return(FALSE)
    else if (text == "null")
      return(NULL)
  }

  # recurse
  if (is.recursive(json)) {
    for (i in seq_along(json)) {
      json[i] <- list(renv_json_remap(json[[i]], map))
    }
  }

  json

}


# json-write.R ---------------------------------------------------------------


# @param box A vector of names, whose values should be boxed. By default,
#   scalar values are unboxed.
renv_json_config <- function(box = character()) {
  list(box = box)
}

renv_json_write <- function(object,
                            config = NULL,
                            file = stdout())
{
  config <- config %||% renv_json_config()
  json <- renv_json_convert_impl(NULL, object, config, 0L)
  if (is.null(file))
    return(json)

  writeLines(json, con = file)

}

renv_json_convert <- function(object, config = renv_json_config()) {
  renv_json_convert_impl(NULL, object, config, 0L)
}

renv_json_convert_impl <- function(key, value, config, depth) {

  if (is.list(value) || !is.null(names(value)))
    return(renv_json_convert_list(key, value, config, depth))

  json <- renv_json_convert_atom(key, value, config, depth)
  indent <- renv_json_convert_indent(depth)
  paste0(indent, json)

}

renv_json_convert_list <- function(key, value, config, depth) {
  indent <- renv_json_convert_indent(depth)
  if (empty(value)) {
    json <- if (is.null(names(value))) "[]" else "{}"
    paste0(indent, json)
  } else if (is.null(names(value))) {
    json <- enum_chr(value, renv_json_convert_impl, config = config, depth = depth + 1L)
    paste0(indent, "[", "\n", paste(json, collapse = ",\n"), "\n", indent, "]")
  } else {
    keys <- renv_json_quote(names(value))
    vals <- enum_chr(value, renv_json_convert_impl, config = config, depth = depth + 1L)
    idx  <- regexpr("[^[:space:]]", vals)
    json <- paste0(substring(vals, 1L, idx - 1L), keys, ": ", substring(vals, idx))
    paste0(indent, "{", "\n", paste(json, collapse = ",\n"), "\n", indent, "}")
  }
}

renv_json_convert_atom <- function(key, value, config, depth) {

  unbox <- is.null(key) || !key %in% config$box || inherits(value, "AsIs")
  if (is.null(value))
    return(if (unbox) "null" else "[]")

  n <- length(value)
  if (n == 0L)
    return("[]")

  if (is.character(value)) {
    value <- renv_json_quote(value)
    value[value %in% c("NA")] <- "null"
  }

  if (is.logical(value)) {
    value <- ifelse(value, "true", "false")
    value[is.na(value)] <- "null"
  }

  if (unbox && n == 1L)
    return(if (is.na(value)) "null" else paste0(value))

  indent <- renv_json_convert_indent(depth)
  json <- paste0(renv_json_convert_indent(depth + 1L), value)
  paste0("[", "\n", paste(json, collapse = ",\n"), "\n", indent, "]")

}

renv_json_convert_indent <- function(level) {
  paste(rep("  ", level), collapse = "")
}


# json.R ---------------------------------------------------------------------


renv_json_quote <- function(text) {
  encodeString(text, quote = "\"", justify = "none")
}


# knitr.R --------------------------------------------------------------------


renv_knitr_options_header <- function(text, type) {

  # extract the inner options from the header
  patterns <- renv_knitr_patterns()
  rest <- sub(patterns[[type]]$chunk.begin, "\\1", text)

  # if this is an R Markdown document, parse the initial engine chunk
  # (default to 'r' when not set)
  engine <- "r"
  if (type == "md") {
    idx <- regexpr("(?:[ ,]|$)", rest)
    engine <- substring(rest, 1, idx - 1)
    rest <- sub("^,*\\s*", "", substring(rest, idx + 1))
  }

  # parse the params
  params <- renv_knitr_options_header_impl(rest)

  # ensure an engine is set, if any
  params[["engine"]] <- params[["engine"]] %||% engine

  # return parsed params
  params

}

renv_knitr_options_header_impl <- function(rest) {

  # extract an unquoted label
  label <- ""
  pattern <- "(^\\s*[^=]+)(,|\\s*$)"
  matches <- regexec(pattern, rest)[[1]]
  if (!identical(c(matches), -1L)) {
    submatches <- regmatches(rest, list(matches))[[1]]
    label <- trimws(submatches[[2L]])
    rest <- substring(rest, matches[[3L]] + 1L)
  }

  # parse as alist
  params <- catch(parse(text = sprintf("alist(%s)", rest))[[1]])
  if (inherits(params, "error"))
    return(list())

  # inject the label back in
  names(params) <- names(params) %||% rep.int("", length(params))
  if (length(params) > 1 && names(params)[[2L]] == "")
    names(params)[[2L]] <- "label"

  # fix up 'label' if it's a missing value
  if (identical(params[["label"]], quote(expr = )))
    params[["label"]] <- NULL

  # if we parsed a label, add it in
  if (is.null(params[["label"]]) && nzchar(label))
    params[["label"]] <- label

  # evaluate the alist
  eval(params, envir = parent.frame())

}

renv_knitr_options_chunk <- function(code) {

  # find chunk option lines
  pattern <- "^[[:space:]]*#+[|]"
  matches <- grep(pattern, code[nzchar(code)], value = TRUE)

  # remove prefix
  text <- gsub(pattern, "", matches)

  # try to guess whether it's YAML
  isyaml <- any(grepl("^[[:space:]]*[^[:space:]:]+:", text))

  # first, try to parse as YAML, then as R code
  params <- if (isyaml) {

    # validate that we actually have the yaml package available
    if (!renv_dependencies_require("yaml"))
      return(list())

    catch(renv_yaml_load(text))

  } else {
    code <- paste(text, collapse = ", ")
    catch(renv_knitr_options_header_impl(code))
  }

  # check for error and report if this is in dependency discovery
  if (inherits(params, "error")) {

    state <- renv_dependencies_state()
    if (!is.null(state)) {
      problem <- list(file = state$path %||% "<unknown>", error = params)
      state$problems$push(problem)
    }

    return(list())

  }

  # return parsed params
  params

}

renv_knitr_patterns <- function() {

  list(

    rnw = list(
      chunk.begin = "^\\s*<<(.*)>>=.*$",
      chunk.end = "^\\s*@\\s*(%+.*|)$",
      inline.code = "\\\\Sexpr\\{([^}]+)\\}",
      inline.comment = "^\\s*%.*",
      ref.chunk = "^\\s*<<(.+)>>\\s*$",
      header.begin = "(^|\n)\\s*\\\\documentclass[^}]+\\}",
      document.begin = "\\s*\\\\begin\\{document\\}"
    ),

    tex = list(
      chunk.begin = "^\\s*%+\\s*begin.rcode\\s*(.*)",
      chunk.end = "^\\s*%+\\s*end.rcode",
      chunk.code = "^\\s*%+",
      ref.chunk = "^%+\\s*<<(.+)>>\\s*$",
      inline.comment = "^\\s*%.*",
      inline.code = "\\\\rinline\\{([^}]+)\\}",
      header.begin = "(^|\n)\\s*\\\\documentclass[^}]+\\}",
      document.begin = "\\s*\\\\begin\\{document\\}"
    ),

    html = list(
      chunk.begin = "^\\s*<!--\\s*begin.rcode\\s*(.*)",
      chunk.end = "^\\s*end.rcode\\s*-->",
      ref.chunk = "^\\s*<<(.+)>>\\s*$",
      inline.code = "<!--\\s*rinline(.+?)-->",
      header.begin = "\\s*<head>"
    ),

    md = list(
      chunk.begin = "^[\t >]*```+\\s*\\{([a-zA-Z0-9_]+( *[ ,].*)?)\\}\\s*$",
      chunk.end = "^[\t >]*```+\\s*$",
      ref.chunk = "^\\s*<<(.+)>>\\s*$",
      inline.code = "(?<!(^|\n)``)`r[ #]([^`]+)\\s*`"
    ),

    rst = list(
      chunk.begin = "^\\s*[.][.]\\s+\\{r(.*)\\}\\s*$",
      chunk.end = "^\\s*[.][.]\\s+[.][.]\\s*$",
      chunk.code = "^\\s*[.][.]",
      ref.chunk = "^\\.*\\s*<<(.+)>>\\s*$",
      inline.code = ":r:`([^`]+)`"
    ),

    asciidoc = list(
      chunk.begin = "^//\\s*begin[.]rcode(.*)$",
      chunk.end = "^//\\s*end[.]rcode\\s*$",
      chunk.code = "^//+",
      ref.chunk = "^\\s*<<(.+)>>\\s*$",
      inline.code = "`r +([^`]+)\\s*`|[+]r +([^+]+)\\s*[+]",
      inline.comment = "^//.*"
    ),

    textile = list(
      chunk.begin = "^###[.]\\s+begin[.]rcode(.*)$",
      chunk.end = "^###[.]\\s+end[.]rcode\\s*$",
      ref.chunk = "^\\s*<<(.+)>>\\s*$",
      inline.code = "@r +([^@]+)\\s*@",
      inline.comment = "^###[.].*"
    )

  )

}


# l10n.R ---------------------------------------------------------------------


renv_l10n_mbcs <- function() {
  info <- l10n_info()
  info$MBCS
}

renv_l10n_utf8 <- function() {
  info <- l10n_info()
  info$`UTF-8`
}

renv_l10n_latin1 <- function() {
  info <- l10n_info()
  info$`Latin-1`
}


# libpaths.R -----------------------------------------------------------------


the$libpaths <- new.env(parent = emptyenv())

# NOTE: if sandboxing is used then these symbols will be clobbered;
# save them so we can properly restore them later if so required
renv_libpaths_init <- function() {
  assign(".libPaths()",   .libPaths(),   envir = the$libpaths)
  assign(".Library",      .Library,      envir = the$libpaths)
  assign(".Library.site", .Library.site, envir = the$libpaths)
}

renv_libpaths_active <- function() {
  .libPaths()[[1L]]
}

renv_libpaths_all <- function() {
  .libPaths()
}

renv_libpaths_system <- function() {
  get(".Library", envir = the$libpaths)
}

renv_libpaths_site <- function() {
  get(".Library.site", envir = the$libpaths)
}

renv_libpaths_external <- function(project) {
  projlib <- settings$external.libraries(project = project)
  conflib <- config$external.libraries(project)
  .expand_R_libs_env_var(c(projlib, conflib))
}

# on Windows, attempting to use a library path containing
# characters considered special by cmd.exe will fail.
# to guard against this, we try to create a junction point
# from the temporary directory to the target library path
#
# https://github.com/rstudio/renv/issues/334
renv_libpaths_safe <- function(libpaths) {

  if (renv_libpaths_safe_check(libpaths))
    return(libpaths)

  map_chr(libpaths, renv_libpaths_safe_impl)

}

renv_libpaths_safe_check <- function(libpaths) {

  # if any of the paths have single quotes,
  # then we need to use a safe path
  # https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17973
  if (any(grepl("'", libpaths, fixed = TRUE)))
    return(FALSE)

  # on Windows, we need to use safe library paths for R < 4.0.0
  # https://bugs.r-project.org/bugzilla/show_bug.cgi?id=17709
  if (renv_platform_windows() && getRversion() < "4.0.0")
    return(FALSE)

  # otherwise, we're okay
  return(TRUE)

}

renv_libpaths_safe_impl <- function(libpath) {

  # check for an unsafe library path
  unsafe <-
    Encoding(libpath) == "UTF-8" ||
    grepl("[&<>^|'\"]", libpath)

  # if the path appears safe, use it as-is
  if (!unsafe)
    return(libpath)

  # try to form a safe library path
  methods <- c(
    renv_libpaths_safe_tempdir,
    renv_libpaths_safe_userlib
  )

  for (method in methods) {
    safelib <- catchall(method(libpath))
    if (is.character(safelib))
      return(safelib)
  }

  # could not form a safe library path;
  # just use the existing library path as-is
  libpath

}

renv_libpaths_safe_tempdir <- function(libpath) {
  safelib <- tempfile("renv-safelib-")

  if (renv_platform_windows())
    renv_file_junction(libpath, safelib)
  else
    file.symlink(libpath, safelib)

  safelib
}

renv_libpaths_safe_userlib <- function(libpath) {

  # form path into user library
  userlib <- renv_libpaths_user()[[1]]
  base <- file.path(userlib, ".renv-links")
  ensure_directory(base)

  # create name for actual junction point
  name <- renv_hash_text(libpath)
  safelib <- file.path(base, name)

  # if the junction already exists, use it
  if (renv_file_same(libpath, safelib))
    return(safelib)

  # otherwise, try to create it. note that junction
  # points can be removed with a non-recursive unlink
  unlink(safelib)

  if (renv_platform_windows())
    renv_file_junction(libpath, safelib)
  else
    file.symlink(libpath, safelib)

  safelib

}

renv_libpaths_set <- function(libpaths) {
  oldlibpaths <- .libPaths()
  safepaths <- renv_libpaths_safe(libpaths)
  .libPaths(safepaths)
  oldlibpaths
}

renv_libpaths_default <- function() {
  the$libpaths$`.libPaths()`
}

# NOTE: may return more than one library path!
renv_libpaths_user <- function() {

  # if renv is active, the user library will be saved
  envvars <- c("RENV_DEFAULT_R_LIBS_USER", "R_LIBS_USER")
  for (envvar in envvars) {

    value <- Sys.getenv(envvar, unset = NA)
    if (is.na(value) || value %in% c("", "<NA>", "NULL"))
      next

    parts <- strsplit(value, .Platform$path.sep, fixed = TRUE)[[1L]]
    return(parts)

  }

  # otherwise, default to active library
  # (shouldn't happen but best be safe)
  renv_libpaths_active()

}

renv_init_libpaths <- function(project) {

  projlib <- renv_paths_library(project = project)
  extlib <- renv_libpaths_external(project = project)
  userlib <- if (config$user.library())
    renv_libpaths_user()

  libpaths <- c(projlib, extlib, userlib)
  lapply(libpaths, ensure_directory)

  libpaths

}

renv_libpaths_restore <- function() {
  libpaths <- get(".libPaths()", envir = the$libpaths)
  renv_libpaths_set(libpaths)
}

# We need to ensure the system library is included, for cases where users have
# provided an explicit 'library' argument in calls to functions like
# 'renv::restore(library = <...>)')
#
# https://github.com/rstudio/renv/issues/1544
renv_libpaths_resolve <- function(library = NULL) {

  if (is.null(library))
    return(renv_libpaths_all())

  unique(c(library, .Library))

}


# library.R ------------------------------------------------------------------


# check for problems in the project's private library (e.g. broken symlinks
# to the cache or similar)
renv_library_diagnose <- function(project, libpath) {

  children <- list.files(libpath, full.names = TRUE)
  if (empty(children))
    return(TRUE)

  # if all symlinks are broken, assume the cache is missing or has been moved
  missing <- !file.exists(children)
  if (all(missing)) {
    msg <- lines(
      "The project library's symlinks to the cache are all broken.",
      "Has the cache been removed, or is it otherwise inaccessible?",
      paste("Cache root:", shQuote(renv_paths_cache()[[1L]]))
    )
    warning(msg, call. = FALSE)
    return(FALSE)
  }

  # if only some symlinks are broken, report to user
  if (any(missing)) {

    caution_bullets(
      "The following package(s) are missing entries in the cache:",
      basename(children[missing]),
      "These packages will need to be reinstalled."
    )

    return(FALSE)

  }

  TRUE

}


# license.R ------------------------------------------------------------------


# used to generate the CRAN-compatible license file in R CMD build
renv_license_generate <- function() {

  # only done if we're building
  if (!building())
    return(FALSE)

  contents <- c(
    paste("YEAR:", format(Sys.Date(), "%Y")),
    "COPYRIGHT HOLDER: Posit Software, PBC"
  )

  writeLines(contents, con = "LICENSE")
  return(TRUE)

}

if (identical(.packageName, "renv"))
  renv_license_generate()



# load.R ---------------------------------------------------------------------


#' Load a project
#'
#' @description
#' `renv::load()` sets the library paths to use a project-local library,
#' sets up the system library [sandbox], if needed, and creates shims
#' for `install.packages()`, `update.packages()`, and `remove.packages()`.
#'
#' You should not generally need to call `renv::load()` yourself, as it's
#' called automatically by the project auto-loader created by [renv::init()]/
#' [renv::activate()]. However, if needed, you can use `renv::load("<project>")`
#' to explicitly load an renv project located at a particular path.
#'
#' # Shims
#'
#' To help you take advantage of the package cache, renv places a couple of
#' shims on the search path:
#'
#' * `install.packages()` instead calls `renv::install()`.
#' * `remove.packages()` instead calls `renv::remove()`.
#' * `update.packages()` instead calls `renv::update()`.
#'
#' This allows you to keep using your existing muscle memory for installing,
#' updating, and remove packages, while taking advantage of renv features
#' like the package cache.
#'
#' If you'd like to bypass these shims within an \R session, you can explicitly
#' call the version of these functions from the utils package, e.g. with
#' `utils::install.packages(<...>)`.
#'
#' If you'd prefer not to use the renv shims at all, they can be disabled by
#' setting the R option `options(renv.config.shims.enabled = FALSE)` or by
#' setting the environment variable `RENV_CONFIG_SHIMS_ENABLED = FALSE`. See
#' `?config` for more details.
#'
#' @inherit renv-params
#'
#' @param quiet Boolean; be quiet during load?
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # load a project -- note that this is normally done automatically
#' # by the project's auto-loader, but calling this explicitly to
#' # load a particular project may be useful in some circumstances
#' renv::load()
#'
#' }
load <- function(project = NULL, quiet = FALSE) {

  renv_scope_error_handler()

  project <- renv_path_normalize(
    project %||% renv_project_find(project),
    mustWork = TRUE
  )

  action <- renv_load_action(project)
  if (action[[1L]] == "cancel") {
    cancel()
  } else if (action[[1L]] == "init") {
    return(init(project))
  } else if (action[[1L]] == "alt") {
    project <- action[[2L]]
  }

  renv_project_lock(project = project)

  # indicate that we're now loading the project
  renv_scope_options(renv.load.running = TRUE)

  # avoid suppressing the next auto snapshot
  the$auto_snapshot_running <- TRUE
  defer(the$auto_snapshot_running <- FALSE)

  # if load is being called via the autoloader,
  # then ensure RENV_PROJECT is unset
  # https://github.com/rstudio/renv/issues/887
  if (identical(getOption("renv.autoloader.running"), TRUE))
    renv_project_clear()

  # if we're loading a project different from the one currently loaded,
  # then unload the current project and reload the requested one
  switch <-
    !renv_metadata_embedded() &&
    !is.null(the$project_path) &&
    !identical(project, the$project_path)

  if (switch)
    return(renv_load_switch(project))

  if (quiet || renv_load_quiet())
    renv_scope_options(renv.verbose = FALSE)

  renv_envvars_save()

  # load a minimal amount of state when testing
  if (renv_tests_running())
    return(renv_load_minimal(project))

  # load rest of renv components
  renv_load_init(project)
  renv_load_path(project)
  renv_load_shims(project)
  renv_load_renviron(project)
  renv_load_profile(project)
  renv_load_settings(project)
  renv_load_project(project)
  renv_load_sandbox(project)
  renv_load_libpaths(project)
  renv_load_rprofile(project)
  renv_load_cache(project)

  # load components encoded in lockfile
  lockfile <- renv_lockfile_load(project)
  if (length(lockfile)) {
    renv_load_r(project, lockfile$R)
    renv_load_python(project, lockfile$Python)
    renv_load_bioconductor(project, lockfile$Bioconductor)
  }

  # allow failure to write infrastructure here to be non-fatal
  # https://github.com/rstudio/renv/issues/574#issuecomment-731159197
  catch({
    renv_infrastructure_write_rbuildignore(project)
    renv_infrastructure_write_gitignore(project)
  })

  renv_load_finish(project, lockfile)

  invisible(project)
}

renv_load_action <- function(project) {

  # don't do anything in non-interactive sessions
  if (!interactive())
    return("load")

  # if this project doesn't yet contain an 'renv' folder, assume
  # that it has not yet been initialized, and prompt the user
  renv <- renv_paths_renv(project = project, profile = FALSE)
  if (dir.exists(renv))
    return("load")

  # check and see if we're being called within a sub-directory
  path <- renv_file_find(dirname(project), function(parent) {
    if (file.exists(file.path(parent, "renv")))
      return(parent)
  })

  fmt <- "The project located at %s has not yet been initialized."
  header <- sprintf(fmt, renv_path_pretty(project))
  title <- paste("", header, "", "What would you like to do?", sep = "\n")

  choices <- c(
    init    = "Initialize this project with `renv::init()`.",
    load    = "Continue loading this project as-is.",
    cancel  = "Cancel loading this project."
  )

  if (!is.null(path)) {
    fmt <- "Load the project located at %s instead."
    msg <- sprintf(fmt, renv_path_pretty(path))
    choices <- c(choices, alt = msg)
  }

  selection <- tryCatch(
    utils::select.list(choices, title = title, graphics = FALSE),
    interrupt = identity
  )

  if (inherits(selection, "interrupt")) {
    writef()
    selection <- choices["cancel"]
  }

  list(names(selection), path)

}

renv_load_minimal <- function(project) {

  renv_load_libpaths(project)

  lockfile <- renv_lockfile_load(project)
  if (length(lockfile)) {
    renv_load_r(project, lockfile$R)
    renv_load_python(project, lockfile$Python)
  }

  renv_load_finish(project, lockfile)
  invisible(project)

}

renv_load_r <- function(project, fields) {

  # check for missing fields
  if (is.null(fields)) {
    warning("missing required [R] section in lockfile")
    return(NULL)
  }

  # load repositories
  renv_load_r_repos(fields$Repositories)

  # load (check) version
  version <- fields$Version
  if (is.null(version)) {
    warning("no R version recorded in this lockfile")
    return(NULL)
  }

  # normalize versions as strings
  requested <- renv_version_maj_min(version)
  current <- renv_version_maj_min(getRversion())

  # only compare major, minor versions
  if (!identical(requested, current)) {
    fmt <- "%s Using R %s (lockfile was generated with R %s)"
    writef(fmt, info_bullet(), getRversion(), version)
  }

}

renv_load_r_repos <- function(repos) {

  # force a character vector (https://github.com/rstudio/renv/issues/127)
  repos <- convert(repos, "character")

  # remove trailing slashes
  nms <- names(repos)
  repos <- sub("/+$", "", repos)
  names(repos) <- nms

  # transform PPM URLs if enabled
  # this ensures that install.packages() uses binaries by default on Linux,
  # where 'getOption("pkgType")' is "source" by default
  if (renv_ppm_enabled())
    repos <- renv_ppm_transform(repos)

  # normalize option
  repos <- renv_repos_normalize(repos)

  # set sanitized repos
  options(repos = repos)

  # and return
  repos

}

renv_load_init <- function(project) {

  # warn if the project path cannot be translated into the native encoding,
  # as (especially on Windows) this will likely prevent renv from working
  actual <- enc2utf8(project)
  expected <- catch(enc2utf8(enc2native(actual)))
  if (identical(actual, expected))
    return(TRUE)

  msg <- paste(
    "the project path cannot be represented in the native encoding;",
    "renv may not function as expected"
  )

  warning(msg)

}

renv_load_path <- function(project) {

  # only required when running in RStudio
  if (!renv_rstudio_available())
    return(FALSE)

  # on macOS, read paths from /etc/paths and friends

  # nocov start
  if (renv_platform_macos()) {

    # read the current PATH
    old <- Sys.getenv("PATH", unset = "") %>%
      strsplit(split = .Platform$path.sep, fixed = TRUE) %>%
      unlist()

    # get the new PATH entries
    files <- c(
      if (file.exists("/etc/paths")) "/etc/paths",
      list.files("/etc/paths.d", full.names = TRUE)
    )

    new <- uapply(files, readLines, warn = FALSE)

    # mix them together, preferring things in /etc/paths
    mix <- unique(c(new, old))

    # update the PATH
    Sys.setenv(PATH = paste(mix, collapse = .Platform$path.sep))

  }
  # nocov end

}

renv_load_shims <- function(project) {
  if (renv_shims_enabled())
    renv_shims_activate()
}

renv_load_renviron <- function(project) {

  environs <- c(
    renv_paths_root(".Renviron"),
    if (config$user.environ())
      Sys.getenv("R_ENVIRON_USER", unset = "~/.Renviron"),
    file.path(project, ".Renviron")
  )

  for (environ in environs)
    if (file.exists(environ))
      readRenviron(environ)

  renv_envvars_normalize()

}

renv_load_profile <- function(project) {

  renv_bootstrap_profile_load(project = project)

}

renv_load_settings <- function(project) {

  # migrate settings.dcf => settings.json
  renv_settings_migrate(project = project)

  # load settings.R
  settings <- renv_paths_renv("settings.R", project = project)
  if (!file.exists(settings))
    return(FALSE)

  tryCatch(
    eval(parse(settings), envir = baseenv()),
    error = warnify
  )

  TRUE

}

renv_load_project <- function(project) {

  # update project list if enabled
  if (renv_cache_config_enabled(project = project)) {
    project <- renv_path_normalize(project)
    renv_load_project_projlist(project)
  }

  TRUE

}

renv_load_project_projlist <- function(project) {

  # read project list
  projects <- renv_paths_root("projects")
  projlist <- character()
  if (file.exists(projects))
    projlist <- readLines(projects, warn = FALSE, encoding = "UTF-8")

  # if the project is already recorded, nothing to do
  if (project %in% projlist)
    return(TRUE)

  # sort with C locale (ensure consistent sorting across OSes)
  projlist <- csort(c(projlist, project))

  # update the project list
  ensure_parent_directory(projects)
  catchall(writeLines(enc2utf8(projlist), con = projects, useBytes = TRUE))

  TRUE

}

renv_load_rprofile <- function(project = NULL) {

  project <- renv_project_resolve(project)

  # bail if not enabled by user
  enabled <- identical(config$user.profile(), TRUE)
  if (!enabled)
    return(FALSE)

  # callr will manage sourcing of user profile, so don't try
  # to source the user profile if we're in callr
  callr <- Sys.getenv("CALLR_CHILD_R_LIBS", unset = NA)
  if (!is.na(callr))
    return(FALSE)

  # check for existence of profile
  profile <- Sys.getenv("R_PROFILE_USER", unset = "~/.Rprofile")
  if (!file.exists(profile))
    return(FALSE)

  renv_scope_libpaths()
  renv_load_rprofile_impl(profile)

  TRUE

}

renv_load_rprofile_impl <- function(profile) {

  # NOTE: We'd like to use a regular tryCatch() handler here, but
  # that will cause issues for user profiles which attempt to add
  # global calling handlers. For that reason, we just register a
  # bare restart handler, so at least we can catch the jump.
  #
  # https://github.com/rstudio/renv/issues/1036
  status <- withRestarts(
    sys.source(profile, envir = globalenv()),
    abort = function() { structure(list(), class = "_renv_error") }
  )

  if (inherits(status, "_renv_error")) {
    fmt <- "an error occurred while sourcing %s"
    warningf(fmt, renv_path_pretty(profile))
  }

  FALSE

}

renv_load_libpaths <- function(project = NULL) {
  libpaths <- renv_init_libpaths(project)
  lapply(libpaths, renv_library_diagnose, project = project)
  Sys.setenv(R_LIBS_USER = paste(libpaths, collapse = .Platform$path.sep))
  renv_libpaths_set(libpaths)
}

renv_load_sandbox <- function(project) {
  renv_sandbox_activate(project)
}

renv_load_python <- function(project, fields) {

  python <- tryCatch(
    renv_load_python_impl(project, fields),
    error = function(e) {
      warning(e)
      NULL
    }
  )

  if (is.null(python))
    return(FALSE)

  # set environment variables
  # - RENV_PYTHON is the version of Python renv was configured to use
  # - RETICULATE_PYTHON used to configure version of Python used by reticulate
  Sys.setenv(
    RENV_PYTHON       = python,
    RETICULATE_PYTHON = python
  )

  # place python + relevant utilities on the PATH
  bindir <- normalizePath(dirname(python), mustWork = FALSE)
  renv_envvar_path_add("PATH", bindir)

  # on Windows, for conda environments, we may also have a Scripts directory
  # which will need to be pre-pended to the PATH
  if (renv_platform_windows()) {
    scriptsdir <- file.path(bindir, "Scripts")
    if (file.exists(scriptsdir))
      renv_envvar_path_add("PATH", scriptsdir)
  }

  # for conda environments, we should try to find conda and place the conda
  # executable on the PATH, in case users want to use conda e.g. from
  # the terminal or even via R system calls
  #
  # we'll also need to set some environment variables to ensure that conda
  # uses this environment by default
  info <- renv_python_info(python)
  if (identical(info$type, "conda")) {
    conda <- renv_conda_find(python)
    if (file.exists(conda)) {
      renv_envvar_path_add("PATH", dirname(conda))
      Sys.setenv(CONDA_PREFIX = info$root)
    }
  }

  TRUE

}

renv_load_python_impl <- function(project, fields) {

  # if RENV_PYTHON is already set, just use it
  python <- Sys.getenv("RENV_PYTHON", unset = NA)
  if (!is.na(python))
    return(python)

  # set a default reticulate Python environment path
  envpath <- renv_paths_renv("python/r-reticulate", project = project)
  Sys.setenv(RETICULATE_MINICONDA_PYTHON_ENVPATH = envpath)

  # nothing more to do if no lockfile fields set
  if (is.null(fields))
    return(NULL)

  # delegate based on type appropriately
  type <- fields$Type
  if (is.null(type))
    return(NULL)

  python <- switch(type,
    system     = renv_load_python_default(project, fields),
    virtualenv = renv_load_python_virtualenv(project, fields),
    conda      = renv_load_python_condaenv(project, fields),
    stopf("unrecognized Python type '%s'", type)
  )

  renv_path_canonicalize(python)

}

renv_load_python_default <- function(project, fields) {

  # if 'Name' points to a valid copy of Python, use it
  name <- fields$Name
  if (!is.null(name) && file.exists(name))
    return(name)

  # otherwise, try to find a compatible version of Python
  renv_python_find(fields$Version)

}

renv_load_python_virtualenv <- function(project, fields) {

  renv_use_python_virtualenv_impl(
    project = project,
    name    = fields[["Name"]]    %NA% NULL,
    version = fields[["Version"]] %NA% NULL,
    python  = fields[["Python"]]  %NA% NULL
  )

}

renv_load_python_condaenv <- function(project, fields) {

  renv_use_python_condaenv_impl(
    project = project,
    name    = fields[["Name"]]    %NA% NULL,
    version = fields[["Version"]] %NA% NULL,
    python  = fields[["Python"]]  %NA% NULL
  )

}

renv_load_bioconductor <- function(project, bioconductor) {

  # we don't try to support older R anymore
  if (getRversion() < "3.4")
    return()

  # if we don't have a valid Bioconductor version, bail
  version <- bioconductor$Version
  if (is.null(version))
    return()

  # initialize bioconductor
  renv_bioconductor_init()

  # validate version if necessary
  validate <- getOption("renv.bioconductor.validate")
  if (truthy(validate, default = TRUE))
    renv_load_bioconductor_validate(project, version)

  # update the R repositories
  repos <- renv_bioconductor_repos(project, version)
  options(repos = repos)

  # notify the user
  sprintf("- Using Bioconductor '%s'.", version)

}

renv_load_bioconductor_validate <- function(project, version) {

  if (!identical(renv_bioconductor_manager(), "BiocManager"))
    return()

  BiocManager <- renv_scope_biocmanager()
  if (!is.function(BiocManager$.version_validity))
    return()

  # check for valid version of Bioconductor
  # https://github.com/rstudio/renv/issues/1148
  status <- catch(BiocManager$.version_validity(version))
  if (!is.character(status))
    return()

  fmt <- lines(
    "This project is configured to use Bioconductor %1$s, which is not compatible with R %2$s.",
    "Use 'renv::init(bioconductor = \"%1$s\")' to re-initialize this project with the appropriate Bioconductor release.",
    if (renv_package_installed("BiocVersion"))
      "Please uninstall the 'BiocVersion' package first, with `remove.packages(\"BiocVersion\")`."
  )

  warningf(fmt, version, getRversion())

}

renv_load_switch <- function(project) {

  # skip when testing
  if (is_testing())
    return(project)

  # safety check: avoid recursive unload attempts
  unloading <- getOption("renv.unload.project")
  if (!is.null(unloading)) {
    fmt <- "ignoring recursive attempt to load project '%s'"
    warningf(fmt, renv_path_pretty(project))
    return(project)
  }

  # unset the RENV_PATHS_RENV environment variable
  # TODO: is there a path forward if different projects use
  # different RENV_PATHS_RENV paths?
  renvpath <- Sys.getenv("RENV_PATHS_RENV", unset = NA)
  Sys.unsetenv("RENV_PATHS_RENV")

  # validate that this project has an activate script
  script <- renv_paths_activate(project = project)
  if (!file.exists(script)) {
    fmt <- "project %s has no activate script and so cannot be activated"
    stopf(fmt, renv_path_pretty(project))
  }

  # signal that we're unloading now
  renv_scope_options(renv.unload.project = project)

  # perform the unload
  unload()

  # unload the current version of renv (but keep track of position
  # on search path in case we need to revert later)
  path <- renv_namespace_path("renv")
  pos <- match("package:renv", search())
  unloadNamespace("renv")

  # move to new project directory
  renv_scope_wd(project)

  # source the activate script
  source(script)

  # check and see if renv was successfully loaded
  if (!"renv" %in% loadedNamespaces()) {
    fmt <- "could not load renv from project %s; reloading previously-loaded renv"
    warningf(fmt, renv_path_pretty(project))
    loadNamespace("renv", lib.loc = dirname(path))
    Sys.setenv(RENV_PATHS_RENV = renvpath)
    if (!is.na(pos)) {
      args <- list(package = "renv", pos = pos, character.only = TRUE)
      do.call(base::library, args)
    }
  }

}

renv_load_cache <- function(project) {

  if (!interactive())
    return(FALSE)

  oldcache <- renv_paths_cache(version = renv_cache_version_previous())[[1L]]
  newcache <- renv_paths_cache(version = renv_cache_version())[[1L]]
  if (!file.exists(oldcache) || file.exists(newcache))
    return(FALSE)

  msg <- lines(
    "- The cache version has been updated in this version of renv.",
    "- Use `renv::rehash()` to migrate packages from the old renv cache."
  )
  printf(msg)

}

renv_load_check <- function(project) {
  renv_load_check_description(project)
}

renv_load_check_description <- function(project) {

  descpath <- file.path(project, "DESCRIPTION")
  if (!file.exists(descpath))
    return(TRUE)

  # read description file, with whitespace trimmed
  contents <- read(descpath) %>% trim() %>% chop()
  bad <- which(grepl("^\\s*$", contents, perl = TRUE))
  if (empty(bad))
    return(TRUE)

  values <- sprintf("[line %i is blank]", bad)

  caution_bullets(
    sprintf("%s contains blank lines:", renv_path_pretty(descpath)),
    values,
    c(
      "DESCRIPTION files cannot contain blank lines between fields.",
      "Please remove these blank lines from the file."
    )
  )

  return(FALSE)

}

renv_load_quiet <- function() {
  default <- identical(renv_verbose(), FALSE) || renv_session_quiet()
  config$startup.quiet(default = default)
}

renv_load_finish <- function(project = NULL, lockfile = NULL) {

  renv_project_set(project)

  renv_load_check(project)
  renv_load_report_project(project)
  renv_load_report_python(project)

  if (config$updates.check())
    renv_load_report_updates(project)

  if (config$synchronized.check())
    renv_load_report_synchronized(project, lockfile)

  renv_snapshot_auto_update(project = project)

}

renv_load_report_project <- function(project) {

  profile <- renv_profile_get()
  version <- renv_metadata_version_friendly(shafmt = "; sha: %s")

  if (!is.null(profile)) {
    fmt <- "- Project '%s' loaded. [renv %s; using profile '%s']"
    writef(fmt, renv_path_aliased(project), version, profile)
  } else {
    fmt <- "- Project '%s' loaded. [renv %s]"
    writef(fmt, renv_path_aliased(project), version)
  }

}

renv_load_report_python <- function(project) {
  # TODO
}

# nocov start
renv_load_report_updates <- function(project) {

  lockpath <- renv_lockfile_path(project = project)
  if (!file.exists(lockpath))
    return(FALSE)

  status <- update(project = project, check = TRUE)
  available <- inherits(status, "renv_updates") && length(status$diff)
  if (!available)
    return(FALSE)

  writef("- Use `renv::update()` to install updated packages.")
  if (!interactive())
    print(status)

  TRUE

}
# nocov end


renv_load_report_synchronized <- function(project = NULL, lockfile = NULL) {

  project  <- renv_project_resolve(project)
  lockfile <- lockfile %||% renv_lockfile_load(project)

  # signal that we're running synchronization checks
  renv_scope_binding(the, "project_synchronized_check_running", TRUE)

  # be quiet when checking for dependencies in this scope
  # https://github.com/rstudio/renv/issues/1181
  renv_scope_options(renv.config.dependency.errors = "ignored")

  # check for packages referenced in the lockfile which are not installed
  lockpkgs <- names(lockfile$Packages)
  libpkgs <- renv_snapshot_library(
    library = renv_libpaths_all(),
    project = project,
    records = FALSE
  )

  # ignore renv
  lockpkgs <- setdiff(lockpkgs, "renv")
  libpkgs <- setdiff(libpkgs, "renv")

  # check for case where no packages are installed (except renv)
  if (length(intersect(lockpkgs, libpkgs)) == 0 && length(lockpkgs) > 0L) {

    caution("- No packages recorded in the lockfile are installed.")
    choice <- menu(
      title = "What do you want to do?",
      choices = c(
        restore = "Restore the project library with `renv::restore()`",
        cancel = "Leave project library empty"
      )
    )

    if (choice == "restore") {
      restore(project, prompt = FALSE, exclude = "renv")
      return(TRUE)
    } else {
      return(FALSE)
    }
  }

  # check for case where one or more packages are missing
  missing <- setdiff(lockpkgs, basename(libpkgs))
  if (length(missing)) {
    msg <- lines(
      "- One or more packages recorded in the lockfile are not installed.",
      "- Use `renv::status()` for more details."
    )
    caution(msg)
    return(FALSE)
  }

  # otherwise, use status to detect if we're synchronized
  info <- local({
    renv_scope_options(renv.verbose = FALSE)
    renv_scope_caution(FALSE)
    status(project = project, sources = FALSE)
  })

  if (!identical(info$synchronized, TRUE)) {
    caution("- The project is out-of-sync -- use `renv::status()` for details.")
    return(FALSE)
  }

  TRUE

}


# lock.R ---------------------------------------------------------------------


the$lock_registry <- new.env(parent = emptyenv())

renv_lock_acquire <- function(path) {

  # normalize path
  path <- renv_lock_path(path)
  dlog("lock", "%s [acquiring lock]", renv_path_pretty(path))

  # if we already have this lock, increment our counter
  count <- the$lock_registry[[path]] %||% 0L
  if (count > 0L) {
    the$lock_registry[[path]] <- count + 1L
    return(TRUE)
  }

  # make sure parent directory exists
  ensure_parent_directory(path)

  # make sure warnings are errors here
  renv_scope_options(warn = 2L)

  # loop until we acquire the lock
  repeat tryCatch(
    renv_lock_acquire_impl(path) && break,
    error = function(cnd) Sys.sleep(0.2)
  )

  # mark this path as locked by us
  the$lock_registry[[path]] <- 1L

  # notify the watchdog
  renv_watchdog_notify("LockAcquired", list(path = path))

  # TRUE to mark successful lock
  dlog("lock", "%s [lock acquired]", renv_path_pretty(path))
  TRUE

}

# https://rcrowley.org/2010/01/06/things-unix-can-do-atomically.html
renv_lock_acquire_impl <- function(path) {

  # check for orphaned locks
  if (renv_lock_orphaned(path)) {
    dlog("lock", "%s: removing orphaned lock", path)
    unlink(path, recursive = TRUE, force = TRUE)
  }

  # attempt to create the lock
  dir.create(path, mode = "0755")

}

renv_lock_release <- function(path) {

  # normalize path
  path <- renv_lock_path(path)

  # decrement our lock count
  count <- the$lock_registry[[path]] <- the$lock_registry[[path]] - 1L

  # remove the lock if we have no more locks
  if (count == 0L) {
    dlog("lock", "%s [lock released]", renv_path_pretty(path))
    renv_lock_release_impl(path)
  }

}

renv_lock_release_impl <- function(path) {
  renv_scope_options(warn = -1L)
  unlink(path, recursive = TRUE, force = TRUE)
  rm(list = path, envir = the$lock_registry)
  renv_watchdog_notify("LockReleased", list(path = path))
}

renv_lock_orphaned <- function(path) {

  timeout <- getOption("renv.lock.timeout", default = 60L)
  if (timeout <= 0L)
    return(TRUE)

  info <- renv_file_info(path)
  if (is.na(info$isdir))
    return(FALSE)

  diff <- difftime(Sys.time(), info$mtime, units = "secs")
  diff >= timeout

}

renv_lock_refresh <- function(lock) {
  Sys.setFileTime(lock, Sys.time())
}

renv_lock_unload <- function() {
  locks <- ls(envir = the$lock_registry, all.names = TRUE)
  unlink(locks, recursive = TRUE, force = TRUE)
}

renv_lock_path <- function(path) {

  file.path(
    renv_path_normalize(dirname(path), mustWork = TRUE),
    basename(path)
  )

}


# lockfile-api.R -------------------------------------------------------------


# NOTE: These functions are used by the 'dockerfiler' package, even though
# they are not exported. We retain these functions here just to avoid issues
# during CRAN submission. We'll consider removing them in a future release.

renv_lockfile_api <- function(lockfile = NULL) {

  .lockfile <- lockfile
  .self <- new.env(parent = emptyenv())

  .self$repos <- function(..., .repos = NULL) {

    if (nargs() == 0) {
      repos <- .lockfile$R$Repositories
      return(repos)
    }

    repos <- .repos %||% list(...)
    if (is.null(names(repos)) || "" %in% names(repos))
      stop("repositories must all be named", call. = FALSE)

    .lockfile$R$Repositories <<- as.list(convert(repos, "character"))
    invisible(.self)

  }

  .self$version <- function(..., .version = NULL) {

    if (nargs() == 0) {
      version <- .lockfile$R$Version
      return(version)
    }

    version <- .version %||% c(...)

    if (length(version) > 1) {
      stop("Version should be length 1 character e.g. `\"3.6.3\"`")
    }

    .lockfile$R$Version <<- version
    invisible(.self)

  }

  .self$add <- function(..., .list = NULL) {

    records <- renv_lockfile_records(.lockfile)

    dots <- .list %||% list(...)
    enumerate(dots, function(package, remote) {
      resolved <- renv_remotes_resolve(remote)
      records[[package]] <<- resolved
    })

    renv_lockfile_records(.lockfile) <<- records
    invisible(.self)

  }

  .self$remove <- function(packages) {
    records <- renv_lockfile_records(.lockfile) %>% exclude(packages)
    renv_lockfile_records(.lockfile) <<- records
    invisible(.self)
  }

  .self$write <- function(file = stdout()) {
    renv_lockfile_write(.lockfile, file = file)
    invisible(.self)
  }

  .self$data <- function() {
    .lockfile
  }

  class(.self) <- "renv_lockfile_api"
  .self

}

#' Programmatically Create and Modify a Lockfile
#'
#' This function provides an API for creating and modifying `renv` lockfiles.
#' This can be useful when you'd like to programmatically generate or modify
#' a lockfile -- for example, because you want to update or change a package
#' record in an existing lockfile.
#'
#' @inheritParams renv-params
#'
#' @param file The path to an existing lockfile. When no lockfile is provided,
#'   a new one will be created based on the current project context. If you
#'   want to create a blank lockfile, use `file = NA` instead.
#'
#' @seealso \code{\link{lockfiles}}, for a description of the structure of an
#'   `renv` lockfile.
#'
#' @examples
#'
#' \dontrun{
#'
#' lock <- lockfile("renv.lock")
#'
#' # set the repositories for a lockfile
#' lock$repos(CRAN = "https://cran.r-project.org")
#'
#' # depend on digest 0.6.22
#' lock$add(digest = "digest@@0.6.22")
#'
#' # write to file
#' lock$write("renv.lock")
#'
#' }
#'
#' @keywords internal
#' @rdname lockfile-api
#' @name lockfile-api
#'
lockfile <- function(file = NULL, project = NULL) {
  project <- renv_project_resolve(project)
  renv_scope_error_handler()

  lock <- if (is.null(file)) {

    renv_lockfile_create(
      project  = project,
      libpaths = renv_libpaths_all(),
      type     = settings$snapshot.type(project = project)
    )

  } else if (is.na(file)) {

    renv_lockfile_init(project)

  } else {

    renv_lockfile_read(file = file)

  }

  renv_lockfile_api(lock)

}


# lockfile-diff.R ------------------------------------------------------------


renv_lockfile_diff <- function(old, new, compare = NULL) {

  compare <- compare %||% function(lhs, rhs) {
    list(before = lhs, after = rhs)
  }

  # ensure both lists have the same names, inserting missing
  # entries for those without any value
  nms <- union(names(old), names(new)) %||% character()
  if (length(nms)) {

    nms <- sort(nms)
    old[renv_vector_diff(nms, names(old))] <- list(NULL)
    new[renv_vector_diff(nms, names(new))] <- list(NULL)

    old <- old[nms]
    new <- new[nms]

  }

  # ensure that these have the same length for comparison
  if (is.list(old) && is.list(new))
    length(old) <- length(new) <- max(length(old), length(new))

  # check for differences
  diffs <- mapply(
    renv_lockfile_diff_impl, old, new,
    MoreArgs = list(compare = compare),
    SIMPLIFY = FALSE
  )

  # drop NULL entries
  reject(diffs, empty)

}

renv_lockfile_diff_impl <- function(lhs, rhs, compare) {
  case(
    is.list(lhs) && empty(rhs)   ~ renv_lockfile_diff(lhs, list(), compare),
    empty(lhs) && is.list(rhs)   ~ renv_lockfile_diff(list(), rhs, compare),
    is.list(lhs) && is.list(rhs) ~ renv_lockfile_diff(lhs, rhs, compare),
    !identical(c(lhs), c(rhs))   ~ compare(lhs, rhs),
    NULL
  )
}

renv_lockfile_diff_record <- function(before, after) {

  before <- renv_record_normalize(before)
  after  <- renv_record_normalize(after)

  # first, compare on version / record existence
  type <- case(
    is.null(before) ~ "install",
    is.null(after)  ~ "remove",
    before$Version < after$Version ~ "upgrade",
    before$Version > after$Version ~ "downgrade"
  )

  if (!is.null(type))
    return(type)

  # check for a crossgrade -- where the package version is the same,
  # but details about the package's remotes have changed
  if (!setequal(renv_record_names(before), renv_record_names(after)))
    return("crossgrade")

  nm <- union(renv_record_names(before), renv_record_names(after))
  if (!identical(before[nm], after[nm]))
    return("crossgrade")

  NULL

}

renv_lockfile_diff_packages <- function(old, new) {

  old <- renv_lockfile_records(old)
  new <- renv_lockfile_records(new)

  packages <- named(union(names(old), names(new)))
  actions <- lapply(packages, function(package) {
    before <- old[[package]]; after <- new[[package]]
    renv_lockfile_diff_record(before, after)
  })

  Filter(Negate(is.null), actions)

}

renv_lockfile_override <- function(lockfile) {
  records <- renv_lockfile_records(lockfile)
  overrides <- renv_records_override(records)
  renv_lockfile_records(lockfile) <- overrides
  lockfile
}

renv_lockfile_repair <- function(lockfile) {

  records <- renv_lockfile_records(lockfile)

  # fix up records in lockfile
  renv_lockfile_records(lockfile) <- enumerate(records, function(package, record) {

    # if this package is from a repository, but doesn't specify an explicit
    # version, then use the latest-available version of that package
    source <- renv_record_source_normalize(record, record$Source)
    if (identical(source, "Repository") && is.null(record$Version)) {
      entry <- renv_available_packages_latest(package)
      record$Version <- entry$Version
    }

    # return normalized record
    record

  })

  lockfile

}


# lockfile-read.R ------------------------------------------------------------


renv_lockfile_read_finish_impl <- function(key, val) {

  # convert repository records to named vectors
  # (be careful to handle NAs, NULLs)
  if (identical(key, "Repositories") && is.null(names(val))) {

    getter <- function(name) function(record) record[[name]] %||% "" %NA% ""
    keys <- map_chr(val, getter("Name"))
    vals <- map_chr(val, getter("URL"))

    result <- case(
      empty(keys)       ~ list(),
      any(nzchar(keys)) ~ named(vals, keys),
      TRUE              ~ vals
    )

    return(as.list(result))

  }

  # convert the "Requirements" field to a character vector
  if (identical(key, "Requirements"))
    return(unlist(val))

  # recurse for lists
  if (is.list(val))
    return(enumerate(val, renv_lockfile_read_finish_impl))

  # return other values as-is
  val

}

renv_lockfile_read_finish <- function(data) {
  data <- enumerate(data, renv_lockfile_read_finish_impl)
  class(data) <- "renv_lockfile"
  data
}

renv_lockfile_read_preflight <- function(contents) {

  # check for merge conflict markers
  starts <- grep("^[<]+", contents)
  ends   <- grep("^[>]+", contents)

  hasconflicts <-
    length(starts) &&
    length(ends) &&
    length(starts) == length(ends)

  if (hasconflicts) {

    parts <- .mapply(function(start, end) {
      c(contents[start:end], "")
    }, list(starts, ends), NULL)

    all <- unlist(parts, recursive = TRUE, use.names = FALSE)

    caution_bullets(
      "The lockfile contains one or more merge conflict markers:",
      head(all, n = -1L),
      "You will need to resolve these merge conflicts before the file can be read."
    )

    stop("lockfile contains merge conflict markers; cannot proceed", call. = FALSE)

  }

}

renv_lockfile_read <- function(file = NULL, text = NULL) {

  # read the lockfile
  contents <- if (is.null(file))
    unlist(strsplit(text, "\n", fixed = TRUE))
  else
    readLines(file, warn = FALSE, encoding = "UTF-8")

  # check and report some potential errors (e.g. merge conflicts)
  renv_lockfile_read_preflight(contents)
  withCallingHandlers(
    json <- renv_json_read(text = contents),
    error = function(err) {
      stop("Failed to parse 'renv.lock':\n", conditionMessage(err))
    }
  )

  renv_lockfile_read_finish(json)

}


# lockfile-write.R -----------------------------------------------------------


the$lockfile_state <- new.env(parent = emptyenv())

renv_lockfile_state_get <- function(key) {
  if (exists(key, envir = the$lockfile_state))
    get(key, envir = the$lockfile_state, inherits = FALSE)
}

renv_lockfile_state_set <- function(key, value) {
  assign(key, value, envir = the$lockfile_state, inherits = FALSE)
}

renv_lockfile_state_clear <- function() {
  rm(list = ls(the$lockfile_state), envir = the$lockfile_state)
}

renv_lockfile_write_preflight <- function(old, new) {

  diff <- renv_lockfile_diff(old, new)
  if (empty(diff))
    return(new)

  packages <- diff$Packages
  if (empty(diff$Packages))
    return(new)

  enumerate(packages, function(package, changes) {

    # avoid spurious changes between CRAN and RSPM
    spurious <-
      identical(changes, list(Repository = list(before = "CRAN", after = "RSPM"))) ||
      identical(changes, list(Repository = list(before = "RSPM", after = "CRAN")))

    if (spurious)
      new$Packages[[package]]$Repository <<- old$Packages[[package]]$Repository

    # avoid spurious changes between CRAN and PPM
    spurious <-
      identical(changes, list(Repository = list(before = "CRAN", after = "PPM"))) ||
      identical(changes, list(Repository = list(before = "PPM", after = "CRAN")))

    if (spurious)
      new$Packages[[package]]$Repository <<- old$Packages[[package]]$Repository

  })

  new

}

renv_lockfile_write <- function(lockfile, file = stdout()) {

  # if we're updating an existing lockfile, try to avoid
  # "unnecessary" diffs that might otherwise be annoying
  if (is.character(file) && file.exists(file)) {
    old <- catch(renv_lockfile_read(file))
    if (!inherits(old, "error"))
      lockfile <- renv_lockfile_write_preflight(old, lockfile)
  }

  lockfile <- renv_lockfile_sort(lockfile)
  result <- renv_lockfile_write_json(lockfile, file)

  if (is.character(file))
    writef("- Lockfile written to %s.", renv_path_pretty(file))

  result

}

renv_lockfile_write_json_prepare_repos <- function(repos) {

  prepared <- enumerate(repos, function(name, url) {
    url <- sub("/+$", "", url)
    list(Name = name, URL = url)
  })

  unname(prepared)
}

renv_lockfile_write_json_prepare <- function(key, val) {

  if (key == "Repositories")
    renv_lockfile_write_json_prepare_repos(val)
  else if (is.list(val) && !is.null(names(val)))
    enumerate(val, renv_lockfile_write_json_prepare)
  else
    val

}

renv_lockfile_write_json <- function(lockfile, file = stdout()) {

  prepared <- enumerate(lockfile, renv_lockfile_write_json_prepare)

  box <- c("Depends", "Imports", "Suggests", "LinkingTo", "Requirements")
  config <- list(box = box)
  json <- renv_json_convert(prepared, config)
  if (is.null(file))
    return(json)

  writeLines(json, con = file)

}

renv_lockfile_write_internal <- function(lockfile,
                                         file = stdout(),
                                         delim = "=")
{
  if (is.character(file)) {
    file <- textfile(file)
    defer(close(file))
  }

  emitter <- function(text) writeLines(text, con = file)

  renv_lockfile_state_set("delim", delim)
  renv_lockfile_state_set("emitter", emitter)
  defer(renv_lockfile_state_clear())

  renv_lockfile_write_list(lockfile, section = character())
  invisible(lockfile)
}

renv_lockfile_write_list <- function(entry, section) {
  enumerate(entry, renv_lockfile_write_atoms, section = section)
  enumerate(entry, renv_lockfile_write_lists, section = section)
}

renv_lockfile_write_atoms <- function(key, value, section) {

  sublists <- map_lgl(value, function(x) identical(class(x), "list"))
  if (all(sublists))
    return()

  subsection <- c(section, key)
  label <- sprintf("[%s]", paste(subsection, collapse = "/"))
  renv_lockfile_write_emit(label)

  enumerate(value[!sublists], renv_lockfile_write_atom)
  renv_lockfile_write_emit()

}

renv_lockfile_write_atom <- function(key, value) {

  lhs <- key
  rhs <- if (is_named(value))
    paste(sprintf("\n\t%s=%s", names(value), value), collapse = "")
  else
    paste(value, collapse = ", ")

  delim <- renv_lockfile_state_get("delim")
  text <- paste(lhs, rhs, sep = delim)
  renv_lockfile_write_emit(text)

}

renv_lockfile_write_lists <- function(key, value, section) {
  sublists <- map_lgl(value, function(x) identical(class(x), "list"))
  renv_lockfile_write_list(value[sublists], section = c(section, key))
}

renv_lockfile_write_emit <- function(text = "") {
  emitter <- renv_lockfile_state_get("emitter")
  emitter(text)
}


# lockfile.R -----------------------------------------------------------------


renv_lockfile_init <- function(project) {

  lockfile <- list()

  lockfile$R        <- renv_lockfile_init_r(project)
  lockfile$Python   <- renv_lockfile_init_python(project)
  lockfile$Packages <- list()

  class(lockfile) <- "renv_lockfile"
  lockfile

}

renv_lockfile_init_r_version <- function(project) {

  # NOTE: older versions of renv may have written out an empty array
  # for the R version in some cases, so we explicitly check that we
  # receive a length-one string here.
  version <- settings$r.version(project = project)
  if (!pstring(version))
    version <- getRversion()

  format(version)

}

renv_lockfile_init_r_repos <- function(project) {

  repos <- getOption("repos")

  # save names
  nms <- names(repos)

  # force as character
  repos <- as.character(repos)

  # clear RStudio attribute
  attr(repos, "RStudio") <- NULL

  # set a default URL
  repos[repos == "@CRAN@"] <- getOption(
    "renv.repos.cran",
    "https://cloud.r-project.org"
  )

  # remove PPM bits from URL
  if (renv_ppm_enabled()) {
    pattern <- "/__[^_]+__/[^/]+/"
    repos <- sub(pattern, "/", repos)
  }

  # force as list
  repos <- as.list(repos)

  # ensure names
  names(repos) <- nms

  repos

}

renv_lockfile_init_r <- function(project) {
  version <- renv_lockfile_init_r_version(project)
  repos   <- renv_lockfile_init_r_repos(project)
  list(Version = version, Repositories = repos)
}

renv_lockfile_init_python <- function(project) {

  python <- Sys.getenv("RENV_PYTHON", unset = NA)
  if (is.na(python))
    return(NULL)

  if (!file.exists(python))
    return(NULL)

  info <- renv_python_info(python)
  if (is.null(info))
    return(NULL)

  version <- renv_python_version(python)
  type <- info$type
  root <- info$root
  name <- renv_python_envname(project, root, type)

  fields <- list()

  fields$Version <- version
  fields$Type    <- type
  fields$Name    <- name

  fields

}

renv_lockfile_fini <- function(lockfile, project) {
  lockfile$Bioconductor <- renv_lockfile_fini_bioconductor(lockfile, project)
  lockfile
}

renv_lockfile_fini_bioconductor <- function(lockfile, project) {

  # check for explicit version in settings
  version <- settings$bioconductor.version(project = project)
  if (length(version))
    return(list(Version = version))

  # otherwise, check for a package which required Bioconductor
  records <- renv_lockfile_records(lockfile)
  if (empty(records))
    return(NULL)

  for (package in c("BiocManager", "BiocInstaller"))
    if (!is.null(records[[package]]))
      return(list(Version = renv_bioconductor_version(project = project)))

  sources <- extract_chr(records, "Source")
  if ("Bioconductor" %in% sources)
    return(list(Version = renv_bioconductor_version(project = project)))

  # nothing found; return NULL
  NULL

}

renv_lockfile_path <- function(project) {
  renv_paths_lockfile(project = project)
}

renv_lockfile_save <- function(lockfile, project) {
  file <- renv_lockfile_path(project)
  renv_lockfile_write(lockfile, file = file)
}

renv_lockfile_load <- function(project, strict = FALSE) {

  path <- renv_lockfile_path(project)
  if (file.exists(path))
    return(renv_lockfile_read(path))

  if (strict) {
    abort(c(
      "This project does not contain a lockfile.",
      i = "Have you called `snapshot()` yet?"
    ))
  }

  renv_lockfile_init(project = project)

}

renv_lockfile_sort <- function(lockfile) {

  # extract R records (nothing to do if empty)
  records <- renv_lockfile_records(lockfile)
  if (empty(records))
    return(lockfile)

  # sort the records
  sorted <- records[csort(names(records))]
  renv_lockfile_records(lockfile) <- sorted

  # sort top-level fields
  fields <- unique(c("R", "Bioconductor", "Python", "Packages", names(lockfile)))
  lockfile <- lockfile[intersect(fields, names(lockfile))]

  # return post-sort
  lockfile

}

renv_lockfile_create <- function(project,
                                 type = NULL,
                                 libpaths = NULL,
                                 packages = NULL,
                                 exclude = NULL,
                                 prompt = NULL,
                                 force = NULL)
{
  libpaths <- libpaths %||% renv_libpaths_all()
  type <- type %||% settings$snapshot.type(project = project)

  # use a restart, so we can allow the user to install packages before snapshot
  lockfile <- withRestarts(
    renv_lockfile_create_impl(project, type, libpaths, packages, exclude, prompt, force),
    renv_recompute_records = function() {
      renv_dynamic_reset()
      renv_lockfile_create_impl(project, type, libpaths, packages, exclude, prompt, force)
    }
  )
}

renv_lockfile_create_impl <- function(project, type, libpaths, packages, exclude, prompt, force) {

  lockfile <- renv_lockfile_init(project)

  # compute the project's top-level package dependencies
  packages <- packages %||% renv_snapshot_dependencies(
    project = project,
    type = type,
    dev = FALSE
  )

  # expand the recursive dependencies of these packages
  records <- renv_snapshot_packages(
    packages = setdiff(packages, exclude),
    libpaths = libpaths,
    project  = project
  )

  # check for missing packages
  ignored <- c(renv_project_ignored_packages(project), renv_packages_base(), exclude, "renv")
  missing <- setdiff(packages, c(names(records), ignored))

  # cancel automatic snapshots if we have missing packages
  if (length(missing) && the$auto_snapshot_running)
    invokeRestart("cancel")

  # give user a chance to handle missing packages, if any
  #
  # we only run this in top-level calls to snapshot() since renv will internally
  # use snapshot() to create lockfiles, and missing packages are understood /
  # tolerated there. this code mostly exists so interactive usages of snapshot()
  # can recover and install missing packages
  if (identical(topfun(), snapshot))
    renv_snapshot_report_missing(missing, type)

  records <- renv_snapshot_fixup(records)
  renv_lockfile_records(lockfile) <- records

  lockfile <- renv_lockfile_fini(lockfile, project)

  keys <- unique(c("R", "Bioconductor", names(lockfile)))
  lockfile <- lockfile[intersect(keys, names(lockfile))]

  class(lockfile) <- "renv_lockfile"
  lockfile

}

renv_lockfile_modify <- function(lockfile, records) {

  enumerate(records, function(package, record) {
    renv_lockfile_records(lockfile)[[package]] <<- record
  })

  lockfile

}

renv_lockfile_compact <- function(lockfile) {

  records <- renv_lockfile_records(lockfile)
  remotes <- map_chr(records, renv_record_format_remote)

  remotes <- csort(remotes)

  formatted <- sprintf("  \"%s\"", remotes)
  joined <- paste(formatted, collapse = ",\n")

  all <- c("renv::use(", joined, ")")
  paste(all, collapse = "\n")

}

renv_lockfile_records <- function(lockfile) {
  as.list(lockfile$Packages %||% lockfile)
}

`renv_lockfile_records<-` <- function(x, value) {
  x$Packages <- filter(value, zlength)
  invisible(x)
}

# for compatibility with older versions of RStudio
renv_records <- renv_lockfile_records


# lockfiles.R ----------------------------------------------------------------


#' Lockfiles
#'
#' A **lockfile** records the state of a project at some point in time.
#'
#' A lockfile captures the state of a project's library at some point in time.
#' In particular, the package names, their versions, and their sources (when
#' known) are recorded in the lockfile.
#'
#' Projects can be restored from a lockfile using the [restore()] function. This
#' implies reinstalling packages into the project's private library, as encoded
#' within the lockfile.
#'
#' While lockfiles are normally generated and used with [snapshot()] /
#' [restore()], they can also be edited by hand if so desired. Lockfiles are
#' written as `.json`, to allow for easy consumption by other tools.
#'
#' An example lockfile follows:
#'
#' ```
#' {
#'   "R": {
#'     "Version": "3.6.1",
#'     "Repositories": [
#'       {
#'         "Name": "CRAN",
#'         "URL": "https://cloud.r-project.org"
#'       }
#'     ]
#'   },
#'   "Packages": {
#'     "markdown": {
#'       "Package": "markdown",
#'       "Version": "1.0",
#'       "Source": "Repository",
#'       "Repository": "CRAN",
#'       "Hash": "4584a57f565dd7987d59dda3a02cfb41"
#'     },
#'     "mime": {
#'       "Package": "mime",
#'       "Version": "0.7",
#'       "Source": "Repository",
#'       "Repository": "CRAN",
#'       "Hash": "908d95ccbfd1dd274073ef07a7c93934"
#'     }
#'   }
#' }
#' ```
#'
#' The sections used within a lockfile are described next.
#'
#' ## renv
#'
#' Information about the version of renv used to manage this project.
#'
#' \tabular{ll}{
#' \strong{Version}     \tab The version of the renv package used with this project. \cr
#' }
#'
#' ## R
#'
#' Properties related to the version of \R associated with this project.
#'
#' \tabular{ll}{
#' \strong{Version}      \tab The version of \R used. \cr
#' \strong{Repositories} \tab The \R repositories used in this project. \cr
#' }
#'
#' ## Packages
#'
#' \R package records, capturing the packages used or required by a project
#' at the time when the lockfile was generated.
#'
#' \tabular{ll}{
#' \strong{Package}      \tab The package name. \cr
#' \strong{Version}      \tab The package version. \cr
#' \strong{Source}       \tab The location from which this package was retrieved. \cr
#' \strong{Repository}   \tab The name of the repository (if any) from which this package was retrieved. \cr
#' \strong{Hash}         \tab (Optional) A unique hash for this package, used for package caching. \cr
#' }
#'
#' Additional remote fields, further describing how the package can be
#' retrieved from its corresponding source, will also be included as
#' appropriate (e.g. for packages installed from GitHub).
#'
#' ## Python
#'
#' Metadata related to the version of Python used with this project (if any).
#'
#' \tabular{ll}{
#' \strong{Version} \tab The version of Python being used. \cr
#' \strong{Type}    \tab The type of Python environment being used ("virtualenv", "conda", "system") \cr
#' \strong{Name}    \tab The (optional) name of the environment being used.
#' }
#'
#' Note that the `Name` field may be empty. In that case, a project-local Python
#' environment will be used instead (when not directly using a system copy of Python).
#'
#' # Caveats
#'
#' These functions are primarily intended for expert users -- in most cases,
#' [snapshot()] and [restore()] are the primariy tools you will need when
#' creating and using lockfiles.
#'
#' @inheritParams snapshot
#' @inheritParams renv-params
#'
#' @param lockfile An `renv` lockfile; typically created by either
#'   `lockfile_create()` or `lockfile_read()`.
#'
#' @param file A file path, or \R connection.
#'
#' @family reproducibility
#' @name lockfiles
#' @rdname lockfiles
NULL

#' @param libpaths The library paths to be used when generating the lockfile.
#' @rdname lockfiles
#' @export
lockfile_create <- function(type = settings$snapshot.type(project = project),
                            libpaths = .libPaths(),
                            packages = NULL,
                            exclude = NULL,
                            prompt = interactive(),
                            force = FALSE,
                            ...,
                            project = NULL)
{
  renv_dots_check(...)

  project <- renv_project_resolve(project)
  renv_scope_verbose_if(prompt)

  renv_lockfile_create(
    project  = project,
    type     = type,
    libpaths = libpaths,
    packages = packages,
    exclude  = exclude,
    prompt   = prompt,
    force    = force
  )
}

#' @rdname lockfiles
#' @export
lockfile_read <- function(file = NULL, ..., project = NULL) {
  project <- renv_project_resolve(project)
  file <- file %||% renv_paths_lockfile(project = project)
  renv_lockfile_read(file = file)
}

#' @rdname lockfiles
#' @export
lockfile_write <- function(lockfile, file = NULL, ..., project = NULL) {
  project <- renv_project_resolve(project)
  file <- file %||% renv_paths_lockfile(project = project)
  renv_lockfile_write(lockfile, file = file)
}

#' @param remotes An \R vector of remote specifications.
#'
#' @param repos A named vector, mapping \R repository names to their URLs.
#'
#' @rdname lockfiles
#' @export
lockfile_modify <- function(lockfile = NULL,
                            ...,
                            remotes = NULL,
                            repos = NULL,
                            project = NULL)
{
  renv_dots_check(...)

  project <- renv_project_resolve(project)
  lockfile <- lockfile %||% renv_lockfile_load(project, strict = TRUE)

  if (!is.null(repos))
    lockfile$R$Repositories <- as.list(repos)

  if (!is.null(remotes)) {
    remotes <- renv_records_resolve(remotes, latest = TRUE)
    names(remotes) <- map_chr(remotes, `[[`, "Package")
    enumerate(remotes, function(package, remote) {
      record <- renv_remotes_resolve(remote)
      renv_lockfile_records(lockfile)[[package]] <<- record
    })
  }

  lockfile

}


# log.R ----------------------------------------------------------------------


# the log level, indicating what severity of messages will be logged
the$log_level <- 4L

# the file to which log messages will be written
the$log_file <- NULL

# the scopes for which filtering will be enabled
the$log_scopes <- NULL

elog <- function(scope, fmt, ...) {
  renv_log_impl(4L, scope, fmt, ...)
}

wlog <- function(scope, fmt, ...) {
  renv_log_impl(3L, scope, fmt, ...)
}

ilog <- function(scope, fmt, ...) {
  renv_log_impl(2L, scope, fmt, ...)
}

dlog <- function(scope, fmt, ...) {
  renv_log_impl(1L, scope, fmt, ...)
}


renv_log_impl <- function(level, scope, fmt, ...) {

  # check log level
  if (level < the$log_level)
    return()

  # only include scopes matching the scopes
  scopes <- the$log_scopes
  if (is.character(scopes) && !scope %in% scopes)
    return()

  # build message
  message <- sprintf(fmt, ...)

  # annotate with prefix from scope, timestamp
  fmt <- "%sZ [renv-%i] %s: %s"
  now <- format(Sys.time(), format = "%Y-%m-%d %H:%M:%OS6", tz = "UTC")
  all <- sprintf(fmt, now, Sys.getpid(), scope, message)

  # write it out
  cat(all, file = the$log_file, sep = "\n", append = TRUE)

}

renv_log_init <- function() {
  the$log_level  <- renv_log_level()
  the$log_file   <- renv_log_file()
  the$log_scopes <- renv_log_scopes()
}

renv_log_level <- function() {

  level <- Sys.getenv("RENV_LOG_LEVEL", unset = NA)
  if (is.na(level))
    return(4L)

  case(
    level %in% c("4", "error",   "ERROR")   ~ 4L,
    level %in% c("3", "warning", "WARNING") ~ 3L,
    level %in% c("2", "info",    "INFO")    ~ 2L,
    level %in% c("1", "debug",   "DEBUG")   ~ 1L,
    ~ {
      warningf("ignoring invalid RENV_LOG_LEVEL '%s'", level)
      4L
    }
  )

}

renv_log_file <- function() {

  # check for log file
  file <- Sys.getenv("RENV_LOG_FILE", unset = NA)
  if (!is.na(file))
    return(file)

  # default to stderr, since it's unbuffered
  stderr()

}

renv_log_scopes <- function() {

  scopes <- Sys.getenv("RENV_LOG_SCOPES", unset = NA)
  if (is.na(scopes))
    return(NULL)

  strsplit(scopes, ",", fixed = TRUE)[[1L]]

}



# manifest-convert.R ---------------------------------------------------------


#' Generate `renv.lock` from an RStudio Connect `manifest.json`
#'
#' Use `renv_lockfile_from_manifest()` to convert a `manifest.json` file from
#' an RStudio Connect content bundle into an `renv.lock` lockfile.
#'
#' This function can be useful when you need to recreate the package environment
#' of a piece of content that is deployed to RStudio Connect. The content bundle
#' contains a `manifest.json` file that is used to recreate the package
#' environment. This function will let you convert that manifest file to an
#' `renv.lock` file. Run `renv::restore()` after you've converted the file to
#' restore the package environment.
#'
#' @param manifest
#'   The path to a `manifest.json` file.
#'
#' @param lockfile
#'   The path to the lockfile to be generated and / or updated.
#'   When `NA` (the default), the generated lockfile is returned as an \R
#'   object; otherwise, the lockfile will be written to the path specified by
#'   `lockfile`.
#'
#' @details
#' By default the `lockfile` argument is set to `NA`. This will not create a new
#' `renv.lock` file. Rather, it will return a lockfile object (see `?lockfile`)
#' that can be used to create a new `renv.lock` file. If `lockfile` is set to a
#' character string, a new file will be created with that path -- e.g.
#' `renv.lock` -- and the lockfile object will be returned.
#'
#' @return
#' An renv lockfile.
#'
#' @keywords internal
renv_lockfile_from_manifest <- function(manifest,
                                        lockfile = NA,
                                        project = NULL)
{
  renv_scope_error_handler()
  project <- renv_project_resolve(project)

  # read the manifest (accept both lists and file paths)
  manifest <- case(
    is.character(manifest) ~ renv_json_read(manifest),
    is.list(manifest)      ~ manifest,
    TRUE                   ~ renv_type_unexpected(manifest)
  )

  # convert descriptions into records
  records <- map(manifest[["packages"]], function(entry) {
    desc <- entry[["description"]]
    renv_snapshot_description_impl(desc)
  })

  # extract repositories from descriptions
  repos <- list()
  for (entry in manifest[["packages"]]) {

    if (is.null(entry[["Repository"]]))
      next

    src <- entry[["Source"]] %||% "CRAN"
    repo <- entry[["Repository"]]

    repos[[src]] <- repo

  }

  # extract version
  version <- numeric_version(manifest[["platform"]] %||% getRversion())

  # create R field for lockfile
  r <- list(Version = version, Repositories = repos)

  # create the lockfile
  lock <- list(R = r, Packages = records)
  class(lock) <- "renv_lockfile"

  # return lockfile as R object if requested
  if (is.na(lockfile))
    return(lock)

  # otherwise, write to file and report for user
  renv_lockfile_write(lock, file = lockfile)
  fmt <- "- Lockfile written to %s."
  writef(fmt, renv_path_pretty(lockfile))

  invisible(lock)

}


# mask.R ---------------------------------------------------------------------


# functions which mask internal / base R equivalents, usually to provide
# backwards compatibility or guard against common errors

numeric_version <- function(x, strict = TRUE) {
  base::numeric_version(as.character(x), strict = strict)
}

sprintf <- function(fmt, ...) {
  if (nargs() == 1L)
    fmt
  else
    base::sprintf(fmt, ...)
}

unique <- function(x) {
  base::unique(x)
}

# a wrapper for 'utils::untar()' that throws an error if untar fails
untar <- function(tarfile,
                  files = NULL,
                  list = FALSE,
                  exdir = ".",
                  tar = Sys.getenv("TAR"))
{
  # delegate to utils::untar()
  result <- utils::untar(
    tarfile = tarfile,
    files   = files,
    list    = list,
    exdir   = exdir,
    tar     = tar
  )

  # check for errors (tar returns a status code)
  if (is.integer(result) && result != 0L) {
    call <- stringify(sys.call())
    stopf("'%s' returned status code %i", call, result)
  }

  # return other results as-is
  result
}



# memoize.R ------------------------------------------------------------------


the$memoize <- new.env(parent = emptyenv())

memo <- function(value, scope = NULL) {
  scope <- scope %||% stringify(sys.call(sys.parent())[[1L]])
  (the$memoize[[scope]] <- the$memoize[[scope]] %||% value)
}

memoize <- function(key, value, scope = NULL) {

  # figure out scope to use
  scope <- scope %||% stringify(sys.call(sys.parent())[[1L]])

  # initialize memoized environment
  envir <-
    the$memoize[[scope]] <-
    the$memoize[[scope]] %||%
    new.env(parent = emptyenv())

  # retrieve, or compute, memoized value
  envir[[key]] <- envir[[key]] %||% value

}


# metadata.R -----------------------------------------------------------------


# NOTE: 'the$metadata' is initialized either in 'renv_metadata_init()', for
# stand-alone installations of renv, or via an embedded initialize script for
# vendored copies of renv.

renv_metadata_create <- function(embedded, version) {
  list(embedded = embedded, version = version)
}

renv_metadata_embedded <- function() {
  the$metadata$embedded
}

renv_metadata_version <- function() {
  the$metadata$version
}

renv_metadata_version_create <- function(record) {
  version <- record[["Version"]]
  attr(version, "sha") <- record[["RemoteSha"]]
  version
}

renv_metadata_remote <- function(metadata = the$metadata) {

  # check for development versions
  sha <- attr(metadata$version, "sha")
  if (!is.null(sha) && nzchar(sha))
    return(paste("rstudio/renv", sha, sep = "@"))

  # otherwise, use release version
  paste("renv", metadata$version, sep = "@")

}

renv_metadata_version_friendly <- function(metadata = the$metadata,
                                           shafmt = NULL)
{
  renv_bootstrap_version_friendly(
    version = metadata$version,
    shafmt  = shafmt
  )
}

renv_metadata_init <- function() {

  # if renv was embedded, then the$metadata should already be initialized
  if (!is.null(the$metadata))
    return()

  # renv doesn't appear to be embedded; initialize metadata
  path <- renv_namespace_path("renv")
  record <- renv_description_read(path = file.path(path, "DESCRIPTION"))
  version <- renv_metadata_version_create(record)

  the$metadata <- renv_metadata_create(
    embedded = FALSE,
    version  = version
  )

}


# methods.R ------------------------------------------------------------------


renv_methods_map <- function() {

  list(

    renv_path_normalize = c(
      unix  = "renv_path_normalize_unix",
      win32 = "renv_path_normalize_win32"
    ),

    renv_file_exists = c(
      unix  = "renv_file_exists_unix",
      win32 = "renv_file_exists_win32"
    ),

    renv_file_list_impl = c(
      unix  = "renv_file_list_impl_unix",
      win32 = "renv_file_list_impl_win32"
    ),

    renv_file_broken = c(
      unix  = "renv_file_broken_unix",
      win32 = "renv_file_broken_win32"
    ),

    renv_paths_sandbox = c(
      unix  = "renv_paths_sandbox_unix",
      win32 = "renv_paths_sandbox_win32"
    )

  )

}

renv_methods_init <- function() {

  # get list of method mappings
  methods <- renv_methods_map()

  # determine appropriate lookup key for finding alternative
  key <- if (renv_platform_windows()) "win32" else "unix"
  alts <- map(methods, `[[`, key)

  # update methods in namespace
  envir <- renv_envir_self()
  enumerate(alts, function(name, alt) {
    replacement <- eval(parse(text = alt), envir = envir)
    assign(name, replacement, envir = envir)
  })

}

renv_methods_error <- function() {
  call <- sys.call(sys.parent())
  fmt <- "internal error: '%s()' not initialized in .onLoad()"
  stopf(fmt, as.character(call[[1L]]), call. = FALSE)
}


# migrate.R ------------------------------------------------------------------


#' Migrate a project from packrat to renv
#'
#' Migrate a project's infrastructure from packrat to renv.
#'
#' # Migration
#'
#' When migrating Packrat projects to renv, the set of components migrated
#' can be customized using the `packrat` argument. The set of components that
#' can be migrated are as follows:
#'
#' \tabular{ll}{
#'
#' **Name** \tab **Description** \cr
#'
#' `lockfile` \tab
#'   Migrate the Packrat lockfile (`packrat/packrat.lock`) to the renv lockfile
#'   (`renv.lock`). \cr
#'
#' `sources` \tab
#'   Migrate package sources from the `packrat/src` folder to the renv
#'   sources folder. Currently, only CRAN packages are migrated to renv --
#'   packages retrieved from other sources (e.g. GitHub) are ignored.
#'   \cr
#'
#' `library` \tab
#'   Migrate installed packages from the Packrat library to the renv project
#'   library.
#'   \cr
#'
#' `options` \tab
#'   Migrate compatible Packrat options to the renv project.
#'   \cr
#'
#' `cache` \tab
#'   Migrate packages from the Packrat cache to the renv cache.
#'   \cr
#'
#' }
#'
#' @inherit renv-params
#'
#' @param packrat Components of the Packrat project to migrate. See the default
#'   argument list for components of the Packrat project that can be migrated.
#'   Select a subset of those components for migration as appropriate.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # migrate Packrat project infrastructure to renv
#' renv::migrate()
#'
#' }
migrate <- function(
  project = NULL,
  packrat = c("lockfile", "sources", "library", "options", "cache"))
{
  renv_consent_check()
  renv_scope_error_handler()

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  project <- renv_path_normalize(project, mustWork = TRUE)
  if (file.exists(file.path(project, "packrat/packrat.lock"))) {
    packrat <- match.arg(packrat, several.ok = TRUE)
    renv_migrate_packrat(project, packrat)
  }

  invisible(project)
}

renv_migrate_packrat <- function(project = NULL, components = NULL) {
  project <- renv_project_resolve(project)

  if (!requireNamespace("packrat", quietly = TRUE))
    stopf("migration requires the 'packrat' package to be installed")

  callbacks <- list(
    lockfile = renv_migrate_packrat_lockfile,
    sources  = renv_migrate_packrat_sources,
    library  = renv_migrate_packrat_library,
    options  = renv_migrate_packrat_options,
    cache    = renv_migrate_packrat_cache
  )

  components <- components %||% names(callbacks)
  callbacks <- callbacks[components]
  for (callback in callbacks)
    callback(project)

  renv_migrate_packrat_infrastructure(project)
  renv_imbue_impl(project)

  fmt <- "- Project '%s' has been migrated from Packrat to renv."
  writef(fmt, renv_path_aliased(project))

  writef("- Consider deleting the project 'packrat' folder if it is no longer needed.")
  invisible(TRUE)
}

renv_migrate_packrat_lockfile <- function(project) {

  plock <- file.path(project, "packrat/packrat.lock")
  if (!file.exists(plock))
    return(FALSE)

  # read the lockfile
  contents <- read(plock)
  splat <- strsplit(contents, "\n{2,}")[[1]]
  dcf <- lapply(splat, function(section) {
    renv_dcf_read(text = section)
  })

  # split into header + package fields
  header <- dcf[[1]]
  records <- dcf[-1L]

  # parse the repositories
  repos <- getOption("repos")
  if (!is.null(header$Repos)) {
    parts <- strsplit(header$Repos, "\\s*,\\s*")[[1]]
    repos <- renv_properties_read(text = parts, delimiter = "=")
  }

  # fix-up some record fields for renv
  fields <- c("Package", "Version", "Source")
  records <- lapply(records, function(record) {

    # remove an old packrat hash
    record$Hash <- NULL

    # add RemoteType for GitHub records
    if (any(grepl("^Github", names(record))))
      record$RemoteType <- "github"

    # remap '^Github'-style records to '^Remote'
    map <- c(
      "GithubRepo"     = "RemoteRepo",
      "GithubUsername" = "RemoteUsername",
      "GithubRef"      = "RemoteRef",
      "GithubSha1"     = "RemoteSha",
      "GithubSHA1"     = "RemoteSha",
      "GithubSubdir"   = "RemoteSubdir"
    )
    names(record) <- remap(names(record), map)

    # keep only fields of interest
    keep <- c(fields, grep("^Remote", names(record), value = TRUE))
    as.list(record[keep])

  })

  # pull out names for records
  names(records) <- extract_chr(records, "Package")

  # ensure renv is added
  records <- renv_snapshot_fixup_renv(records)

  # generate a blank lockfile
  lockfile <- structure(list(), class = "renv_lockfile")
  lockfile$R <- renv_lockfile_init_r(project)

  # update fields
  lockfile$R$Version <- header$RVersion
  lockfile$R$Repositories <- as.list(repos)
  renv_lockfile_records(lockfile) <- records

  # finish
  lockfile <- renv_lockfile_fini(lockfile, project)

  # write the lockfile
  lockpath <- renv_lockfile_path(project = project)
  renv_lockfile_write(lockfile, file = lockpath)

}

renv_migrate_packrat_sources <- function(project) {

  packrat <- asNamespace("packrat")
  srcdir <- packrat$srcDir(project = project)
  if (!file.exists(srcdir))
    return(TRUE)

  pattern <- paste0(
    "^",                   # start
    "[^_]+",               # package name
    "_",                   # separator
    "\\d+(?:[_.-]\\d+)*",  # version
    "\\.tar\\.gz",         # extension
    "$"                    # end
  )

  suffixes <- list.files(
    srcdir,
    pattern = pattern,
    recursive = TRUE
  )

  sources <- file.path(srcdir, suffixes)
  targets <- renv_paths_source("cran", suffixes)

  keep <- !file.exists(targets)
  sources <- sources[keep]; targets <- targets[keep]

  printf("- Migrating package sources from Packrat to renv ... ")
  copy <- renv_progress_callback(renv_file_copy, length(targets))
  mapply(sources, targets, FUN = function(source, target) {
    ensure_parent_directory(target)
    copy(source, target)
  })
  writef("Done!")

  TRUE

}

renv_migrate_packrat_library <- function(project) {

  packrat <- asNamespace("packrat")

  libdir <- packrat$libDir(project = project)
  if (!file.exists(libdir))
    return(TRUE)

  sources <- list.files(libdir, full.names = TRUE)
  if (empty(sources))
    return(TRUE)

  targets <- renv_paths_library(basename(sources), project = project)

  names(targets) <- sources
  targets <- targets[!file.exists(targets)]
  if (empty(targets)) {
    writef("- The renv library is already synchronized with the Packrat library.")
    return(TRUE)
  }

  # copy packages from Packrat to renv private library
  printf("- Migrating library from Packrat to renv ... ")
  ensure_parent_directory(targets)
  copy <- renv_progress_callback(renv_file_copy, length(targets))
  enumerate(targets, copy)
  writef("Done!")

  # move packages into the cache
  if (renv_cache_config_enabled(project = project)) {
    printf("- Moving packages into the renv cache ... ")
    records <- lapply(targets, renv_description_read)
    sync <- renv_progress_callback(renv_cache_synchronize, length(targets))
    lapply(records, sync, linkable = TRUE)
    writef("Done!")
  }

  TRUE

}

renv_migrate_packrat_options <- function(project) {

  packrat <- asNamespace("packrat")
  opts <- packrat$get_opts(project = project)

  settings$ignored.packages(opts$ignored.packages, project = project)

}

renv_migrate_packrat_cache <- function(project) {

  # find packages in the packrat cache
  packrat <- asNamespace("packrat")
  cache <- packrat$cacheLibDir()
  packages <- list.files(cache, full.names = TRUE)
  hashes <- list.files(packages, full.names = TRUE)
  sources <- list.files(hashes, full.names = TRUE)

  # sanity check: make sure the source folder is an R package
  ok <- file.exists(file.path(sources, "DESCRIPTION"))
  sources <- sources[ok]

  # construct cache target paths
  targets <- map_chr(sources, renv_cache_path)
  names(targets) <- sources

  # only copy to cache target paths that don't exist
  targets <- targets[!file.exists(targets)]
  if (empty(targets)) {
    writef("- The renv cache is already synchronized with the Packrat cache.")
    return(TRUE)
  }

  # cache each installed package
  if (renv_cache_config_enabled(project = project))
    renv_migrate_packrat_cache_impl(targets)

  TRUE

}

renv_migrate_packrat_cache_impl <- function(targets) {

  # attempt to copy packages from Packrat to renv cache
  printf("- Migrating Packrat cache to renv cache ... ")
  ensure_parent_directory(targets)
  copy <- renv_progress_callback(renv_file_copy, length(targets))

  result <- enumerate(targets, function(source, target) {
    status <- catch(copy(source, target))
    broken <- inherits(status, "error")
    reason <- if (broken) conditionMessage(status) else ""
    list(source = source, target = target, broken = broken, reason = reason)
  })

  writef("Done!")

  # report failures
  status <- bind(result)
  bad <- status[status$broken, ]
  if (nrow(bad) == 0)
    return(TRUE)

  caution_bullets(
    "The following packages could not be copied from the Packrat cache:",
    with(bad, sprintf("%s [%s]", format(source), reason)),
    "These packages may need to be reinstalled and re-cached."
  )

}

renv_migrate_packrat_infrastructure <- function(project) {
  unlink(file.path(project, ".Rprofile"))
  renv_infrastructure_write(project)
  writef("- renv support infrastructure has been written.")
  TRUE
}


# modify.R -------------------------------------------------------------------


#' Modify a Lockfile
#'
#' Modify a project's lockfile, either interactively or non-interactively.
#'
#' After edit, if the lockfile edited is associated with the active project, any
#' state-related changes (e.g. to \R repositories) will be updated in the
#' current session.
#'
#' @inherit renv-params
#'
#' @param changes A list of changes to be merged into the lockfile.
#'   When `NULL` (the default), the lockfile is instead opened for
#'   interactive editing.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # modify an existing lockfile
#' if (interactive())
#'   renv::modify()
#'
#' }
modify <- function(project = NULL, changes = NULL) {
  renv_scope_error_handler()
  project <- renv_project_resolve(project)
  renv_project_lock(project = project)
  renv_modify_impl(project, changes)
  invisible(project)
}

renv_modify_impl <- function(project, changes) {

  lockfile <- if (is.null(changes))
    renv_modify_interactive(project)
  else
    renv_modify_noninteractive(project, changes)

  if (renv_project_loaded(project))
    renv_modify_fini(lockfile)

}

renv_modify_interactive <- function(project) {

  # check for interactive session
  if (!interactive())
    stop("can't modify lockfile in non-interactive session")

  # resolve path to lockfile
  lockpath <- renv_lockfile_path(project)
  if (!file.exists(lockpath))
    stopf("lockfile '%s' does not exist", renv_path_aliased(lockpath))

  # copy the lockfile to a temporary file
  dir <- renv_scope_tempfile("renv-lockfile-")
  ensure_directory(dir)

  templock <- file.path(dir, "renv.lock")
  file.copy(lockpath, templock)

  # edit the temporary lockfile
  renv_file_edit(templock)

  # check that the new lockfile can be read
  withCallingHandlers(
    lockfile <- catch(renv_lockfile_read(file = templock)),
    error = function(cnd) {
      stop(lines(
        "renv was unable to parse the modified lockfile:",
        conditionMessage(cnd),
        "Your changes will be discarded"
      ))
    }
  )

  lockfile

}

renv_modify_noninteractive <- function(project, changes) {

  # resolve path to lockfile
  lockpath <- renv_lockfile_path(project)
  if (!file.exists(lockpath))
    stopf("lockfile '%s' does not exist", renv_path_aliased(lockpath))

  # read it
  lockfile <- renv_lockfile_read(file = lockpath)

  # merge changes
  merged <- overlay(lockfile, changes)

  # write updated lockfile to a temporary file
  templock <- renv_scope_tempfile("renv-lock-")
  renv_lockfile_write(merged, file = templock)

  # try reading it once more
  newlock <- renv_lockfile_read(file = templock)
  if (!identical(merged, newlock))
    stop("modify produced an invalid lockfile")

  # overwrite the original lockfile
  file.rename(templock, lockpath)

  # finish up
  merged

}

renv_modify_fini <- function(lockfile) {

  # synchronize relevant changes into the session
  repos <- lockfile$R$Repositories
  options(repos = convert(repos, "character"))

}


# mran.R ---------------------------------------------------------------------


renv_mran_enabled <- function() {
  !identical(getOption("pkgType"), "source") && config$mran.enabled()
}

renv_mran_database_path <- function() {
  renv_paths_mran("packages.rds")
}

renv_mran_database_encode <- function(database) {
  database <- as.list(database)
  encoded <- lapply(database, renv_mran_database_encode_impl)
  encoded[order(names(encoded))]
}

renv_mran_database_encode_impl <- function(entry) {

  entry <- as.list(entry)
  keys <- names(entry)
  vals <- unlist(entry)

  splat <- strsplit(keys, " ", fixed = TRUE)

  encoded <- data_frame(
    Package          = map_chr(splat, `[[`, 1L),
    Version          = map_chr(splat, `[[`, 2L),
    Date             = as.integer(vals)
  )

  encoded <- encoded[order(encoded$Package, encoded$Version), ]
  rownames(encoded) <- NULL

  encoded$Package <- as.factor(encoded$Package)
  encoded$Version <- as.factor(encoded$Version)

  encoded

}

renv_mran_database_decode <- function(encoded) {
  decoded <- lapply(encoded, renv_mran_database_decode_impl)
  list2env(decoded, parent = emptyenv())
}

renv_mran_database_decode_impl <- function(entry) {

  entry$Package <- as.character(entry$Package)
  entry$Version <- as.character(entry$Version)

  keys <- paste(entry$Package, entry$Version)
  vals <- as.list(entry$Date)
  names(vals) <- keys

  envir <- list2env(vals, parent = emptyenv())
  attr(envir, "keys") <- keys

  envir

}

renv_mran_database_save <- function(database, path = NULL) {

  path <- path %||% renv_mran_database_path()
  ensure_parent_directory(path)

  encoded <- renv_mran_database_encode(database)

  conn <- xzfile(path)
  defer(close(conn))
  saveRDS(encoded, file = conn, version = 2L)

}

renv_mran_database_load <- function(path = NULL) {

  filebacked(
    context  = "renv_mran_database_load",
    path     = path %||% renv_mran_database_path(),
    callback = renv_mran_database_load_impl
  )

}

renv_mran_database_load_impl <- function(path) {

  # read from database file if it exists
  if (file.exists(path)) {
    encoded <- readRDS(path)
    return(renv_mran_database_decode(encoded))
  }

  # otherwise, initialize a new database
  new.env(parent = emptyenv())

}

renv_mran_database_dates <- function(version, all = TRUE) {

  # release dates for old versions of R
  releases <- c(
    "3.2" = "2015-04-16",
    "3.3" = "2016-05-03",
    "3.4" = "2017-04-21",
    "3.5" = "2018-04-23",
    "3.6" = "2019-04-26",
    "4.0" = "2020-04-24",
    "4.1" = "2021-05-18",
    "4.2" = "2022-04-22",
    "4.3" = "2023-05-18",  # a guess
    "4.4" = "2024-05-18",  # a guess
    NULL
  )

  # find the start date
  index <- match(version, names(releases))
  if (is.na(index))
    stopf("no known release date for R %s", version)

  start <- as.Date(releases[[index]])
  if (!all)
    return(start)

  # form end date (ensure not in future)
  # we look 2 releases in the future as R builds binaries for
  # the previous releases of R as well
  end <- min(
    as.Date(releases[[index + 2L]]),
    as.Date(Sys.time(), tz = "UTC")
  )

  # generate list of dates
  seq(start, end, by = 1L)

}

renv_mran_database_key <- function(platform, version) {
  sprintf("/bin/%s/contrib/%s", platform, version)
}

renv_mran_database_update <- function(platform, version, dates = NULL) {

  # load database
  database <- renv_mran_database_load()

  # get reference to entry in database (initialize if not yet created)
  suffix <- renv_mran_database_key(platform, version)
  database[[suffix]] <- database[[suffix]] %||% new.env(parent = emptyenv())
  entry <- database[[suffix]]

  # rough release dates for R releases
  dates <- as.list(dates %||% renv_mran_database_dates(version))

  for (date in dates) {

    # attempt to update our database entry for this date
    url <- renv_mran_url(date, suffix)
    tryCatch(
      renv_mran_database_update_impl(date, url, entry),
      error = warnify
    )

  }

  # save at end
  printf("[%s]: saving database ... ", date)
  renv_mran_database_save(database)
  writef("DONE")

}

renv_mran_database_update_impl <- function(date, url, entry) {

  printf("[%s]: reading package database ... ", date)

  # get date as number of days since epoch
  idate <- as.integer(date)

  # retrieve available packages
  errors <- new.env(parent = emptyenv())
  db <- renv_available_packages_query_impl(url, errors)
  if (is.null(db)) {
    writef("ERROR")
    return(FALSE)
  }

  # insert packages into database
  for (i in seq_len(nrow(db))) {

    # construct key for index
    name <- db[i, "Package"]
    vers <- db[i, "Version"]
    key <- paste(name, vers)

    # update database
    entry[[key]] <- max(entry[[key]] %||% 0L, idate)

  }

  writef("OK")
  TRUE

}

renv_mran_url <- function(date, suffix) {
  root <- Sys.getenv("RENV_MRAN_URL", unset = "https://mran.microsoft.com/snapshot")
  snapshot <- file.path(root, date)
  paste(snapshot, suffix, sep = "")
}

renv_mran_database_url <- function() {
  default <- "https://rstudio-buildtools.s3.amazonaws.com/renv/mran/packages.rds"
  Sys.getenv("RENV_MRAN_DATABASE_URL", unset = default)
}

renv_mran_database_refresh <- function(explicit = TRUE) {

  if (explicit || renv_mran_database_refresh_required())
    renv_mran_database_refresh_impl()

}

renv_mran_database_refresh_required <- function() {
  dynamic(
    key   = list(),
    value = renv_mran_database_refresh_required_impl()
  )
}

renv_mran_database_refresh_required_impl <- function() {

  # if the cache doesn't exist, we must refresh
  path <- renv_mran_database_path()
  if (!file.exists(path))
    return(TRUE)

  # if we're using an older version of R, but we have newer package
  # versions available in the cache, we don't need to refresh
  db <- tryCatch(renv_mran_database_load(), error = identity)
  if (!inherits(db, "error")) {
    keys <- names(db)
    versions <- unique(basename(keys))
    if (any(versions > getRversion()))
      return(FALSE)
  }

  # read the file mtime
  info <- renv_file_info(path)
  if (is.na(info$mtime))
    return(FALSE)

  # if it's older than a day, then we should update
  difftime(Sys.time(), info$mtime, units = "days") > 1

}

renv_mran_database_refresh_impl <- function() {

  url  <- renv_mran_database_url()
  path <- renv_mran_database_path()

  if (nzchar(url) && nzchar(path)) {
    ensure_parent_directory(path)
    download(url = url, destfile = path, quiet = TRUE)
  }

}

renv_mran_database_sync <- function(platform, version) {

  # read database
  database <- renv_mran_database_load()

  # read entry for this platform + version combo
  key <- renv_mran_database_key(platform, version)
  entry <- database[[key]]
  if (is.null(entry)) {
    database[[key]] <- new.env(parent = emptyenv())
    entry <- database[[key]]
  }

  # get the last known updated date
  last <- max(0L, as.integer(as.list(entry)))
  if (identical(last, 0L)) {
    date <- renv_mran_database_dates(version, all = FALSE)
    last <- as.integer(date)
  }

  # get yesterday's date
  now <- as.integer(as.Date(Sys.time(), tz = "UTC")) - 1L

  # sync up to the last version's release date
  dates <- as.integer(renv_mran_database_dates(version))
  now <- min(now, max(dates))

  # if we've already in sync, nothing to do
  if (last >= now)
    return(FALSE)

  # invoke update for missing dates
  writef("==> Synchronizing MRAN database (%s/%s)", platform, version)
  dates <- as.Date(seq(last + 1L, now, by = 1L), origin = "1970-01-01")
  renv_mran_database_update(platform, version, dates)
  writef("Finished synchronizing MRAN database (%s/%s)", platform, version)

  # return TRUE to indicate update occurred
  return(TRUE)

}

renv_mran_database_sync_all <- function() {

  # NOTE: this needs to be manually updated since the binary URL for
  # packages can change from version to version, especially on macOS

  # R 3.2
  renv_mran_database_sync("windows", "3.2")
  renv_mran_database_sync("macosx/mavericks", "3.2")

  # R 3.3
  renv_mran_database_sync("windows", "3.3")
  renv_mran_database_sync("macosx/mavericks", "3.3")

  # R 3.4
  renv_mran_database_sync("windows", "3.4")
  renv_mran_database_sync("macosx/el-capitan", "3.4")

  # R 3.5
  renv_mran_database_sync("windows", "3.5")
  renv_mran_database_sync("macosx/el-capitan", "3.5")

  # R 3.6
  renv_mran_database_sync("windows", "3.6")
  renv_mran_database_sync("macosx/el-capitan", "3.6")

  # R 4.0
  renv_mran_database_sync("windows", "4.0")
  renv_mran_database_sync("macosx", "4.0")

  # R 4.1
  renv_mran_database_sync("windows", "4.1")
  renv_mran_database_sync("macosx", "4.1")
  renv_mran_database_sync("macosx/big-sur-arm64", "4.1")



}


# namespace.R ----------------------------------------------------------------


renv_namespace_spec <- function(package) {
  namespace <- asNamespace(package)
  .getNamespaceInfo(namespace, "spec")
}

renv_namespace_version <- function(package) {
  spec <- renv_namespace_spec(package)
  spec[["version"]]
}

renv_namespace_path <- function(package) {
  namespace <- asNamespace(package)
  .getNamespaceInfo(namespace, "path")
}

renv_namespace_load <- function(package) {
  suppressPackageStartupMessages(getNamespace(package))
}

renv_namespace_unload <- function(package) {
  unloadNamespace(package)
}

renv_namespace_parse <- function(package) {

  parseNamespaceFile(
    package     = package,
    package.lib = dirname(renv_package_find(package)),
    mustExist   = TRUE
  )

}


# new.R ----------------------------------------------------------------------


new <- function(expr) {

  private <- new.env(parent = renv_envir_self())
  public  <- new.env(parent = private)

  for (expr in as.list(substitute(expr))[-1L]) {

    assigning <- renv_call_matches(expr, name = c("=", "<-"))

    if (!assigning)
      return(eval(expr, envir = public))

    hidden <-
      is.symbol(expr[[2L]]) &&
      substring(as.character(expr[[2L]]), 1L, 1L) == "."

    eval(expr, envir = if (hidden) private else public)

  }

  public

}


# nexus.R --------------------------------------------------------------------


renv_nexus_enabled <- function(repo) {

  # first, check a global option
  enabled <- getOption("renv.nexus.enabled", default = FALSE)
  if (enabled)
    return(TRUE)

  # otherwise, check cached repository information
  info <- renv_repos_info(repo)
  identical(info$nexus, TRUE)

}


# once.R ---------------------------------------------------------------------


# mechanism for running a block of code only once
the$once <- new.env(parent = emptyenv())

once <- function() {

  call <- sys.call(sys.parent())[[1L]]
  id <- as.character(call)

  once <- the$once[[id]] %||% TRUE
  the$once[[id]] <- FALSE

  once

}


# options.R ------------------------------------------------------------------


renv_options_set <- function(key, value) {
  data <- list(value)
  names(data) <- key
  do.call(base::options, data)
}

renv_options_resolve <- function(value, arguments) {

  if (is.function(value))
    return(do.call(value, arguments))

  value

}

renv_options_override <- function(scope, key, default = NULL, extra = NULL) {

  # first, check for scoped option
  value <- getOption(paste(scope, key, sep = "."))
  if (!is.null(value))
    return(renv_options_resolve(value, list(extra)))

  # next, check for unscoped option
  value <- getOption(scope)
  if (key %in% names(value))
    return(renv_options_resolve(value[[key]], list(extra)))

  # resolve option value
  if (!is.null(value))
    return(renv_options_resolve(value, list(key, extra)))

  # nothing found; use default
  default

}


# package.R ------------------------------------------------------------------


# NOTE: intentionally checks library paths before checking loaded namespaces
renv_package_find <- function(package,
                              lib.loc = renv_libpaths_all(),
                              check.loaded = TRUE)
{
  map_chr(
    package,
    renv_package_find_impl,
    lib.loc = lib.loc,
    check.loaded = check.loaded
  )
}

renv_package_find_impl <- function(package,
                                   lib.loc = renv_libpaths_all(),
                                   check.loaded = TRUE)
{
  # if we've been given the path to an existing package, use it as-is
  if (grepl("/", package) && file.exists(file.path(package, "DESCRIPTION")))
    return(renv_path_normalize(package, mustWork = TRUE))

  # first, look in the library paths
  for (libpath in lib.loc) {
    pkgpath <- file.path(libpath, package)
    descpath <- file.path(pkgpath, "DESCRIPTION")
    if (file.exists(descpath))
      return(pkgpath)
  }

  # if that failed, check to see if it's loaded and use the associated path
  if (check.loaded && package %in% loadedNamespaces()) {
    path <- renv_namespace_path(package)
    if (file.exists(path))
      return(path)
  }

  # failed to find package
  ""
}

renv_package_installed <- function(package, lib.loc = renv_libpaths_all()) {
  paths <- renv_package_find(package, lib.loc, check.loaded = FALSE)
  nzchar(paths)
}

renv_package_available <- function(package) {
  package %in% loadedNamespaces() || renv_package_installed(package)
}

renv_package_version <- function(package) {
  renv_package_description_field(package, "Version")
}

renv_package_description_field <- function(package, field) {
  path <- renv_package_find(package)
  desc <- renv_description_read(path)
  desc[[field]]
}

renv_package_type <- function(path, quiet = FALSE, default = "source") {

  info <- renv_file_info(path)
  if (is.na(info$isdir))
    stopf("no package at path '%s'", renv_path_aliased(path))

  # for directories, check for Meta
  if (info$isdir) {
    hasmeta <- file.exists(file.path(path, "Meta"))
    type <- if (hasmeta) "binary" else "source"
    return(type)
  }

  # otherwise, guess based on contents of package
  methods <- list(
    tar = function(path) untar(tarfile = path, list = TRUE),
    zip = function(path) unzip(zipfile = path, list = TRUE)$Name
  )

  # guess appropriate method when possible
  type <- renv_archive_type(path)
  if (type %in% c("tar", "zip"))
    methods <- methods[type]

  for (method in methods) {

    # suppress warnings to avoid issues with e.g.
    # 'skipping pax global extended headers' when
    # using internal tar
    files <- catch(suppressWarnings(method(path)))
    if (inherits(files, "error"))
      next

    hasmeta <- any(grepl("^[^/]+/Meta/?$", files))
    type <- if (hasmeta) "binary" else "source"
    return(type)

  }

  if (!quiet) {
    fmt <- "failed to determine type of package '%s'; assuming source"
    warningf(fmt, renv_path_aliased(path))
  }

  default

}

renv_package_priority <- function(package) {

  # treat 'R' as pseudo base package
  if (package == "R")
    return("base")

  # read priority from db
  db <- installed_packages()
  entry <- db[db$Package == package, ]
  entry$Priority %NA% ""

}

renv_package_tarball_name <- function(path) {
  desc <- renv_description_read(path)
  with(desc, sprintf("%s_%s.tar.gz", Package, Version))
}

renv_package_ext <- function(type) {

  # always use '.tar.gz' for source packages
  type <- match.arg(type, c("binary", "source"))
  if (type == "source")
    return(".tar.gz")

  # otherwise, infer appropriate extension based on platform
  case(
    renv_platform_macos()   ~ ".tgz",
    renv_platform_windows() ~ ".zip",
    renv_platform_unix()    ~ ".tar.gz"
  )

}

renv_package_pkgtypes <- function() {

  # only use binaries if the user has specifically requested it
  # and binaries are available for this installation of R
  # (users may want to install from sources explicitly to take
  # advantage of custom local compiler configurations)
  binaries <-
    !identical(.Platform$pkgType, "source") &&
    !identical(getOption("pkgType"), "source")

  if (binaries) c("binary", "source") else "source"

}

renv_package_augment <- function(installpath, record) {

  # check for remotes fields
  remotes <- record[grep("^Remote", names(record))]
  if (empty(remotes))
    return(FALSE)

  # for backwards compatibility with older versions of Packrat,
  # we write out 'Github*' fields as well
  if (identical(record$Source, "GitHub")) {

    map <- list(
      "GithubHost"     = "RemoteHost",
      "GithubRepo"     = "RemoteRepo",
      "GithubUsername" = "RemoteUsername",
      "GithubRef"      = "RemoteRef",
      "GithubSHA1"     = "RemoteSha"
    )

    enumerate(map, function(old, new) {
      remotes[[old]] <<- remotes[[old]] %||% remotes[[new]]
    })

  }

  # ensure RemoteType field is written out
  remotes$RemoteType <- remotes$RemoteType %||% renv_record_source(record)
  remotes <- remotes[c("RemoteType", renv_vector_diff(names(remotes), "RemoteType"))]

  # update package items
  renv_package_augment_description(installpath, remotes)
  renv_package_augment_metadata(installpath, remotes)

}

renv_package_augment_impl <- function(data, remotes) {
  remotes <- remotes[map_lgl(remotes, Negate(is.null))]
  nonremotes <- grep("^(?:Remote|Github)", names(data), invert = TRUE)
  remotes[["Remotes"]] <- data[["Remotes"]] %||% remotes[["Remotes"]]
  c(data[nonremotes], remotes)
}

renv_package_augment_description <- function(path, remotes) {

  descpath <- file.path(path, "DESCRIPTION")

  before <- renv_description_read(descpath)
  after <- renv_package_augment_impl(before, remotes)
  if (identical(before, after))
    return(FALSE)

  renv_dcf_write(after, file = descpath)

}

renv_package_augment_metadata <- function(path, remotes) {

  metapath <- file.path(path, "Meta/package.rds")
  if (!file.exists(metapath))
    return(FALSE)

  meta <- readRDS(metapath)
  before <- as.list(meta$DESCRIPTION)
  after <- renv_package_augment_impl(before, remotes)
  if (identical(before, after))
    return(FALSE)

  meta$DESCRIPTION <- map_chr(after, identity)
  saveRDS(meta, file = metapath, version = 2L)

}

# find recursive dependencies of a package. note that this routine
# doesn't farm out to CRAN; it relies on the package and its dependencies
# all being installed locally. returns a named vector mapping package names
# to the path where they were discovered, or NA if those packages are not
# installed
renv_package_dependencies <- function(packages,
                                      libpaths = NULL,
                                      fields = NULL,
                                      callback = NULL,
                                      project = NULL)
{
  visited <- new.env(parent = emptyenv())
  ignored <- renv_project_ignored_packages(project = project)
  packages <- renv_vector_diff(packages, ignored)
  libpaths <- libpaths %||% renv_libpaths_all()
  fields <- fields %||% settings$package.dependency.fields(project = project)
  callback <- callback %||% function(package, location, project) location
  project <- renv_project_resolve(project)

  for (package in packages)
    renv_package_dependencies_impl(package, visited, libpaths, fields, callback, project)

  as.list(visited)
}

renv_package_dependencies_impl <- function(package,
                                           visited,
                                           libpaths,
                                           fields = NULL,
                                           callback = NULL,
                                           project = NULL)
{
  # skip the 'R' package
  if (package == "R")
    return()

  # if we've already visited this package, bail
  if (!is.null(visited[[package]]))
    return()

  # default to unknown path for visited packages
  visited[[package]] <- ""

  # find the package -- note that we perform a permissive lookup here
  # because we want to capture potentially invalid / broken package installs
  # (that is, the 'package' we find might be an incomplete or broken package
  # installation at this point)
  location <- find(libpaths, function(libpath) {
    candidate <- file.path(libpath, package)
    if (renv_file_exists(candidate))
      return(candidate)
  })

  if (is.null(location))
    return(callback(package, "", project))

  # we know the path, so set it now
  visited[[package]] <- callback(package, location, project)

  # find its dependencies from the DESCRIPTION file
  deps <- renv_dependencies_discover_description(location, fields = "strong")
  subpackages <- deps$Package
  for (subpackage in subpackages)
    renv_package_dependencies_impl(subpackage, visited, libpaths, fields, callback, project)
}

renv_package_reload <- function(package, library = NULL) {
  status <- catch(renv_package_reload_impl(package, library))
  !inherits(status, "error") && status
}

renv_package_reload_impl <- function(package, library) {

  if (renv_tests_running())
    return(FALSE)

  # record if package is attached (and, if so, where)
  name <- paste("package", package, sep = ":")
  pos <- match(name, search())

  # unload the package
  if (!is.na(pos))
    renv_package_reload_impl_searchpath(package, library, pos)
  else
    renv_package_reload_impl_namespace(package, library)

  TRUE

}

renv_package_reload_impl_searchpath <- function(package, library, pos) {

  args <- list(pos = pos, unload = TRUE, force = TRUE)
  quietly(do.call(base::detach, args), sink = FALSE)

  args <- list(package = package, pos = pos, lib.loc = library, quietly = TRUE)
  quietly(do.call(base::library, args), sink = FALSE)

}

renv_package_reload_impl_namespace <- function(package, library) {
  unloadNamespace(package)
  loadNamespace(package, lib.loc = library)
}

renv_package_hook <- function(package, hook) {
  if (package %in% loadedNamespaces())
    hook()
  else
    setHook(packageEvent(package, "onLoad"), hook)
}

renv_package_metadata <- function(package) {
  pkgpath <- renv_package_find(package)
  metapath <- file.path(pkgpath, "Meta/package.rds")
  readRDS(metapath)
}

renv_package_shlib <- function(package) {

  pkgpath <- renv_package_find(package)

  pkgname <- basename(package)
  if (pkgname == "data.table")
    pkgname <- "datatable"

  libname <- paste0(pkgname, .Platform$dynlib.ext)
  file.path(pkgpath, "libs", libname)

}

renv_package_built <- function(path) {

  info <- renv_file_info(path)

  # list files in package
  isarchive <- identical(info$isdir, FALSE)
  files <- if (isarchive)
    renv_archive_list(path)
  else
    list.files(path, full.names = TRUE, recursive = TRUE)

  # for a source package, the canonical way to determine if it has already
  # been built is the presence of a 'Packaged:' field in the DESCRIPTION file
  # ('Built:' for binary packages) but we want to avoid the overhead of
  # unpacking the package if at all possible
  pattern <- "/(?:MD5$|INDEX/|Meta/package\\.rds$)"
  matches <- grep(pattern, files)
  if (length(matches) != 0L)
    return(TRUE)

  # if the above failed, then we'll use the contents of the DESCRIPTION file
  descpaths <- grep("/DESCRIPTION$", files, value = TRUE)
  if (length(descpaths) == 0L)
    return(FALSE)

  n <- nchar(descpaths)
  descpath <- descpaths[n == min(n)]
  contents <- if (isarchive)
    renv_archive_read(path, descpath)
  else
    readLines(descpath, warn = FALSE)

  # check for signs it was built
  pattern <- "^(?:Packaged|Built):"
  matches <- grep(pattern, contents)
  if (length(matches) != 0L)
    return(TRUE)

  # does not appear to be a source package
  FALSE

}

renv_package_checking <- function() {
  is_testing() ||
    "CheckExEnv" %in% search() ||
    renv_envvar_exists("_R_CHECK_PACKAGE_NAME_") ||
    renv_envvar_exists("_R_CHECK_SIZE_OF_TARBALL_")
}

renv_package_unpack <- function(package, path, subdir = "", force = FALSE) {

  # if this isn't an archive, nothing to do
  info <- renv_file_info(path)
  if (identical(info$isdir, TRUE))
    return(path)

  # find DESCRIPTION files in the archive
  descpaths <- renv_archive_find(path, "(?:^|/)DESCRIPTION$")

  # check for a top-level DESCRIPTION file
  # this is done in case the archive has been already been re-packed, so that a
  # package originally located within a sub-directory is now at the top level
  if (!force) {
    descpath <- grep("^[^/]+/DESCRIPTION$", descpaths, perl = TRUE, value = TRUE)
    if (length(descpath))
      return(path)
  }

  # try to resolve the path to the DESCRIPTION file in the archive
  descpath <- if (nzchar(subdir)) {
    pattern <- sprintf("(?:^|/)\\Q%s\\E/DESCRIPTION$", subdir)
    grep(pattern, descpaths, perl = TRUE, value = TRUE)
  } else {
    n <- nchar(descpaths)
    descpaths[n == min(n)]
  }

  # if this failed, error
  if (length(descpath) != 1L) {
    fmt <- "internal error: couldn't find DESCRIPTION file for package '%s' in archive '%s'"
    stopf(fmt, package, path)
  }

  # create extraction directory
  old <- renv_scope_tempfile("renv-package-old-")
  new <- renv_scope_tempfile("renv-package-new-", scope = parent.frame())
  ensure_directory(c(old, new))

  # decompress archive to dir
  renv_archive_decompress(path, exdir = old)

  # rename (without sub-directory)
  oldpath <- file.path(old, dirname(descpath))
  newpath <- file.path(new, package)
  file.rename(oldpath, newpath)

  # use newpath
  newpath

}


# packages.R -----------------------------------------------------------------


the$packages_base <- NULL
the$packages_recommended <- NULL

renv_packages_base <- function() {

  the$packages_base <- the$packages_base %||% {
    db <- installed_packages(lib.loc = .Library, priority = "base")
    c("R", db$Package, "translations")
  }

}

renv_packages_recommended <- function() {

  the$packages_recommended <- the$packages_recommended %||% {
    db <- installed_packages(lib.loc = .Library, priority = "recommended")
    db$Package
  }

}


# pak.R ----------------------------------------------------------------------


# the minimum-required version of 'pak' for renv integration
the$pak_minver <- numeric_version("0.5.1")

renv_pak_init <- function(stream = NULL, force = FALSE) {

  stream <- stream %||% renv_pak_stream()
  if (force || !renv_pak_available())
    renv_pak_init_impl(stream)

  renv_namespace_load("pak")

}

renv_pak_stream <- function() {

  # check if stable is new enough
  streams <- c("stable", "rc", "devel")
  for (stream in streams) {
    repos <- renv_pak_repos(stream)
    latest <- renv_available_packages_latest("pak", repos = repos)
    version <- numeric_version(latest$Version)
    if (version >= the$pak_minver)
      return(stream)
  }

  fmt <- "internal error: pak (>= %s) is not available"
  stopf(fmt, format(the$pak_minver))

}

renv_pak_available <- function() {
  tryCatch(
    packageVersion("pak") >= the$pak_minver,
    error = function(e) FALSE
  )
}

renv_pak_repos <- function(stream) {

  # on macOS, we can only use pak binaries with CRAN R
  if (renv_platform_macos() && .Platform$pkgType == "source")
    return(getOption("repos"))

  # otherwise, use pre-built pak binaries
  fmt <- "https://r-lib.github.io/p/pak/%s/%s/%s/%s"
  sprintf(fmt, stream, .Platform$pkgType, version$os, version$arch)

}

renv_pak_init_impl <- function(stream) {

  repos <- c("r-lib" = renv_pak_repos(stream))
  renv_scope_options(renv.config.pak.enabled = FALSE, repos = repos)

  library <- renv_libpaths_active()
  install("pak", library = library)
  loadNamespace("pak", lib.loc = library)

}

renv_pak_install <- function(packages, library, project) {

  pak <- renv_namespace_load("pak")
  lib <- library[[1L]]

  # transform repositories
  if (renv_ppm_enabled()) {
    repos <- getOption("repos")
    renv_scope_options(repos = renv_ppm_transform(repos))
  }

  # make sure pak::pkg_install() still works even if we're
  # running in renv with devtools::load_all()
  name <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA)
  if (identical(name, "renv"))
    renv_scope_envvars("_R_CHECK_PACKAGE_NAME_" = NULL)

  # if we received a named list of remotes, use the names
  packages <- if (any(nzchar(names(packages))))
     names(packages)
  else
    as.character(packages)

  if (length(packages) == 0L)
    return(pak$local_install_dev_deps(root = project, lib = lib))

  pak$pkg_install(
    pkg     = packages,
    lib     = lib,
    upgrade = TRUE
  )

}

renv_pak_restore <- function(lockfile,
                             packages = NULL,
                             exclude = NULL,
                             project = NULL)
{
  pak <- renv_namespace_load("pak")

  # make sure pak::pkg_install() still works even if we're
  # running in renv with devtools::load_all()
  name <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA)
  if (identical(name, "renv"))
    renv_scope_envvars("_R_CHECK_PACKAGE_NAME_" = NULL)

  # get records to install
  records <- renv_lockfile_records(lockfile)
  packages <- setdiff(packages %||% names(records), c(exclude, "pak", "renv"))
  records <- records[packages]

  # attempt to link packages that have cache entries
  if (renv_cache_config_enabled(project = project)) {
    linked <- map_lgl(records, renv_cache_synchronize)
    records <- records[!linked]
  }

  # convert into specs compatible with pak, and install
  remotes <- map_chr(records, renv_record_format_remote)

  # TODO: We previously tried converting version-ed remotes into "plain" remotes
  # if the package version happened to be current, but then 'pak' would choose
  # not to install the package if a newer version was available. Hence, we need
  # to preserve the exact remote we wish to install here.

  # perform installation
  pak$pkg_install(remotes)
}



# parallel.R -----------------------------------------------------------------


renv_parallel_cores <- function() {

  if (renv_platform_windows())
    return(1L)

  value <- config$updates.parallel()
  case(
    identical(value, TRUE)  ~ getOption("mc.cores", default = 2L),
    identical(value, FALSE) ~ 1L,
    ~ as.integer(value)
  )

}

renv_parallel_exec <- function(data, callback) {
  cores <- renv_parallel_cores()
  if (cores > 1)
    parallel::mclapply(data, callback, mc.cores = cores)
  else
    lapply(data, callback)
}


# parse.R --------------------------------------------------------------------


renv_parse_file <- function(file = "", ...) {
  if (nzchar(file)) {
    renv_scope_options(warn = -1L)
    text <- readLines(file, warn = FALSE, encoding = "UTF-8")
    renv_parse_impl(text, srcfile = file, ...)
  }
}

renv_parse_text <- function(text = NULL, ...) {
  if (is.character(text)) {
    renv_parse_impl(text, ...)
  }
}

renv_parse_impl <- function(text, ...) {

  # save default encoding
  enc <- Encoding(text)

  # disable warnings + encoding conversions
  renv_scope_options(
    warn     = 1L,
    encoding = "native.enc"
  )

  # attempt multiple parse methods
  methods <- list(
    renv_parse_impl_asis,
    renv_parse_impl_native,
    renv_parse_impl_utf8
  )

  # attempt with different guessed encodings
  encodings <- c("UTF-8", "unknown")

  for (encoding in encodings) {
    Encoding(text) <- encoding
    for (method in methods) {
      parsed <- catch(method(text, ...))
      if (!inherits(parsed, "error"))
        return(parsed)
    }
  }

  # if these all fail, then just try the default
  # parse and let the error propagate
  defer(Sys.setlocale())
  Encoding(text) <- enc
  parse(text = text, ...)

}

renv_parse_impl_asis <- function(text, ...) {
  defer(Sys.setlocale())
  parse(text = text, ...)
}

renv_parse_impl_native <- function(text, ...) {
  defer(Sys.setlocale())
  parse(text = enc2native(text), encoding = "unknown", ...)
}

renv_parse_impl_utf8 <- function(text, ...) {
  defer(Sys.setlocale())
  parse(text = enc2utf8(text), encoding = "UTF-8", ...)
}



# patch.R --------------------------------------------------------------------


renv_patch_init <- function() {
  renv_patch_rprofile()
  renv_patch_tar()
  renv_patch_repos()
  renv_patch_golem()
  renv_patch_methods_table()
}

renv_patch_rprofile <- function() {

  # resolve path to user profile
  path <- Sys.getenv("R_PROFILE_USER", unset = "~/.Rprofile")
  info <- renv_file_info(path)
  if (!identical(info$isdir, FALSE))
    return(FALSE)

  # if the .Rprofile is empty, do nothing
  if (info$size == 0)
    return(TRUE)

  # check for trailing newline
  data <- readBin(path, raw(), n = info$size)
  if (empty(data))
    return(TRUE)

  last <- data[length(data)]
  endings <- as.raw(c(0x0a, 0x0d))
  if (last %in% endings)
    return(TRUE)

  # if it's missing, inform the user
  warningf("%s is missing a trailing newline", renv_path_pretty(path))
  FALSE

}

renv_patch_tar <- function() {

  # read value of TAR
  tar <- Sys.getenv("TAR", unset = "")

  # on Windows, if TAR is unset, then force the usage
  # of R's internal tar implementation. this is done to
  # avoid issues where e.g. versions of tar which do not
  # understand Windows paths are on the PATH
  #
  # https://github.com/rstudio/renv/issues/521
  if (renv_platform_windows() && !nzchar(tar)) {
    Sys.setenv(TAR = "internal")
    return(TRUE)
  }

  # otherwise, allow empty / internal tars
  if (tar %in% c("", "internal"))
    return(TRUE)

  # the user (or R itself) has set the TAR environment variable
  # validate that it exists (resolve from PATH)
  #
  # note that the user can set TAR to be a full command; e.g.
  #
  #    TAR = /path/to/tar --force-local
  #
  # so we need to handle that case appropriately
  whitespace <- gregexpr("(?:\\s+|$)", tar, perl = TRUE)[[1L]]
  for (index in whitespace) {
    candidate <- substring(tar, 1L, index - 1L)
    resolved <- Sys.which(candidate)
    if (nzchar(resolved))
      return(TRUE)
  }

  # TAR appears to be set but invalid; override it
  # and warn the user
  newtar <- Sys.which("tar")
  if (!nzchar(newtar))
    newtar <- "internal"

  Sys.setenv(TAR = newtar)

  # report to the user
  fmt <- "requested TAR '%s' does not exist; using '%s' instead"
  warningf(fmt, tar, newtar)

}

renv_patch_golem <- function() {
  renv_package_hook("golem", renv_patch_golem_impl)
}

renv_patch_golem_impl <- function(...) {

  if (packageVersion("golem") != "0.2.1")
    return()

  golem <- getNamespace("golem")

  replacement <- function(file, pattern, replace) {

    # skip .rds files
    if (grepl("[.]rds$", file))
      return()

    # skip files containing nul bytes
    info <- renv_file_info(file)
    bytes <- readBin(file, "raw", info$size)
    if (any(bytes == 0L))
      return()

    # otherwise, attempt replacement
    old <- readLines(file)
    new <- gsub(pattern, replace, old)
    writeLines(new, con = file)

  }

  environment(replacement) <- golem

  if ("compiler" %in% loadedNamespaces())
    replacement <- compiler::cmpfun(replacement)

  renv_binding_replace(golem, "replace_word", replacement)

}

renv_patch_methods_table <- function() {
  catchall(renv_patch_methods_table_impl())
}

renv_patch_methods_table_impl <- function() {

  # ensure promises in S3 methods table are forced
  # https://bugs.r-project.org/bugzilla/show_bug.cgi?id=16644
  for (envir in list(.BaseNamespaceEnv, renv_namespace_load("utils"))) {

    # unlock binding if it's locked
    binding <- ".__S3MethodsTable__."
    base <- baseenv()
    if (base$bindingIsLocked(binding, env = envir)) {
      base$unlockBinding(binding, env = envir)
      defer(base$lockBinding(binding, envir))
    }

    # force everything defined in the environment
    table <- envir[[binding]]
    for (key in ls(envir = table, all.names = TRUE))
      table[[key]] <- force(table[[key]])

  }

}

# puts the current version of renv into an on-disk package repository,
# so that packages using renv can find this version of renv in tests
# this helps renv survive CRAN revdep checks (e.g. jetpack)
renv_patch_repos <- function() {

  # nothing to do in embedded mode
  if (renv_metadata_embedded())
    return()

  # nothing to do if we're not running tests
  checking <- renv_package_checking()
  if (!checking)
    return()

  # nothing to do if we're running our own tests
  name <- Sys.getenv("_R_CHECK_PACKAGE_NAME_", unset = NA)
  if (identical(name, "renv"))
    return()

  # presumably this will never happen when the dev version of renv is
  # installed, so we skip to avoid parsing a sha as version
  sha <- attr(the$metadata$version, "sha")
  if (!is.null(sha))
    return()

  # nothing to do if this version of 'renv' is already available
  version <- renv_metadata_version()
  entry <- catch(renv_available_packages_entry("renv", filter = version, quiet = TRUE))
  if (!inherits(entry, "error"))
    return()

  # check if we've already set repos
  if ("RENV" %in% names(getOption("repos")))
    return()

  # use package-local repository path
  repopath <- system.file("repos", package = "renv", mustWork = FALSE)
  if (!file.exists(repopath))
    return()

  # update our repos option
  fmt <- if (renv_platform_windows()) "file:///%s" else "file://%s"
  repourl <- sprintf(fmt, repopath)

  # renv needs to be first so the right version is found?
  repos <- c(RENV = repourl, getOption("repos"))
  names(repos) <- make.names(names(repos))
  options(repos = repos)

  # make sure these repositories are used in restore too
  options(renv.config.repos.override = repos)

}


# path.R ---------------------------------------------------------------------


the$alpha <- c(letters, LETTERS)

renv_path_absolute <- function(path) {

  substr(path, 1L, 1L) %in% c("~", "/", "\\") || (
    substr(path, 1L, 1L) %in% the$alpha &&
    substr(path, 2L, 3L) %in% c(":/", ":\\")
  )

}

renv_path_aliased <- function(path) {

  home <- Sys.getenv("HOME", unset = Sys.getenv("R_USER"))
  if (!nzchar(home))
    return(path)

  home <- gsub("\\", "/", home, fixed = TRUE)
  path <- gsub("\\", "/", path, fixed = TRUE)

  match <- regexpr(home, path, fixed = TRUE, useBytes = TRUE)
  path[match == 1L] <- file.path("~", substring(path[match == 1L], nchar(home) + 2L))

  path

}

renv_path_within <- function(path, parent) {
  path <- renv_path_canonicalize(path)
  prefix <- paste(renv_path_canonicalize(parent), "/", sep = "")
  path == parent | substring(path, 1L, nchar(prefix)) == prefix
}

renv_path_normalize <- function(path, winslash = "/", mustWork = FALSE) {
  if (renv_platform_unix())
    renv_path_normalize_unix(path, winslash, mustWork)
  else
    renv_path_normalize_win32(path, winslash, mustWork)
}

renv_path_normalize_unix <- function(path,
                                     winslash = "/",
                                     mustWork = FALSE)
{
  # force paths to be absolute
  bad <- !map_lgl(path, renv_path_absolute)
  if (any(bad)) {
    prefix <- normalizePath(".", winslash = winslash)
    path[bad] <- paste(prefix, path[bad], sep = winslash)
  }

  # normalize the expanded paths
  normalizePath(path, winslash, mustWork)
}

# NOTE: in versions of R < 4.0.0, normalizePath() does not normalize path
# casing; e.g. normalizePath("~/MyPaTh") will not normalize to "~/MyPath"
# (assuming that is the "true" underlying casing on the filesystem)
#
# we work around this by round-tripping between the short name and
# the long name, as Windows then has no choice but to figure out
# the correct casing for us
#
# this isn't 100% reliable (not all paths have a short-path equivalent)
# but seems to be good enough in practice ...
#
# except that, if the path contains characters that cannot be represented in the
# current encoding, then attempting to normalize the short version of that path
# will fail -- so if the path is already UTF-8, then we need to avoid
# round-tripping through the short path.
#
# furthermore, it appears that shortPathName() can mis-encode its result for
# strings marked with latin1 encoding?
#
# https://github.com/rstudio/renv/issues/629
renv_path_normalize_win32 <- function(path,
                                      winslash = "/",
                                      mustWork = FALSE)
{

  # see the NOTE above, this workaround is only necessary for R < 4.0.0,
  # and it complicates things unnecessarily
  if (getRversion() >= "4.0.0")
    return(renv_path_normalize_unix(path, winslash, mustWork))

  # get encoding for this set of paths
  enc <- Encoding(path)

  # perform separate operations for each
  utf8    <- enc == "UTF-8"
  latin1  <- enc == "latin1"
  unknown <- enc == "unknown"

  # normalize based on their encoding
  path[utf8]    <- normalizePath(path[utf8], winslash, mustWork)
  path[latin1]  <- normalizePath(path[latin1], winslash, mustWork)
  path[unknown] <- renv_path_normalize_win32_impl(path[unknown], winslash, mustWork)

  # return resulting path
  path
}

renv_path_normalize_win32_impl <- function(path,
                                           winslash = "/",
                                           mustWork = FALSE)
{
  # get short path
  expanded <- path.expand(path)
  short <- utils::shortPathName(expanded)

  # if a UTF-8 string is passed to utils::shortPathName(), it seems that
  # the string might be latin1-encoded, even though it's marked as UTF-8?
  if (!identical(R.version$crt, "ucrt")) {
    utf8 <- Encoding(short) == "UTF-8"
    Encoding(short[utf8]) <- "latin1"
  }

  # normalize
  normalizePath(short, winslash, mustWork)
}

# TODO: this is a lie; for existing paths symlinks will be resolved.
# don't use this for paths that need to be uniquely resolved!
renv_path_canonicalize <- function(path) {
  parent <- dirname(path)
  root <- renv_path_normalize(parent)
  trimmed <- sub("/+$", "", root)
  file.path(trimmed, basename(path))
}

renv_path_same <- function(lhs, rhs) {
  renv_path_canonicalize(lhs) == renv_path_canonicalize(rhs)
}

# get the nth path component from the end of the path
renv_path_component <- function(path, index = 1) {
  splat <- strsplit(path, "[/\\]+")
  map_chr(splat, function(parts) parts[length(parts) - index + 1])
}

renv_path_pretty <- function(path) {
  renv_json_quote(renv_path_aliased(path))
}

renv_path_relative <- function(path, root) {
  within <- startswith(path, root)
  path[within] <- substring(path[within], nchar(root) + 2L)
  path
}



# paths.R --------------------------------------------------------------------


the$root <- NULL

renv_paths_override <- function(name) {

  # # check for value from option
  # optname <- paste("renv.paths", name, sep = ".")
  # optval <- getOption(optname)
  # if (!is.null(optval))
  #   return(optval)

  # check for value from envvar
  envname <- paste("RENV_PATHS", toupper(name), sep = "_")
  envval  <- Sys.getenv(envname, unset = NA)
  if (!is.na(envval))
    return(envval)

}

renv_paths_common <- function(name, prefixes = NULL, ...) {

  # check for single absolute path supplied by user
  # TODO: handle multiple?
  end <- file.path(...)
  if (length(end) == 1 && renv_path_absolute(end))
    return(end)

  # check for path provided via option
  root <- renv_paths_override(name) %||% renv_paths_root(name)

  # split path entries containing a separator
  if (name %in% c("cache", "local", "cellar")) {
    pattern <- if (renv_platform_windows()) "[;]" else "[;:]"
    root <- strsplit(root, pattern)[[1L]]
  }

  # form rest of path
  prefixed <- if (length(prefixes))
    file.path(root, paste(prefixes, collapse = "/"))
  else
    root

  path <- file.path(prefixed, ...)
  if (length(path)) path else ""
}

renv_paths_library_root <- function(project) {
  renv_bootstrap_library_root(project)
}

renv_paths_library <- function(..., project = NULL) {
  project <- renv_project_resolve(project)
  root <- renv_paths_library_root(project)
  file.path(root, renv_platform_prefix(), ...) %||% ""
}

renv_paths_lockfile <- function(project = NULL) {

  # allow override
  # TODO: profiles?
  override <- Sys.getenv("RENV_PATHS_LOCKFILE", unset = NA)
  if (!is.na(override)) {
    last <- substr(override, nchar(override), nchar(override))
    if (last %in% c("/", "\\"))
      override <- paste0(override, "renv.lock")
    return(override)
  }

  # otherwise, use default location (location location relative to renv folder)
  project <- renv_project_resolve(project)
  renv <- renv_paths_renv(project = project)
  file.path(dirname(renv), "renv.lock")

}

renv_paths_settings <- function(project = NULL) {
  renv_paths_renv("settings.json", project = project)
}

renv_paths_activate <- function(project = NULL) {
  renv_paths_renv("activate.R", profile = FALSE, project = project)
}

renv_paths_sandbox <- function(project = NULL) {
  if (renv_platform_unix())
    renv_paths_sandbox_unix(project)
  else
    renv_paths_sandbox_win32(project)
}

renv_paths_sandbox_unix <- function(project = NULL) {

  # construct a platform prefix
  hash <- substring(renv_hash_text(R()), 1L, 8L)
  prefix <- paste(renv_platform_prefix(), hash, sep = "/")

  # check for override
  root <- Sys.getenv("RENV_PATHS_SANDBOX", unset = NA)
  if (!is.na(root))
    return(paste(root, prefix, sep = "/"))

  # otherwise, build path in user data directory
  userdir <- renv_bootstrap_user_dir()
  paste(userdir, "sandbox", prefix, sep = "/")

}

renv_paths_sandbox_win32 <- function(project = NULL) {

  # NOTE: We previously used the R temporary directory here, but
  # a number of users reported issues with the base R packages being
  # deleted by over-aggressive temporary directory cleaners.
  #
  # https://github.com/rstudio/renv/issues/835

  # construct a platform prefix
  hash <- substring(renv_hash_text(R()), 1L, 8L)
  prefix <- paste(renv_platform_prefix(), hash, sep = "/")

  # check for override
  root <- Sys.getenv("RENV_PATHS_SANDBOX", unset = NA)
  if (!is.na(root))
    return(paste(root, prefix, sep = "/"))

  # otherwise, build path in user data directory
  userdir <- renv_bootstrap_user_dir()
  paste(userdir, "sandbox", prefix, sep = "/")

}

renv_paths_renv <- function(..., profile = TRUE, project = NULL) {
  renv_bootstrap_paths_renv(..., profile = profile, project = project)
}

renv_paths_cellar <- function(...) {
  renv_paths_common("cellar", c(), ...)
}

renv_paths_local <- function(...) {
  renv_paths_common("local", c(), ...)
}

renv_paths_source <- function(...) {
  renv_paths_common("source", c(), ...)
}

renv_paths_binary <- function(...) {
  renv_paths_common("binary", c(renv_platform_prefix()), ...)
}

renv_paths_cache <- function(..., version = NULL) {
  platform <- renv_platform_prefix()
  version <- version %||% renv_cache_version()
  renv_paths_common("cache", c(version, platform), ...)
}

renv_paths_rtools <- function() {

  root <- renv_paths_override("rtools")
  if (is.null(root)) {
    spec <- renv_rtools_find()
    root <- spec$root
  }

  root %||% ""
}

renv_paths_extsoft <- function(...) {
  renv_paths_common("extsoft", c(), ...)
}

renv_paths_mran <- function(...) {
  renv_paths_common("mran", c(), ...)
}

renv_paths_index <- function(...) {
  renv_paths_common("index", c(renv_platform_prefix()), ...)
}


renv_paths_root <- function(...) {
  root <- renv_paths_override("root") %||% renv_paths_root_default()
  file.path(root, ...) %||% ""
}

# nocov start
renv_paths_root_default <- function() {

  (the$root <- the$root %||% {

    # use tempdir for cache when running tests
    # this check is necessary here to support packages which might use renv
    # during testing (and we don't want those to try to use the user dir)
    checking <- renv_package_checking()

    # compute the root directory
    if (checking)
      renv_paths_root_default_tempdir()
    else
      renv_paths_root_default_impl()

  })

}

renv_paths_root_default_impl <- function() {

  # compute known root directories
  roots <- c(
    renv_paths_root_default_impl_v2(),
    renv_paths_root_default_impl_v1()
  )

  # iterate through those roots, finding the first existing
  for (root in roots)
    if (file.exists(root))
      return(root)

  # if none exist, choose the most recent definition
  roots[[1L]]

}

renv_paths_root_default_impl_v2 <- function() {

  # try using tools to get the user directory
  tools <- renv_namespace_load("tools")
  if (is.function(tools$R_user_dir))
    return(tools$R_user_dir("renv", "cache"))

  renv_paths_root_default_impl_v2_fallback()

}

renv_paths_root_default_impl_v2_fallback <- function() {

  # try using our own backfill for older versions of R
  envvars <- c("R_USER_CACHE_DIR", "XDG_CACHE_HOME")
  for (envvar in envvars) {
    root <- Sys.getenv(envvar, unset = NA)
    if (!is.na(root)) {
      path <- file.path(root, "R/renv")
      return(path)
    }
  }

  # use platform-specific default fallbacks
  if (renv_platform_windows())
    file.path(Sys.getenv("LOCALAPPDATA"), "R/cache/R/renv")
  else if (renv_platform_macos())
    "~/Library/Caches/org.R-project.R/R/renv"
  else
    "~/.cache/R/renv"

}

renv_paths_root_default_impl_v1 <- function() {

  base <- switch(
    Sys.info()[["sysname"]],
    Darwin  = Sys.getenv("XDG_DATA_HOME", "~/Library/Application Support"),
    Windows = Sys.getenv("LOCALAPPDATA", Sys.getenv("APPDATA")),
    Sys.getenv("XDG_DATA_HOME", "~/.local/share")
  )

  file.path(base, "renv")

}

renv_paths_root_default_tempdir <- function() {
  temp <- file.path(tempdir(), "renv")
  ensure_directory(temp)
  return(temp)
}

# nocov end

#' Path for storing global state
#'
#' @description
#' By default, renv stores global state in the following OS-specific folders:
#'
#' \tabular{ll}{
#' **Platform** \tab **Location** \cr
#' Linux        \tab `~/.cache/R/renv` \cr
#' macOS        \tab `~/Library/Caches/org.R-project.R/R/renv` \cr
#' Windows      \tab `%LOCALAPPDATA%/R/cache/R/renv` \cr
#' }
#'
#' If desired, this path can be customized by setting the `RENV_PATHS_ROOT`
#' environment variable. This can be useful if you'd like, for example, multiple
#' users to be able to share a single global cache.
#'
#' # Customising individual paths
#'
#' The various state sub-directories can also be individually adjusted, if so
#' desired (e.g. you'd prefer to keep the cache of package installations on a
#' separate volume). The various environment variables that can be set are
#' enumerated below:
#'
#' \tabular{ll}{
#' \strong{Environment Variable}     \tab \strong{Description} \cr
#' \code{RENV_PATHS_ROOT}            \tab The root path used for global state storage. \cr
#' \code{RENV_PATHS_LIBRARY}         \tab The path to the project library. \cr
#' \code{RENV_PATHS_LIBRARY_ROOT}    \tab The parent path for project libraries. \cr
#' \code{RENV_PATHS_LIBRARY_STAGING} \tab The parent path used for staged package installs. \cr
#' \code{RENV_PATHS_SANDBOX}         \tab The path to the sandboxed \R system library. \cr
#' \code{RENV_PATHS_LOCKFILE}        \tab The path to the [lockfile]. \cr
#' \code{RENV_PATHS_CELLAR}          \tab The path to the cellar, containing local package binaries and sources. \cr
#' \code{RENV_PATHS_SOURCE}          \tab The path containing downloaded package sources. \cr
#' \code{RENV_PATHS_BINARY}          \tab The path containing downloaded package binaries. \cr
#' \code{RENV_PATHS_CACHE}           \tab The path containing cached package installations. \cr
#' \code{RENV_PATHS_PREFIX}          \tab An optional prefix to prepend to the constructed library / cache paths. \cr
#' \code{RENV_PATHS_RENV}            \tab The path to the project's renv folder. For advanced users only. \cr
#' \code{RENV_PATHS_RTOOLS}          \tab (Windows only) The path to [Rtools](https://cran.r-project.org/bin/windows/Rtools/). \cr
#' \code{RENV_PATHS_EXTSOFT}         \tab (Windows only) The path containing external software needed for compilation of Windows source packages. \cr
#' \code{RENV_PATHS_MRAN}            \tab The path containing MRAN-related metadata. See `vignette("mran", package = "renv")` for more details. \cr
#' }
#'
#' (If you want these settings to persist in your project, it is recommended that
#' you add these to an appropriate \R startup file. For example, these could be
#' set in: a project-local `.Renviron`, the user-level `.Renviron`, or a
#' site-wide file at `file.path(R.home("etc"), "Renviron.site")`. See
#' [Startup] for more details).
#'
#' Note that renv will append platform-specific and version-specific entries
#' to the set paths as appropriate. For example, if you have set:
#'
#' ```
#' Sys.setenv(RENV_PATHS_CACHE = "/mnt/shared/renv/cache")
#' ```
#'
#' then the directory used for the cache will still depend on the renv cache
#' version (e.g. `v2`), the \R version (e.g. `3.5`) and the platform (e.g.
#' `x86_64-pc-linux-gnu`). For example:
#'
#' ```
#' /mnt/shared/renv/cache/v2/R-3.5/x86_64-pc-linux-gnu
#' ```
#'
#' This ensures that you can set a single `RENV_PATHS_CACHE` environment variable
#' globally without worry that it may cause collisions or errors if multiple
#' versions of \R needed to interact with the same cache.
#'
#' If reproducibility of a project is desired on a particular machine, it is
#' highly recommended that the renv cache of installed packages + binary
#' packages is backed up and persisted, so that packages can be easily restored
#' in the future -- installation of packages from source can often be arduous.
#'
#' # Sharing state across operating systems
#'
#' If you need to share the same cache with multiple different Linux operating
#' systems, you may want to set the `RENV_PATHS_PREFIX` environment variable
#' to help disambiguate the paths used on Linux. For example, setting
#' `RENV_PATHS_PREFIX = "ubuntu-bionic"` would instruct renv to construct a
#' cache path like:
#'
#' ```
#' /mnt/shared/renv/cache/v2/ubuntu-bionic/R-3.5/x86_64-pc-linux-gnu
#' ```
#'
#' If this is required, it's strongly recommended that this environment
#' variable is set in your \R installation's `Renviron.site` file, typically
#' located at `file.path(R.home("etc"), "Renviron.site")`, so that it can be
#' active for any \R sessions launched on that machine.
#'
#' Starting from `renv 0.13.0`, you can also instruct renv to auto-generate
#' an OS-specific component to include as part of library and cache paths,
#' by setting the environment variable:
#'
#' ```
#' RENV_PATHS_PREFIX_AUTO = TRUE
#' ```
#'
#' The prefix will be constructed based on fields within the system's
#' `/etc/os-release` file.
#'
#' # Package cellar
#'
#' If your project depends on one or \R packages that are not available in any
#' remote location, you can still provide a locally-available tarball for renv
#' to use during restore. By default, these packages should be made available in
#' the folder as specified by the `RENV_PATHS_CELLAR` environment variable. The
#' package sources should be placed in a file at one of these locations:
#'
#' - `${RENV_PATHS_CELLAR}/<package>_<version>.<ext>`
#' - `${RENV_PATHS_CELLAR}/<package>/<package>_<version>.<ext>`
#' - `<project>/renv/cellar/<package>_<version>.<ext>`
#' - `<project>/renv/cellar/<package>/<package>_<version>.<ext>`
#'
#' where `.<ext>` is `.tar.gz` for source packages, or `.tgz` for binaries on
#' macOS and `.zip` for binaries on Windows. During `restore()`, renv will
#' search the cellar for a compatible package, and prefer installation with
#' that copy of the package if appropriate.
#'
#' # Older versions
#'
#' Older version of renv used a different default cache location.
#' Those cache locations are:
#'
#' \tabular{ll}{
#' **Platform** \tab **Location** \cr
#' Linux        \tab `~/.local/share/renv` \cr
#' macOS        \tab `~/Library/Application Support/renv` \cr
#' Windows      \tab `%LOCALAPPDATA%/renv` \cr
#' }
#'
#' If an renv root directory has already been created in one of the old
#' locations, that will still be used. This change was made to comply with the
#' CRAN policy requirements of \R packages.
#'
#' @rdname paths
#' @name paths
#'
#' @format NULL
#'
#' @export
#'
#' @examples
#' # get the path to the project library
#' path <- renv::paths$library()
paths <- list(
  root     = renv_paths_root,
  library  = renv_paths_library,
  lockfile = renv_paths_lockfile,
  settings = renv_paths_settings,
  cache    = renv_paths_cache,
  sandbox  = renv_paths_sandbox
)


# pip.R ----------------------------------------------------------------------


pip_freeze <- function(..., python = NULL) {
  python <- python %||% renv_python_active()
  renv_scope_envvars(PIP_DISABLE_PIP_VERSION_CHECK = "1")
  python <- renv_path_canonicalize(python)
  args <- c("-m", "pip", "freeze")
  action <- "invoking pip freeze"
  renv_system_exec(python, args, action, ...)
}

pip_install <- function(modules, ..., python = NULL) {
  python <- python %||% renv_python_active()
  renv_scope_envvars(PIP_DISABLE_PIP_VERSION_CHECK = "1")
  python <- renv_path_canonicalize(python)
  args <- c("-m", "pip", "install", "--upgrade", modules)
  action <- paste("installing", paste(shQuote(modules), collapse = ", "))
  renv_system_exec(python, args, action, ...)
}

pip_install_requirements <- function(requirements, ..., python = NULL) {

  python <- python %||% renv_python_active()

  file <- renv_scope_tempfile("renv-requirements-", fileext = ".txt")
  writeLines(requirements, con = file)

  renv_scope_envvars(PIP_DISABLE_PIP_VERSION_CHECK = "1")
  python <- renv_path_canonicalize(python)
  args <- c("-m", "pip", "install", "--upgrade", "-r", renv_shell_path(file))
  action <- "restoring Python packages"
  renv_system_exec(python, args, action, ...)

}

pip_uninstall <- function(modules, ..., python = NULL) {

  python <- python %||% renv_python_active()

  renv_scope_envvars(PIP_DISABLE_PIP_VERSION_CHECK = "1")
  python <- renv_path_canonicalize(python)
  args <- c("-m", "pip", "uninstall", "--yes", modules)
  action <- paste("uninstalling", paste(shQuote(modules), collapse = ", "))
  renv_system_exec(python, args, action, ...)

  TRUE

}


# platform.R -----------------------------------------------------------------


the$sysinfo <- NULL

renv_platform_init <- function() {
  the$sysinfo <- Sys.info()
}

renv_platform_unix <- function() {
  .Platform$OS.type == "unix"
}

renv_platform_windows <- function() {
  .Platform$OS.type == "windows"
}

renv_platform_macos <- function() {
  the$sysinfo[["sysname"]] == "Darwin"
}

renv_platform_linux <- function() {
  the$sysinfo[["sysname"]] == "Linux"
}

renv_platform_solaris <- function() {
  the$sysinfo[["sysname"]] == "SunOS"
}

renv_platform_wsl <- function() {

  pv <- "/proc/version"
  if (!file.exists(pv))
    return(FALSE)

  renv_scope_options(warn = -1L)
  contents <- catch(readLines(pv, warn = FALSE))
  if (inherits(contents, "error"))
    return(FALSE)

  any(grepl("(?:Microsoft|WSL)", contents, ignore.case = TRUE))

}

renv_platform_prefix <- function() {
  renv_bootstrap_platform_prefix()
}

renv_platform_os <- function() {
  renv_bootstrap_platform_os()
}

renv_platform_machine <- function() {
  the$sysinfo[["machine"]]
}


# ppm.R ----------------------------------------------------------------------


renv_ppm_normalize <- function(url) {
  sub("/__[^_]+__/[^/]+/", "/", url)
}

renv_ppm_transform <- function(repos = getOption("repos")) {
  map_chr(repos, function(url) {
    tryCatch(
      renv_ppm_transform_impl(url),
      error = function(e) url
    )
  })
}

renv_ppm_transform_impl <- function(url) {

  # if this function is being called as part of `install(..., type = "source')`
  # then we want to transform binary URLs to source URLs here
  if (identical(the$install_pkg_type, "source"))
    return(renv_ppm_normalize(url))

  # repository URL transformation is only necessary on Linux
  os <- renv_ppm_os()
  if (!identical(os, "__linux__"))
    return(url)

  # check for a known platform
  platform <- renv_ppm_platform()
  if (is.null(platform))
    return(url)

  # don't transform non-https URLs
  if (!grepl("^https?://", url))
    return(url)

  # if this already appears to be a binary URL, then avoid
  # transforming it
  if (grepl("/__[^_]+__/", url))
    return(url)

  # try to parse the repository URL
  parts <- catch(renv_url_parse(url))
  if (inherits(parts, "error"))
    return(url)

  # only attempt to transform URLs that are formatted like PPM urls:
  #
  #   https://ppm.company.org/cran/checkpoint/id
  #
  # in particular, there should be at least two trailing
  # alphanumeric path components
  pattern <- "/[^/]+/[^/]+"
  if (!grepl(pattern, parts$path))
    return(url)

  # check if this is an 'ignored' URL; that is, a repository which we
  # know is not a PPM URL
  mirrors <- catch(getCRANmirrors(local.only = TRUE))
  ignored <- c(
    getOption("renv.ppm.ignoredUrls", default = character()),
    settings$ppm.ignored.urls(),
    mirrors$URL,
    "http://cran.rstudio.com",
    "http://cran.rstudio.org",
    "https://cran.rstudio.com",
    "https://cran.rstudio.org"
  )

  if (sub("/+$", "", url) %in% sub("/+$", "", ignored))
    return(url)

  # if this is a 'known' PPM instance, then skip the query step
  known <- c(
    dirname(dirname(config$ppm.url())),
    getOption("renv.ppm.repos", default = NULL)
  )

  if (any(startswith(url, known))) {
    parts <- c(dirname(url), "__linux__", platform, basename(url))
    binurl <- paste(parts, collapse = "/")
    return(binurl)
  }

  # try to query the status endpoint
  # TODO: this could fail if the URL is a proxy back to PPM?
  base <- dirname(dirname(url))
  status <- catch(renv_ppm_status(base))
  if (inherits(status, "error"))
    return(url)

  # iterate through distros and check for a match
  for (distro in status$distros) {

    ok <-
      identical(distro$binaryURL, platform) &&
      identical(distro$binaries, TRUE)

    if (ok) {
      parts <- c(dirname(url), "__linux__", platform, basename(url))
      binurl <- paste(parts, collapse = "/")
      return(binurl)
    }

  }

  # no match; return url as-is
  url

}

renv_ppm_status <- function(base) {
  memoize(
    key   = base,
    value = catch(renv_ppm_status_impl(base))
  )
}

renv_ppm_status_impl <- function(base) {

  # use a shorter delay to avoid hanging a session
  renv_scope_options(
    renv.config.connect.timeout = 10L,
    renv.config.connect.retry   = 1L
  )

  # attempt the download
  endpoint <- file.path(base, "__api__/status")
  destfile <- renv_scope_tempfile("renv-ppm-status-", fileext = ".json")
  quietly(download(endpoint, destfile))

  # read the downloaded JSON
  renv_json_read(destfile)

}

renv_ppm_platform <- function(file = "/etc/os-release") {

  platform <- Sys.getenv("RENV_PPM_PLATFORM", unset = NA)
  if (!is.na(platform))
    return(platform)

  platform <- Sys.getenv("RENV_RSPM_PLATFORM", unset = NA)
  if (!is.na(platform))
    return(platform)

  if (renv_platform_windows())
    return("windows")

  if (renv_platform_macos())
    return("macos")

  renv_ppm_platform_impl(file)

}

renv_ppm_platform_impl <- function(file = "/etc/os-release") {

  if (file.exists(file)) {

    properties <- renv_properties_read(
      path      = file,
      delimiter = "=",
      dequote   = TRUE
    )

    id <- properties$ID %||% ""

    case(
      identical(id, "ubuntu") ~ renv_ppm_platform_ubuntu(properties),
      identical(id, "centos") ~ renv_ppm_platform_centos(properties),
      identical(id, "rhel")   ~ renv_ppm_platform_rhel(properties),
      grepl("\\bsuse\\b", id) ~ renv_ppm_platform_suse(properties)
    )

  }

}

renv_ppm_platform_ubuntu <- function(properties) {

  codename <- properties$VERSION_CODENAME
  if (is.null(codename))
    return(NULL)

  codename

}

renv_ppm_platform_centos <- function(properties) {

  id <- properties$VERSION_ID
  if (is.null(id))
    return(NULL)

  paste0("centos", substring(id, 1L, 1L))

}

renv_ppm_platform_rhel <- function(properties) {

  id <- properties$VERSION_ID
  if (is.null(id))
    return(NULL)

  paste0("centos", substring(id, 1L, 1L))

}


renv_ppm_platform_suse <- function(properties) {

  id <- properties$VERSION_ID
  if (is.null(id))
    return(NULL)

  parts <- strsplit(id, ".", fixed = TRUE)[[1L]]
  paste0("opensuse", parts[[1L]])

}

renv_ppm_os <- function() {

  os <- Sys.getenv("RENV_PPM_OS", unset = NA)
  if (!is.na(os))
    return(os)

  os <- Sys.getenv("RENV_RSPM_OS", unset = NA)
  if (!is.na(os))
    return(os)

  if (renv_platform_windows())
    "__windows__"
  else if (renv_platform_macos())
    "__macos__"
  else if (renv_platform_linux())
    "__linux__"

}


renv_ppm_enabled <- function() {

  # allow environment variable override
  enabled <- Sys.getenv("RENV_PPM_ENABLED", unset = NA)
  if (!is.na(enabled))
    return(truthy(enabled, default = TRUE))

  # support older options as well
  enabled <- Sys.getenv("RENV_RSPM_ENABLED", unset = NA)
  if (!is.na(enabled))
    return(truthy(enabled, default = TRUE))

  # TODO: can we remove this check?
  # https://github.com/rstudio/renv/issues/1132
  if (!is_testing()) {

    disabled <-
      renv_platform_linux() &&
      identical(renv_platform_machine(), "aarch64")

    if (disabled)
      return(FALSE)

  }

  # check for project setting
  enabled <- settings$ppm.enabled()
  if (!is.null(enabled))
    return(enabled)

  # otherwise, use configuration option
  config$ppm.enabled()

}


# predicate.R ----------------------------------------------------------------


pscalar <- function(x) {
  length(x) == 1L
}

pstring <- function(x) {
  is.character(x) && length(x) == 1L
}



# preflight.R ----------------------------------------------------------------


# returns TRUE if problems detected
renv_preflight <- function(lockfile) {

  problems <- stack()

  # check that we can compile C programs
  renv_preflight_compiler(problems)

  # if rJava is being used, ensure that Java is properly configured
  renv_preflight_java(lockfile, problems)

  data <- problems$data()
  if (length(data)) {

    feedback <- lines(
      "The following problems were detected in your environment:",
      "",
      paste(data, collapse = "\n\n"),
      "",
      "The environment may not be restored correctly."
    )

    caution(feedback)

  }

  length(data) == 0

}

renv_preflight_compiler <- function(problems) {

  # try to compile a simple program
  program <- "void test() {}"
  file <- renv_scope_tempfile("renv-test-compile-", fileext = ".c")
  writeLines(program, con = file)

  args <- c("CMD", "SHLIB", renv_shell_path(file))
  status <- system2(R(), args, stdout = FALSE, stderr = FALSE)

  if (!identical(status, 0L)) {

    feedback <- lines(
      "- Cannot compile C / C++ files from source.",
      "  Please ensure you have a compiler toolchain installed."
    )

    problems$push(feedback)

  }

}

renv_preflight_java <- function(lockfile, problems) {

  # no need to check if we're not using rJava
  records <- renv_lockfile_records(lockfile)
  if (is.null(records[["rJava"]]))
    return(TRUE)

  # TODO: no need to do anything if we're only installing binaries?
  switch(
    Sys.info()[["sysname"]],
    Windows = renv_preflight_java_windows(problems),
    renv_preflight_java_unix(problems)
  )

}

renv_preflight_java_windows <- function(problems) {

  home <- Sys.getenv("JAVA_HOME", unset = NA)
  feedback <- case(

    is.na(home) ~ lines(
      "- JAVA_HOME is not set.",
      "  Please ensure you have a Java Development Kit (JDK) installed."
    ),

    !file.exists(home) ~ lines(
      "- JAVA_HOME is set to a non-existent directory.",
      "  Please ensure you have a Java Development Kit (JDK) installed."
    )

  )

  if (!is.null(feedback))
    problems$push(feedback)

}

renv_preflight_java_unix <- function(problems) {

  args <- c("CMD", "javareconf", "--dry-run")
  status <- system2(R(), args, stdout = FALSE, stderr = FALSE)
  if (!identical(status, 0L)) {

    feedback <- lines(
      "- Cannot compile Java files from source.",
      "  Please ensure you have a Java Development Kit (JDK) installed."
    )

    problems$push(feedback)

  }

}


# pretty.R -------------------------------------------------------------------


renv_pretty_print_records <- function(preamble, records, postamble = NULL) {

  if (empty(records))
    return(invisible(NULL))

  if (!renv_verbose())
    return(invisible(NULL))

  # NOTE: use 'sort()' rather than 'csort()' here so that
  # printed output is sorted in the expected way in the users locale
  # https://github.com/rstudio/renv/issues/1289
  names(records) <- names(records) %||% map_chr(records, `[[`, "Package")
  records <- records[sort(names(records))]
  packages <- names(records)
  descs <- map_chr(records, renv_record_format_short)
  text <- sprintf("- %s [%s]", format(packages), descs)

  all <- c(preamble, text, postamble, if (length(postamble)) "")
  renv_caution_impl(all)

}

renv_pretty_print_records_pair <- function(preamble,
                                                old,
                                                new,
                                                postamble = NULL,
                                                formatter = NULL)
{
  formatter <- formatter %||% renv_record_format_pair

  all <- c(
    c(preamble, ""),
    renv_pretty_print_records_pair_impl(old, new, formatter),
    if (length(postamble)) c(postamble, "")
  )

  renv_caution_impl(all)
}

renv_pretty_print_records_pair_impl <- function(old, new, formatter) {

  # NOTE: use 'sort()' rather than 'csort()' here so that
  # printed output is sorted in the expected way in the users locale
  # https://github.com/rstudio/renv/issues/1289
  all <- sort(union(names(old), names(new)))

  # compute groups
  groups <- map_chr(all, function(package) {

    lhs <- old[[package]]; rhs <- new[[package]]
    case(
      is.null(lhs$Source)      ~ rhs$Repository %||% rhs$Source,
      is.null(rhs$Source)      ~ lhs$Repository %||% lhs$Source,
      !is.null(rhs$Repository) ~ rhs$Repository,
      !is.null(rhs$Source)     ~ rhs$Source
    )

  })

  n <- max(nchar(all))

  # iterate over each group and print
  uapply(csort(unique(groups)), function(group) {

    lhs <- renv_records_select(old, groups, group)
    rhs <- renv_records_select(new, groups, group)

    nms <- union(names(lhs), names(rhs))
    text <- map_chr(nms, function(nm) {
      formatter(lhs[[nm]], rhs[[nm]])
    })

    if (group == "unknown")
      group <- "(Unknown Source)"

    c(
      header(group),
      paste("-", format(nms, width = n), " ", text),
      ""
    )

  })

}

# NOTE: Used by vetiver, so perhaps is part of the API.
# We should think of a cleaner way of exposing this.
# https://github.com/rstudio/renv/issues/1413
renv_pretty_print_impl <- renv_caution_impl


# process.R ------------------------------------------------------------------


# NOTE: We use 'psnice()' here as R also supports using that
# for process detection on Windows; on all platforms R returns
# NA if you request information about a non-existent process
renv_process_exists <- function(pid) {
  !is.na(psnice(pid))
}

renv_process_kill <- function(pid, signal = 15L) {
  pskill(pid, signal)
}


# profile.R ------------------------------------------------------------------


renv_profile_prefix <- function() {
  renv_bootstrap_profile_prefix()
}

renv_profile_get <- function() {
  renv_bootstrap_profile_get()
}

renv_profile_set <- function(profile) {
  renv_bootstrap_profile_set(profile)
}

renv_profile_normalize <- function(profile) {
  renv_bootstrap_profile_normalize(profile)
}


# progress.R -----------------------------------------------------------------


renv_progress_create <- function(max, wait = 1.0) {

  # local variables for closure
  count <- 0L
  max <- max
  message <- ""
  start <- Sys.time()

  function() {

    # check for and print progress
    count <<- count + 1L

    # if not enough time has elapsed yet, nothing to do
    if (Sys.time() - start < wait)
      return()

    # create message
    backspaces <- paste(rep("\b", nchar(message)), collapse = "")
    message <<- sprintf("[%i/%i] ", count, max)
    all <- paste(backspaces, message, sep = "")
    cat(all, file = stdout(), sep = "")

  }

}

renv_progress_callback <- function(callback, max, wait = 1.0) {
  tick <- renv_progress_create(max, wait)
  function(...) { tick(); callback(...) }
}


# project.R ------------------------------------------------------------------


# The path to the currently-loaded project, if any.
# NULL when no project is currently loaded.
the$project_path <- NULL

# Flag indicating whether we're checking if the project is synchronized.
the$project_synchronized_check_running <- FALSE

#' Retrieve the active project
#'
#' Retrieve the path to the active project (if any).
#'
#' @param default The value to return when no project is
#'   currently active. Defaults to `NULL`.
#'
#' @export
#'
#' @return The active project directory, as a length-one character vector.
#'
#' @examples
#' \dontrun{
#'
#' # get the currently-active renv project
#' renv::project()
#'
#' }
project <- function(default = NULL) {
  renv_project_get(default = default)
}

renv_project_get <- function(default = NULL) {
  the$project_path %||% default
}

# NOTE: RENV_PROJECT kept for backwards compatibility with RStudio
renv_project_set <- function(project) {
  the$project_path <- project
  Sys.setenv(RENV_PROJECT = project)
}

# NOTE: 'RENV_PROJECT' kept for backwards compatibility with RStudio
renv_project_clear <- function() {
  the$project_path <- NULL
  Sys.unsetenv("RENV_PROJECT")
}

renv_project_resolve <- function(project = NULL, default = getwd()) {
  project <- project %||% renv_project_get(default = default)
  renv_path_normalize(project)
}

renv_project_initialized <- function(project) {

  lockfile <- renv_lockfile_path(project)
  if (file.exists(lockfile))
    return(TRUE)

  library <- renv_paths_library(project = project)
  if (file.exists(library))
    return(TRUE)

  FALSE

}

renv_project_type <- function(path) {

  if (!nzchar(path))
    return("unknown")

  path <- renv_path_normalize(path)
  filebacked(
    context  = "renv_project_type",
    path     = file.path(path, "DESCRIPTION"),
    callback = renv_project_type_impl
  )

}

renv_project_type_impl <- function(path) {

  if (!file.exists(path))
    return("unknown")

  desc <- tryCatch(
    renv_dcf_read(path),
    error = identity
  )

  if (inherits(desc, "error"))
    return("unknown")

  type <- desc$Type
  if (!is.null(type))
    return(tolower(type))

  package <- desc$Package
  if (!is.null(package))
    return("package")

  "unknown"

}

renv_project_remotes <- function(project, fields = NULL) {

  descpath <- file.path(project, "DESCRIPTION")
  if (!file.exists(descpath))
    return(NULL)

  # first, parse remotes (if any)
  remotes <- renv_description_remotes(descpath)

  # next, find packages mentioned in the DESCRIPTION file
  deps <- renv_dependencies_discover_description(
    path    = descpath,
    project = project
  )

  if (empty(deps))
    return(list())

  # split according to package
  specs <- split(deps, deps$Package)

  # drop ignored specs
  ignored <- renv_project_ignored_packages(project = project)
  specs <- specs[setdiff(names(specs), c("R", ignored))]

  # if any Roxygen fields are included,
  # infer a dependency on roxygen2 and devtools
  desc <- renv_description_read(descpath)
  if (any(grepl("^Roxygen", names(desc)))) {
    for (package in c("devtools", "roxygen2")) {
      if (!package %in% ignored) {
        specs[[package]] <-
          specs[[package]] %||%
          renv_dependencies_list(descpath, package, dev = TRUE)
      }
    }
  }

  # now, try to resolve the packages
  records <- enumerate(specs, function(package, spec) {

    # use remote if supplied
    if (!is.null(remotes[[package]]))
      return(remotes[[package]])

    # check for explicit version requirement
    explicit <- spec[spec$Require == "==", ]
    if (nrow(explicit) == 0)
      return(renv_remotes_resolve(package))

    version <- spec$Version[[1]]
    if (!nzchar(version))
      return(renv_remotes_resolve(package))

    entry <- paste(package, version, sep = "@")
    renv_remotes_resolve(entry)

  })

  # return records
  records

}

renv_project_ignored_packages <- function(project) {

  # if we don't have a project, nothing to do
  if (is.null(project))
    return(character())

  # read base set of ignored packages
  ignored <- c(
    settings$ignored.packages(project = project),
    renv_project_ignored_packages_self(project)
  )

  # return collected set of ignored packages
  ignored

}

renv_project_ignored_packages_self <- function(project) {

  # only ignore self in package projects
  if (renv_project_type(project) != "package")
    return(NULL)

  # read current package
  desc <- renv_description_read(project)
  package <- desc[["Package"]]

  # respect user preference if set
  ignore <- getOption("renv.snapshot.ignore.self", default = NULL)
  if (identical(ignore, TRUE))
    return(package)
  else if (identical(ignore, FALSE))
    return(NULL)

  # don't ignore self in golem projets
  golem <- file.path(project, "inst/golem-config.yml")
  if (file.exists(golem))
    return(NULL)

  # hack for renv: don't depend on self
  if (identical(package, "renv"))
    return(NULL)

  # return the package name
  package

}

renv_project_id <- function(project) {

  idpath <- renv_id_path(project = project)
  if (!file.exists(idpath)) {
    id <- renv_id_generate()
    writeLines(id, con = idpath)
  }

  readLines(idpath, n = 1L, warn = FALSE)

}

# TODO: this gets really dicey once the user starts configuring where
# renv places its project-local state ...
renv_project_find <- function(path = NULL) {

  path <- path %||% getwd()

  anchors <- c("renv.lock", "renv/activate.R")
  resolved <- renv_file_find(path, function(parent) {
    for (anchor in anchors)
      if (file.exists(file.path(parent, anchor)))
        return(parent)
  })

  if (is.null(resolved)) {
    fmt <- "couldn't resolve renv project associated with path %s"
    stopf(fmt, renv_path_pretty(path))
  }

  resolved

}

renv_project_lock <- function(project = NULL) {

  if (!config$locking.enabled())
    return()

  path <- the$project_path
  if (!identical(project, path))
    return()

  project <- renv_project_resolve(project)
  path <- file.path(project, "renv/lock")
  ensure_parent_directory(path)
  renv_scope_lock(path, scope = parent.frame())

}

renv_project_loaded <- function(project) {
  !is.null(project) && identical(project, the$project_path)
}


# properties.R ---------------------------------------------------------------


renv_properties_read <- function(path = NULL,
                                 text = NULL,
                                 delimiter = ":",
                                 dequote = TRUE,
                                 trim = TRUE)
{
  renv_scope_options(warn = -1L)

  # read file
  contents <- paste(text %||% readLines(path, warn = FALSE), collapse = "\n")

  # split on newlines; allow spaces to continue a value
  parts <- strsplit(contents, "\\n(?=\\S)", perl = TRUE)[[1L]]

  # remove comments and blank lines
  parts <- grep("^\\s*(?:#|$)", parts, perl = TRUE, value = TRUE, invert = TRUE)

  # split into key / value pairs
  index <- regexpr(delimiter, parts, fixed = TRUE)
  keys <- substring(parts, 1L, index - 1L)
  vals <- substring(parts, index + 1L)

  # trim whitespace when requested
  if (trim) {
    keys <- trimws(keys)
    vals <- gsub("\n\\s*", " ", trimws(vals), perl = TRUE)
  }

  # strip quotes if requested
  if (dequote) {
    keys <- dequote(keys)
    vals <- dequote(vals)
  }

  # return as named list
  storage.mode(vals) <- "list"
  names(vals) <- keys

  vals

}


# purge.R --------------------------------------------------------------------


#' Purge packages from the cache
#'
#' Purge packages from the cache. This can be useful if a package which had
#' previously been installed in the cache has become corrupted or unusable,
#' and needs to be reinstalled.
#'
#' `purge()` is an inherently destructive option. It removes packages from the
#' cache, and so any project which had symlinked that package into its own
#' project library would find that package now unavailable. These projects would
#' hence need to reinstall any purged packages. Take heed of this in case you're
#' looking to purge the cache of a package which is difficult to install, or
#' if the original sources for that package are no longer available!
#'
#' @inherit renv-params
#'
#' @param package A single package to be removed from the cache.
#' @param version The package version to be removed. When `NULL`, all versions
#'   of the requested package will be removed.
#' @param hash The specific hashes to be removed. When `NULL`, all hashes
#'   associated with a particular package's version will be removed.
#'
#' @return The set of packages removed from the renv global cache,
#'   as a character vector of file paths.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # remove all versions of 'digest' from the cache
#' renv::purge("digest")
#'
#' # remove only a particular version of 'digest' from the cache
#' renv::purge("digest", version = "0.6.19")
#'
#' }
purge <- function(package,
                  ...,
                  version = NULL,
                  hash    = NULL,
                  prompt  = interactive())
{
  renv_scope_error_handler()
  renv_dots_check(...)
  renv_scope_verbose_if(prompt)
  invisible(renv_purge_impl(package, version, hash, prompt))
}

renv_purge_impl <- function(package,
                            version = NULL,
                            hash = NULL,
                            prompt = interactive())
{
  if (length(package) != 1)
    stop("argument 'package' is not of length one", call. = FALSE)

  bail <- function() {
    writef("- The requested package is not installed in the cache -- nothing to do.")
    character()
  }

  # get root cache path entry for package
  paths <- renv_paths_cache(package)
  if (!any(file.exists(paths)))
    return(bail())

  # construct versioned path
  paths <- if (is.null(version))
    list.files(paths, full.names = TRUE)
  else
    file.path(paths, version)
  if (!any(file.exists(paths)))
    return(bail())

  # construct hashed path
  paths <- if (is.null(hash))
    list.files(paths, full.names = TRUE)
  else
    file.path(paths, hash)
  if (all(!file.exists(paths)))
    return(bail())

  # now add package name
  paths <- file.path(paths, renv_path_component(paths, 3))

  # check that these entries exist
  missing <- !file.exists(paths)
  if (any(missing)) {

    caution_bullets(
      "The following entries were not found in the cache:",
      paths[missing],
      "They will be ignored."
    )

    paths <- paths[!missing]

  }

  # nocov start
  if (prompt || renv_verbose()) {

    caution_bullets(
      "The following packages will be purged from the cache:",
      renv_cache_format_path(paths)
    )

    cancel_if(prompt && !proceed())

  }
  # nocov end

  unlink(paths, recursive = TRUE)
  renv_cache_clean_empty()

  n <- length(paths)
  writef("- Removed %s from the cache.", nplural("package", n))

  invisible(paths)

}


# pyenv.R --------------------------------------------------------------------


renv_pyenv_root <- function() {
  root <- Sys.getenv("PYENV_ROOT", unset = renv_pyenv_root_default())
  path.expand(root)
}

renv_pyenv_root_default <- function() {

  if (renv_platform_windows())
    "~/.pyenv/pyenv-win"
  else
    "~/.pyenv"

}



# python-conda.R -------------------------------------------------------------


renv_python_conda_select <- function(name, version = NULL) {

  # get python package
  version <- version %||% Sys.getenv("RENV_CONDA_PYTHON_VERSION", unset = "3.7")
  packages <- paste("python", version, sep = "=")

  # handle paths (as opposed to environment names)
  if (grepl("[/\\\\]", name)) {
    if (!file.exists(name))
      return(reticulate::conda_create(envname = name, packages = packages))
    return(renv_python_exe(name))
  }

  # check for an existing conda environment
  envs <- reticulate::conda_list()
  idx <- which(name == envs$name)
  if (length(idx))
    return(envs$python[[idx]])

  # no environment exists; create it
  reticulate::conda_create(envname = name, packages = packages)

}

renv_python_conda_export_path <- function(project) {

  # check override
  override <- renv_paths_override("CONDA_EXPORT")
  if (!is.null(override))
    return(override)

  # use default
  file.path(project, "environment.yml")

}

# TODO: support prompt
renv_python_conda_snapshot <- function(project, prompt, python) {

  renv_scope_wd(project)

  path <- renv_python_conda_export_path(project = project)

  # find the root of the associated conda environment
  lockfile <- renv_lockfile_load(project = project)
  name <- lockfile$Python$Name %||% renv_python_envpath(project, "conda", version)
  python <- renv_python_conda_select(name)
  info <- renv_python_info(python)
  prefix <- info$root

  conda <- reticulate::conda_binary()
  args <- c(
    "env", "export",
    "--prefix", renv_shell_path(prefix),
    "--file", renv_shell_path(path)
  )

  output <- if (renv_tests_running()) FALSE else ""
  system2(conda, args, stdout = output, stderr = output)

  writef("- Wrote Python packages to '%s'.", renv_path_aliased(path))
  return(TRUE)
}

# TODO: support prompt
renv_python_conda_restore <- function(project, prompt, python) {

  renv_scope_wd(project)

  path <- renv_python_conda_export_path(project = project)

  # find the root of the associated conda environment
  lockfile <- renv_lockfile_load(project = project)
  name <- lockfile$Python$Name %||% renv_python_envpath(project, "conda", version)
  python <- renv_python_conda_select(name)
  info <- renv_python_info(python)
  prefix <- info$root

  conda <- reticulate::conda_binary()
  cmd <- if (file.exists(prefix)) "update" else "create"
  args <- c(
    "env", cmd,
    "--prefix", renv_shell_path(prefix),
    "--file", renv_shell_path(path)
  )

  output <- if (renv_tests_running()) FALSE else ""
  system2(conda, args, stdout = output, stderr = output)

  return(TRUE)

}


# python-virtualenv.R --------------------------------------------------------


renv_python_virtualenv_home <- function() {
  Sys.getenv("WORKON_HOME", unset = "~/.virtualenvs")
}

renv_python_virtualenv_path <- function(name) {

  # if the name contains a slash, use it as-is
  if (grepl("/", name, fixed = TRUE))
    return(renv_path_canonicalize(name))

  # treat names starting with '.' specially
  if (substring(name, 1L, 1L) == ".")
    return(renv_path_canonicalize(name))

  # otherwise, resolve relative to virtualenv home
  home <- renv_python_virtualenv_home()
  file.path(home, name)

}

renv_python_virtualenv_validate <- function(path, version) {

  # get path to python executable
  python <- renv_python_exe(path)

  # compare requested + actual versions
  if (!is.null(version)) {
    request <- renv_version_maj_min(version)
    current <- renv_version_maj_min(renv_python_version(python))
    if (request != current) {
      fmt <- "Project requested Python version '%s' but '%s' is currently being used"
      warningf(fmt, request, current)
    }
  }

  python

}

renv_python_virtualenv_create <- function(python, path) {

  ensure_parent_directory(path)

  python <- renv_path_canonicalize(python)
  version <- renv_python_version(python)
  module <- if (numeric_version(version) > "3.2") "venv" else "virtualenv"
  args <- c("-m", module, renv_shell_path(path))
  renv_system_exec(python, args, "creating virtual environment")

  info <- renv_python_info(path)
  info$python

}

renv_python_virtualenv_update <- function(python) {

  # resolve python executable path
  python <- renv_python_exe(python)
  python <- renv_path_canonicalize(python)

  # resolve packages
  packages <- c("pip", "setuptools", "wheel")

  # don't upgrade these packages for older versions of python, as we may
  # end up installing versions of packages that aren't actually compatible
  # with the version of python we're running
  version <- renv_python_version(python)
  if (renv_version_lt(version, "3.6"))
    return(TRUE)

  # perform the install
  # make errors non-fatal as the environment will still be functional even
  # if we're not able to install or update these packages
  status <- catch(pip_install(packages, python = python))
  if (inherits(status, "error"))
    warnify(status)

  TRUE

}

renv_python_virtualenv_snapshot <- function(project, prompt, python) {

  renv_scope_wd(project)

  path <- file.path(project, "requirements.txt")
  before <- character()
  if (file.exists(path))
    before <- readLines(path, warn = FALSE)

  after <- pip_freeze(python = python)
  if (setequal(before, after)) {
    writef("- Python requirements are already up to date.")
    return(FALSE)
  }

  caution_bullets("The following will be written to requirements.txt:", after)

  cancel_if(prompt && !proceed())

  writeLines(after, con = path)

  fmt <- "- Wrote Python packages to %s."
  writef(fmt, renv_path_pretty(path))
  return(TRUE)

}

renv_python_virtualenv_restore <- function(project, prompt, python) {

  renv_scope_wd(project)

  path <- file.path(project, "requirements.txt")
  if (!file.exists(path))
    return(FALSE)

  before <- readLines(path, warn = FALSE)
  after <- pip_freeze(python = python)
  diff <- renv_vector_diff(before, after)
  if (empty(diff)) {
    writef("- The Python library is already up to date.")
    return(FALSE)
  }

  caution_bullets("The following Python packages will be restored:", diff)

  cancel_if(prompt && !proceed())

  pip_install_requirements(diff, python = python, stream = TRUE)
  TRUE

}


# python.R -------------------------------------------------------------------


renv_python_resolve <- function(python = NULL) {

  # if Python was explicitly supplied, use it
  if (!is.null(python)) {

    resolved <- Sys.which(renv_path_canonicalize(python))
    if (nzchar(resolved))
      return(resolved)

    stopf("'%s' does not refer to a valid python interpreter", python)

  }

  # in interactive sessions, ask user what version of python they'd like to use
  if (interactive()) {

    python <- renv_python_select()

    fmt <- "- Selected %s [Python %s]."
    writef(fmt, renv_path_pretty(python), renv_python_version(python))

    return(path.expand(python))

  }

  # check environment variables
  envvars <- c("RETICULATE_PYTHON", "RETICULATE_PYTHON_ENV")
  for (envvar in envvars) {
    val <- Sys.getenv(envvar, unset = NA)
    if (!is.na(val) && file.exists(val))
      return(val)
  }

  # check on the PATH (prefer Python 3)
  for (binary in c("python3", "python")) {
    python <- Sys.which(binary)
    if (nzchar(python))
      return(python)
  }

  stopf("could not locate Python (not available on the PATH)")

}

renv_python_find <- function(version, path = NULL) {
  renv_python_find_impl(version, path)
}

renv_python_find_impl <- function(version, path = NULL) {

  # if we've been given the name of an environment,
  # check to see if it's already been initialized
  # and use the associated copy of Python if possible
  if (!is.null(path) && file.exists(path)) {
    python <- catch(renv_python_exe(path))
    if (!inherits(python, "error"))
      return(python)
  }

  # try to find a compatible version of python
  pythons <- renv_python_discover()
  if (length(pythons) == 0) {

    fmt <- lines(
      "project requested Python %s, but no compatible Python installation could be found.",
      "renv's Python integration will be disabled in this session.",
      "See `?renv::use_python` for more details."
    )

    stopf(fmt, version)

  }

  # read python versions
  pyversions <- map_chr(pythons, function(python) {
    tryCatch(
      renv_python_version(python),
      error = function(e) "0.0.0"
    )
  })

  # try to find a compatible version
  renv_version_match(pyversions, version)

}

renv_python_exe <- function(path) {

  # if this already looks like a Python executable, use it directly
  info <- renv_file_info(path)
  if (identical(info$isdir, FALSE) && startswith(basename(path), "python"))
    return(renv_path_canonicalize(path))

  # otherwise, attempt to infer the Python executable type
  info <- renv_python_info(path)
  if (!is.null(info$python))
    return(renv_path_canonicalize(info$python))

  fmt <- "failed to find Python executable associated with path %s"
  stopf(fmt, renv_path_pretty(path))

}

renv_python_version <- function(python) {

  filebacked(
    context  = "renv_python_version",
    path     = renv_path_normalize(python),
    callback = renv_python_version_impl
  )

}

renv_python_version_impl <- function(python) {
  python <- renv_path_canonicalize(python)
  code <- "from platform import python_version; print(python_version())"
  args <- c("-c", shQuote(code))
  action <- "reading Python version"
  renv_system_exec(python, args, action)
}

renv_python_info <- function(python) {

  found <- renv_file_find(python, function(path) {

    # check for virtual environment files
    virtualenv <-
      file.exists(file.path(path, "pyvenv.cfg")) ||
      file.exists(file.path(path, ".Python")) ||
      file.exists(file.path(path, "bin/activate_this.py"))

    if (virtualenv) {
      suffix <- if (renv_platform_windows()) "Scripts/python.exe" else "bin/python"
      python <- file.path(path, suffix)
      return(list(python = python, type = "virtualenv", root = path))
    }

    # check for conda-meta
    condaenv <-
      file.exists(file.path(path, "conda-meta")) &&
      !file.exists(file.path(path, "condabin"))

    if (condaenv) {
      suffix <- if (renv_platform_windows()) "python.exe" else "bin/python"
      python <- file.path(path, suffix)
      return(list(python = python, type = "conda", root = path))
    }

  })

  if (!is.null(found))
    return(found)

  if (file.exists(python))
    list(python = python, type = "system", root = python)

}

renv_python_type <- function(python) {
  info <- renv_python_info(python)
  info$type
}

renv_python_action <- function(action, prompt, project) {

  python <- Sys.getenv("RENV_PYTHON", unset = NA)
  if (is.na(python) || !file.exists(python))
    return(NULL)

  type <- renv_python_type(python)
  if (is.null(type))
    return(NULL)

  if (type == "conda" && !requireNamespace("reticulate", quietly = TRUE))
    return(NULL)

  action(python, type, prompt, project)

}

renv_python_snapshot <- function(project, prompt) {
  renv_python_action(
    renv_python_snapshot_impl,
    prompt  = prompt,
    project = project
  )
}

renv_python_snapshot_impl <- function(python, type, prompt, project) {

  switch(type,
    virtualenv = renv_python_virtualenv_snapshot(project, prompt, python),
    conda      = renv_python_conda_snapshot(project, prompt, python)
  )

}

renv_python_restore <- function(project, prompt) {
  renv_python_action(
    renv_python_restore_impl,
    prompt  = prompt,
    project = project
  )
}

renv_python_restore_impl <- function(python, type, prompt, project) {

  case(
    type == "virtualenv" ~ renv_python_virtualenv_restore(project, prompt, python),
    type == "conda"      ~ renv_python_conda_restore(project, prompt, python)
  )

}

renv_python_envpath_virtualenv <- function(version) {
  sprintf("python/virtualenvs/renv-python-%s", renv_version_maj_min(version))
}

renv_python_envpath_condaenv <- function(version) {
  "python/condaenvs/renv-python"
}

renv_python_envpath <- function(project, type, version = NULL) {

  suffix <- case(
    type == "virtualenv" ~ renv_python_envpath_virtualenv(version),
    type == "conda"      ~ renv_python_envpath_condaenv(version),
    ~ stopf("internal error: unrecognized environment type '%s'", type)
  )

  renv_paths_renv(suffix, project = project)

}

renv_python_envname <- function(project, path, type) {

  # check for a project-local environment
  if (renv_path_within(path, project)) {
    stem <- substring(path, nchar(project) + 2L)
    path <- paste(".", stem, sep = "/")
    return(path)
  }

  bn <- basename(path)

  # check for file within virtualenv
  ok <-
    type == "virtualenv" &&
    identical(renv_python_virtualenv_path(bn), path)

  if (ok)
    return(bn)

  # check for named conda environment
  ok <-
    type == "conda" &&
    bn %in% reticulate::conda_list()$name

  if (ok)
    return(bn)

  # doesn't match any known named environments; return full path
  path

}

renv_python_discover <- function() {

  all <- stack()

  # find python in some pre-determined root directories
  roots <- c(
    getOption("renv.python.root"),
    Sys.getenv("WORKON_HOME", "~/.virtualenvs"),
    "/opt/python",
    "/opt/local/python",
    "~/opt/python",
    file.path(renv_pyenv_root(), "versions")
  )

  for (root in roots) {
    versions <- sort(list.files(root, full.names = TRUE), decreasing = TRUE)
    exts <- if (renv_platform_windows()) "Scripts/python.exe" else "bin/python"
    pythons <- file.path(versions, exts)
    all$push(pythons)
  }

  # find Homebrew python
  if (renv_platform_macos()) {

    homebrew <- renv_homebrew_root()
    roots <- sort(list.files(
      path       = file.path(homebrew, "opt"),
      pattern    = "^python@[[:digit:]]+[.][[:digit:]]+$",
      full.names = TRUE
    ), decreasing = TRUE)

    for (root in roots) {

      # homebrew python doesn't install bin/python, so we need
      # to be a little bit more clever here
      exes <- list.files(
        path = file.path(root, "bin"),
        pattern = "^python[[:digit:]]+[.][[:digit:]]+$",
        full.names = TRUE
      )

      if (length(exes))
        all$push(exes[[1L]])

    }

  }

  # find Windows python installations
  if (renv_platform_windows()) {

    sd <- Sys.getenv("SYSTEMDRIVE", unset = "C:")
    roots <- file.path(sd, c("", "Program Files"))

    lad <- Sys.getenv("LOCALAPPDATA", unset = NA)
    if (!is.na(lad))
      roots <- c(roots, file.path(lad, "Programs/Python"))

    dirs <- list.files(
      path       = roots,
      pattern    = "^Python",
      full.names = TRUE
    )

    if (length(dirs)) {
      exes <- file.path(dirs, "python.exe")
      pythons <- renv_path_normalize(exes)
      all$push(pythons)
    }

  }

  # find Python installations on the PATH
  path <- Sys.getenv("PATH", unset = "")
  splat <- strsplit(path, .Platform$path.sep, fixed = TRUE)[[1L]]
  for (entry in splat) {
    for (exe in c("python3", "python")) {
      python <- Sys.which(file.path(entry, exe))
      if (nzchar(python))
        all$push(python)
    }
  }

  # collect discovered pythons as vector
  pythons <- unlist(all$data(), recursive = FALSE, use.names = TRUE)

  # don't include /usr/bin/python on macOS (too old)
  if (renv_platform_macos())
    pythons <- setdiff(pythons, "/usr/bin/python")

  # get list of pythons
  pythons <- renv_path_canonicalize(pythons[file.exists(pythons)])

  # don't include WindowsApps
  if (renv_platform_windows())
    pythons <- grep("/WindowsApps/", pythons, invert = TRUE, value = TRUE)

  unique(pythons)

}

renv_python_select_error <- function() {

  lines <- c(
    "renv was unable to find any Python installations on your machine.",
    if (renv_platform_windows())
      "Consider installing Python from https://www.python.org/downloads/windows/.",
    if (renv_platform_macos())
      "Consider installing Python from https://www.python.org/downloads/mac-osx/."
  )

  stop(paste(lines, collapse = "\n"))

}

renv_python_select <- function(candidates = NULL) {

  candidates <- renv_path_aliased(candidates %||% renv_python_discover())
  if (empty(candidates))
    return(renv_python_select_error())

  title <- "Please select a version of Python to use with this project:"
  selection <- tryCatch(
    utils::select.list(candidates, title = title, graphics = FALSE),
    interrupt = identity
  )

  if (selection %in% "" || inherits(selection, "interrupt"))
    stop("operation canceled by user")

  return(path.expand(selection))

}

renv_python_module_available <- function(python, module) {
  python <- renv_path_canonicalize(python)
  command <- paste("import", module)
  args <- c("-c", shQuote(command))
  status <- system2(python, args, stdout = FALSE, stderr = FALSE)
  identical(status, 0L)
}

renv_python_active <- function() {

  python <- Sys.getenv("RENV_PYTHON", unset = NA)
  if (is.na(python))
    stop("internal error: RENV_PYTHON is not set")

  renv_python_validate(python)

}

renv_python_validate <- function(python) {

  if (!file.exists(python)) {
    fmt <- "python %s does not exist"
    stopf(fmt, renv_path_pretty(python))
  }

  invisible(python)

}


# r.R ------------------------------------------------------------------------


R <- function() {
  bin <- normalizePath(R.home("bin"), winslash = "/")
  exe <- if (renv_platform_windows()) "R.exe" else "R"
  file.path(bin, exe)
}

r <- function(args, ...) {

  # ensure R_LIBS is set; unset R_LIBS_USER and R_LIBS_SITE
  # so that R_LIBS will always take precedence
  rlibs <- paste(renv_libpaths_all(), collapse = .Platform$path.sep)
  renv_scope_envvars(R_LIBS = rlibs, R_LIBS_USER = "NULL", R_LIBS_SITE = "NULL")

  # ensure Rtools is on the PATH for Windows
  renv_scope_rtools()

  # invoke r
  suppressWarnings(system2(R(), args, ...))

}

r_exec_error <- function(package, output, label, extra) {

  # installation failed; write output for user
  fmt <- "Error %sing package '%s':"
  header <- sprintf(fmt, label, package)

  lines <- paste(rep("=", nchar(header)), collapse = "")

  # try to add diagnostic information if possible
  diagnostics <- r_exec_error_diagnostics(package, output)
  if (!empty(diagnostics)) {
    size <- min(getOption("width"), 78L)
    dividers <- paste(rep.int("-", size), collapse = "")
    output <- c(output, paste(dividers, diagnostics, collapse = "\n\n"))
  }

  # normalize 'extra'
  extra <- if (is.integer(extra))
    paste("error code", extra)
  else
    paste(renv_path_pretty(extra), "does not exist")

  # stop with an error
  footer <- sprintf("%s of package '%s' failed [%s]", label, package, extra)
  all <- c(header, lines, "", output, footer)
  abort(all)

}

r_exec_error_diagnostics_fortran_library <- function() {

  checker <- function(output) {
    pattern <- "library not found for -l(quadmath|gfortran|fortran)"
    idx <- grep(pattern, output, ignore.case = TRUE)
    if (length(idx))
      return(unique(output[idx]))
  }

  suggestion <- "
R was unable to find one or more FORTRAN libraries during compilation.
This often implies that the FORTRAN compiler has not been properly configured.
Please see https://stackoverflow.com/q/35999874 for more information.
"

  list(
    checker = checker,
    suggestion = suggestion
  )

}

r_exec_error_diagnostics_fortran_binary <- function() {

  checker <- function(output) {
    pattern <- "gfortran: no such file or directory"
    idx <- grep(pattern, output, ignore.case = TRUE)
    if (length(idx))
      return(unique(output[idx]))
  }

  suggestion <- "
R was unable to find the gfortran binary.
gfortran is required for the compilation of FORTRAN source files.
Please check that gfortran is installed and available on the PATH.
Please see https://stackoverflow.com/q/35999874 for more information.
"

  list(
    checker = checker,
    suggestion = suggestion
  )

}

r_exec_error_diagnostics_openmp <- function() {

  checker <- function(output) {
    pattern <- "unsupported option '-fopenmp'"
    idx <- grep(pattern, output, fixed = TRUE)
    if (length(idx))
      return(unique(output[idx]))
  }

  suggestion <- "
R is currently configured to use a compiler that does not have OpenMP support.
You may need to disable OpenMP, or update your compiler toolchain.
Please see https://support.bioconductor.org/p/119536/ for a related discussion.
"

  list(
    checker = checker,
    suggestion = suggestion
  )

}

r_exec_error_diagnostics <- function(package, output) {

  diagnostics <- list(
    r_exec_error_diagnostics_fortran_library(),
    r_exec_error_diagnostics_fortran_binary(),
    r_exec_error_diagnostics_openmp()
  )

  suggestions <- uapply(diagnostics, function(diagnostic) {

    check <- catch(diagnostic$checker(output))
    if (!is.character(check))
      return()

    suggestion <- diagnostics$suggestion
    reasons <- paste("-", shQuote(check), collapse = "\n")
    paste(diagnostic$suggestion, "Reason(s):", reasons, sep = "\n")

  })

  as.character(suggestions)

}

# install package called 'package' located at path 'path'
r_cmd_install <- function(package, path, ...) {

  # normalize path to package
  path <- renv_path_normalize(path, mustWork = TRUE)

  # unpack .zip source archives before install
  # https://github.com/rstudio/renv/issues/1359
  ftype <- renv_file_type(path)
  atype <- renv_archive_type(path)
  ptype <- renv_package_type(path)

  unpack <-
    ftype == "file" &&
    atype == "zip" &&
    ptype == "source"

  if (unpack) {
    newpath <- renv_package_unpack(package, path, force = TRUE)
    if (!identical(newpath, path)) {
      path <- newpath
      defer(unlink(path, recursive = TRUE))
    }
  }

  # rename binary .zip files if necessary
  rename <-
    ftype == "file" &&
    atype == "zip" &&
    ptype == "binary"

  if (rename) {
    regexps <- .standard_regexps()
    fmt <- "^%s(?:_%s)?\\.zip$"
    pattern <- sprintf(fmt, regexps$valid_package_name, regexps$valid_package_version)
    if (!grepl(pattern, basename(path), perl = TRUE)) {
      dir <- renv_scope_tempfile(package)
      ensure_directory(dir)
      newpath <- file.path(dir, paste(package, "zip", sep = "."))
      renv_file_copy(path, newpath)
      path <- newpath
    }
  }

  # resolve default library path
  library <- renv_libpaths_active()

  # validate that we have command line tools installed and
  # available for e.g. macOS
  if (renv_platform_macos() && renv_package_type(path) == "source")
    renv_xcode_check()

  # perform platform-specific pre-install checks
  renv_scope_install()

  # perform the install
  # note that we need to supply '-l' below as otherwise the library paths
  # could be changed by, for example, site-specific profiles
  args <- c(
    "--vanilla",
    "CMD", "INSTALL", "--preclean", "--no-multiarch", "--with-keep.source",
    r_cmd_install_option(package, "configure.args", TRUE),
    r_cmd_install_option(package, "configure.vars", TRUE),
    r_cmd_install_option(package, c("install.opts", "INSTALL_opts"), FALSE),
    "-l", renv_shell_path(library),
    ...,
    renv_shell_path(path)
  )

  if (config$install.verbose()) {

    status <- r(args, stdout = "", stderr = "")
    if (!identical(status, 0L))
      stopf("install of package '%s' failed", package)

    installpath <- file.path(library, package)
    if (!file.exists(installpath)) {
      fmt <- "install of package '%s' failed: %s does not exist"
      stopf(fmt, package, renv_path_pretty(installpath))
    }

    installpath

  } else {

    output <- r(args, stdout = TRUE, stderr = TRUE)
    status <- attr(output, "status") %||% 0L
    if (!identical(status, 0L))
      r_exec_error(package, output, "install", status)

    installpath <- file.path(library, package)
    if (!file.exists(installpath))
      r_exec_error(package, output, "install", installpath)

    installpath

  }


}

r_cmd_build <- function(package, path, ...) {

  path <- renv_path_normalize(path, mustWork = TRUE)
  args <- c("--vanilla", "CMD", "build", "--md5", ..., renv_shell_path(path))

  output <- r(args, stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status") %||% 0L
  if (!identical(status, 0L))
    r_exec_error(package, output, "build", status)

  pasted <- paste(output, collapse = "\n")
  pattern <- "[*] building .([a-zA-Z0-9_.-]+)."
  matches <- regexec(pattern, pasted)
  text <- regmatches(pasted, matches)

  tarball <- text[[1L]][[2L]]
  if (!file.exists(tarball))
    r_exec_error(package, output, "build", tarball)

  file.path(getwd(), tarball)

}

r_cmd_install_option <- function(package, options, configure) {

  # read option -- first, check for package-specific option, then
  # fall back to 'global' option
  for (option in options) {
    value <- r_cmd_install_option_impl(package, option, configure)
    if (!is.null(value))
      return(value)
  }

}

r_cmd_install_option_impl <- function(package, option, configure) {

  value <-
    getOption(paste(option, package, sep = ".")) %||%
    getOption(option)

  if (is.null(value))
    return(NULL)

  # if the value is named, treat it as a list,
  # mapping package names to their configure arguments
  if (!is.null(names(value)))
    value <- as.list(value)

  # check for named values
  if (!is.null(names(value))) {
    value <- value[[package]]
    if (is.null(value))
      return(NULL)
  }

  # if this is a configure option, format specially
  if (configure) {
    confkey <- sub(".", "-", option, fixed = TRUE)
    confval <- if (!is.null(names(value)))
      shQuote(paste(names(value), value, sep = "=", collapse = " "))
    else
      shQuote(paste(value, collapse = " "))
    return(sprintf("--%s=%s", confkey, confval))
  }

  # otherwise, just paste it
  paste(value, collapse = " ")

}

r_cmd_config <- function(...) {

  renv_system_exec(
    command = R(),
    args    = c("--vanilla", "CMD", "config", ...),
    action  = "reading R CMD config"
  )

}


# rebuild.R ------------------------------------------------------------------


#' Rebuild the packages in your project library
#'
#' Rebuild and reinstall packages in your library. This can be useful as a
#' diagnostic tool -- for example, if you find that one or more of your
#' packages fail to load, and you want to ensure that you are starting from a
#' clean slate.
#'
#' @inherit renv-params
#'
#' @param packages The package(s) to be rebuilt. When `NULL`, all packages
#'   in the library will be reinstalled.
#'
#' @param recursive Boolean; should dependencies of packages be rebuilt
#'   recursively? Defaults to `TRUE`.
#'
#' @return A named list of package records which were installed by renv.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # rebuild the 'dplyr' package + all of its dependencies
#' renv::rebuild("dplyr", recursive = TRUE)
#'
#' # rebuild only 'dplyr'
#' renv::rebuild("dplyr", recursive = FALSE)
#'
#' }
rebuild <- function(packages  = NULL,
                    recursive = TRUE,
                    ...,
                    type    = NULL,
                    prompt  = interactive(),
                    library = NULL,
                    project = NULL)
{
  renv_consent_check()
  renv_scope_error_handler()
  renv_dots_check(...)

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)
  renv_scope_verbose_if(prompt)

  libpaths <- renv_libpaths_resolve(library)
  library <- nth(libpaths, 1L)

  # get collection of packages currently installed
  records <- renv_snapshot_libpaths(libpaths = libpaths, project = project)
  packages <- setdiff(packages %||% names(records), "renv")

  # add in missing packages
  for (package in packages) {
    records[[package]] <- records[[package]] %||%
      renv_available_packages_latest(package)
  }

  # make sure records are named
  names(records) <- map_chr(records, `[[`, "Package")
  if (empty(records)) {
    writef("- There are no packages currently installed -- nothing to rebuild.")
    return(invisible(records))
  }


  # apply any overrides
  records <- renv_records_override(records)

  # notify the user
  preamble <- if (recursive)
    "The following package(s) and their dependencies will be reinstalled:"
  else
    "The following package(s) will be reinstalled:"

  renv_pretty_print_records(preamble, records[packages])
  cancel_if(prompt && !proceed())

  # figure out rebuild parameter
  rebuild <- if (recursive) NA else packages

  # perform the install
  install(
    packages = records[packages],
    library  = libpaths,
    type     = type,
    rebuild  = rebuild,
    project  = project
  )
}


# record.R -------------------------------------------------------------------


#' Update package records in a lockfile
#'
#' Use `record()` to record a new entry within an existing renv lockfile.
#'
#' This function can be useful when you need to change one or more of the
#' package records within an renv lockfile -- for example, because a recorded
#' package cannot be restored in a particular environment, and you know of a
#' suitable alternative.
#'
#' # Records
#'
#' Records can be provided either using the **remotes** short-hand syntax,
#' or by using an \R list of entries to record within the lockfile. See
#' `?lockfiles` for more information on the structure of a package record.
#'
#' @inheritParams renv-params
#'
#' @param records A list of named records, mapping package names to a definition
#'   of their source. See **Records** for more details.
#'
#' @example examples/examples-record.R
#' @export
record <- function(records,
                   lockfile = NULL,
                   project  = NULL)
{
  renv_scope_error_handler()

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  lockfile <- lockfile %||% renv_lockfile_path(project)

  records <- case(
    is.character(records) ~ lapply(records, renv_remotes_resolve, latest = TRUE),
    is.list(records)      ~ renv_records_resolve(records, latest = TRUE),
    ~ stopf("unexpected records format '%s'", typeof(records))
  )

  names(records) <- enum_chr(records, function(package, record) {
    if (is.null(package) || is.na(package) || !nzchar(package))
      record[["Package"]]
    else
      package
  })

  if (is.list(lockfile))
    return(renv_lockfile_modify(lockfile, records))

  if (!file.exists(lockfile)) {
    fmt <- "no lockfile exists at path %s"
    stopf(fmt, renv_path_pretty(lockfile))
  }

  old <- renv_lockfile_read(lockfile)
  new <- renv_lockfile_modify(old, records)

  local({
    renv_scope_options(renv.verbose = FALSE)
    renv_lockfile_write(new, lockfile)
  })

  n <- length(records)
  fmt <- "- Updated %s in %s."
  writef(fmt, nplural("record", n), renv_path_pretty(lockfile))

  renv <- records[["renv"]]
  if (!is.null(renv) && !is.null(renv[["Version"]])) {
    renv_infrastructure_write_activate(
      project = project,
      version = renv[["Version"]]
    )
  }

  invisible(lockfile)

}

renv_record_normalize <- function(record) {

  # normalize source
  source <- record$Source %||% "unknown"
  if (source %in% c("CRAN", "PPM", "RSPM"))
    record$Source <- "Repository"

  # drop remotes from records with a repository source
  if (identical(record$Source, "Repository") ||
      identical(record$RemoteType, "standard"))
    record <- record[grep("^Remote", names(record), invert = TRUE)]

  # keep only specific records for comparison
  remotes <- grep("^Remote", names(record), value = TRUE)
  keep <- c("Package", "Version", "Source", remotes)
  record <- record[intersect(names(record), keep)]

  # return normalized record
  record

}


# records.R ------------------------------------------------------------------


renv_records_select <- function(records, actions, action) {
  records <- renv_lockfile_records(records)
  matching <- actions[actions %in% action]
  keep(records, names(matching))
}

renv_records_sort <- function(records) {
  records[csort(names(records))]
}

renv_records_override <- function(records) {
  enumerate(records, renv_options_override, scope = "renv.records")
}

renv_record_names <- function(record, fields = NULL) {
  fields <- fields %||% c("Package", "Version", "Source")
  remotes <- grep("^Remote", names(record), value = TRUE)
  nms <- c(fields, setdiff(remotes, "Remotes"))
  renv_vector_intersect(nms, names(record))
}

renv_record_cacheable <- function(record) {

  # check if the record has been marked as cacheable
  cacheable <- record$Cacheable %||% TRUE
  if (identical(cacheable, FALSE))
    return(FALSE)

  # check for unknown source
  source <- renv_record_source(record)
  if (source == "unknown")
    return(FALSE)

  # record is ok
  TRUE

}

renv_record_source <- function(record, normalize = FALSE) {

  # if this appears to be a file path, then keep it as-is
  source <- record$Source %||% "unknown"
  if (grepl("[/\\]", source))
    return(source)

  # otherwise, try to normalize it
  source <- tolower(record$Source %||% "unknown")
  if (normalize)
    source <- renv_record_source_normalize(record, source)

  source

}

renv_record_source_normalize <- function(record, source) {

  # normalize different types of git remotes
  if (source %in% c("git2r", "xgit"))
    source <- "git"

  # handle old lockfiles where 'source' was explicitly set as CRAN
  if (source %in% c("cran"))
    source <- "repository"

  # check for ad-hoc requests to install from bioc
  if (identical(source, "repository")) {
    repos <- record$Repository %||% ""
    if (tolower(repos) %in% c("bioc", "bioconductor"))
      source <- "bioconductor"
  }

  # all done; return normalized source
  source

}

renv_record_validate <- function(package, record) {

  # check for a record -- minimally, a list with a package name
  if (is.list(record) && is.character(record$Package))
    return(record)

  # if we're running tests, or in CI, then report
  if (renv_tests_running() || renv_envvar_exists("CI")) {
    fmt <- "! Internal error: unexpected record for package '%s'"
    writef(fmt, package)
    print(record)
  }

  # return record as-is
  record

}

renv_record_format_remote <- function(record) {

  remotes <- c("RemoteUsername", "RemoteRepo")
  if (all(remotes %in% names(record)))
    return(renv_record_format_short_remote(record))

  paste(record$Package, record$Version, sep = "@")

}

renv_record_format_short <- function(record, versioned = FALSE) {

  remotes <- c("RemoteUsername", "RemoteRepo")
  if (all(remotes %in% names(record))) {
    remote <- renv_record_format_short_remote(record)
    if (versioned)
      remote <- sprintf("%s  [%s]", record$Version %||% "<NA>", remote)
    return(remote)
  }

  record$Version

}

renv_record_format_short_remote <- function(record) {

  text <- paste(record$RemoteUsername, record$RemoteRepo, sep = "/")

  subdir <- record$RemoteSubdir %||% ""
  if (nzchar(subdir))
    text <- paste(text, subdir, sep = ":")

  if (!is.null(record$RemoteRef)) {
    ref <- record$RemoteRef
    if (!identical(ref, "master"))
      text <- paste(text, record$RemoteRef, sep = "@")
  } else if (!is.null(record$RemoteSha)) {
    sha <- substring(record$RemoteSha, 1L, 8L)
    text <- paste(text, sha, sep = "@")
  }

  text

}

renv_record_format_pair <- function(lhs, rhs) {

  # check for install / remove
  if (is.null(lhs))
    return(sprintf("[* -> %s]", renv_record_format_short(rhs)))
  else if (is.null(rhs))
    return(sprintf("[%s -> *]", renv_record_format_short(lhs)))

  map <- list(
    Source         = "src",
    Repository     = "repo",
    Version        = "ver",
    RemoteHost     = "host",
    RemoteUsername = "user",
    RemoteRepo     = "repo",
    RemoteRef      = "ref",
    RemoteSha      = "sha",
    RemoteSubdir   = "subdir"
  )

  fields <- names(map)

  # check to see which fields have changed between the two
  diff <- map_lgl(fields, function(field) {
    !identical(lhs[[field]], rhs[[field]])
  })

  changed <- names(which(diff))

  if (empty(changed)) {
    fmt <- "[%s: unchanged]"
    lhsf <- renv_record_format_short(lhs)
    return(sprintf(fmt, lhsf))
  }

  # check for CRAN packages; in such cases, we typically want to ignore
  # the Remote fields which might've been added by 'pak' or other tools
  isrepo <-
    nzchar(lhs$Version %||% "") &&
    nzchar(rhs$Version %||% "") &&
    nzchar(lhs$Repository %||% "") &&
    nzchar(rhs$Repository %||% "") &&
    identical(lhs$Repository, rhs$Repository)

  if (isrepo) {
    fmt <- "[%s -> %s]"
    lhsf <- renv_record_format_short(lhs)
    rhsf <- renv_record_format_short(rhs)
    return(sprintf(fmt, lhsf, rhsf))
  }

  # check for only sha changed
  usesha <-
    setequal(changed, "RemoteSha") ||
    setequal(changed, c("RemoteSha", "Version"))

  if (usesha) {

    user <- lhs$RemoteUsername %||% "*"
    repo <- lhs$RemoteRepo %||% "*"
    spec <- paste(user, repo, sep = "/")

    ref <- lhs$RemoteRef %||% "*"
    if (!ref %in% c("master", "*"))
      spec <- paste(spec, ref, sep = "@")

    fmt <- "[%s: %s -> %s]"
    lsha <- substring(lhs$RemoteSha %||% "*", 1L, 8L)
    rsha <- substring(rhs$RemoteSha %||% "*", 1L, 8L)

    return(sprintf(fmt, spec, lsha, rsha))

  }

  # check for only source change
  if (setequal(changed, "Source")) {
    fmt <- "[%s: %s -> %s]"
    ver <- lhs$Version %||% "*"
    lhsf <- lhs$Source %||% "*"
    rhsf <- rhs$Source %||% "*"
    return(sprintf(fmt, ver, lhsf, rhsf))
  }

  # check only version changed
  if (setequal(changed, "Version")) {
    fmt <- "[%s -> %s]"
    lhsf <- lhs$Version %||% "*"
    rhsf <- rhs$Version %||% "*"
    return(sprintf(fmt, lhsf, rhsf))
  }

  # if the source has changed, highlight that
  if ("Source" %in% changed) {
    fmt <- "[%s -> %s]"
    lhsf <- renv_record_format_short(lhs)
    rhsf <- renv_record_format_short(rhs)
    return(sprintf(fmt, lhsf, rhsf))
  }

  # otherwise, report each diff individually
  diffs <- map_chr(changed, function(field) {

    lhsf <- lhs[[field]] %||% "*"
    rhsf <- rhs[[field]] %||% "*"

    if (field == "RemoteSha") {
      lhsf <- substring(lhsf, 1L, 8L)
      rhsf <- substring(rhsf, 1L, 8L)
    }

    fmt <- "%s: %s -> %s"
    sprintf(fmt, map[[field]], lhsf, rhsf)
  })

  sprintf("[%s]", paste(diffs, collapse = "; "))

}

renv_records_equal <- function(lhs, rhs) {

  lhs <- reject(lhs, is.null)
  rhs <- reject(rhs, is.null)

  nm <- setdiff(union(names(lhs), names(rhs)), "Hash")
  identical(keep(lhs, nm), keep(rhs, nm))

}

renv_records_resolve <- function(records, latest = FALSE) {

  enumerate(records, function(package, record) {

    # check for already-resolved records
    if (is.null(record) || is.list(record))
      return(record)

    # check for version-only specifications and
    # prepend the package name in such a case
    pattern <- "^(?:[[:digit:]]+[.-]){1,}[[:digit:]]+$"
    if (grepl(pattern, record))
      record <- paste(package, record, sep = "@")

    # resolve the record
    renv_remotes_resolve(record, latest)

  })

}


# recurse.R ------------------------------------------------------------------


recurse <- function(object, callback, ...) {
  renv_recurse_impl(list(), object, callback, ...)
}

renv_recurse_impl <- function(stack, object, callback, ...) {

  # ignore missing values
  if (missing(object) || identical(object, quote(expr = )))
    return(FALSE)

  # push node on to stack
  stack[[length(stack) + 1]] <- object

  # invoke callback
  result <- callback(object, stack, ...)
  if (is.call(result))
    object <- result
  else if (identical(result, FALSE))
    return(FALSE)

  # recurse
  if (is.recursive(object))
    for (i in seq_along(object))
      renv_recurse_impl(stack, object[[i]], callback, ...)

}


# refresh.R ------------------------------------------------------------------


#' Refresh the local cache of available packages
#'
#' Query the active R package repositories for available packages, and
#' update the in-memory cache of those packages.
#'
#' Note that \R also maintains its own on-disk cache of available packages,
#' which is used by `available.packages()`. Calling `refresh()` will force
#' an update of both types of caches. renv prefers using an in-memory
#' cache as on occasion the temporary directory can be slow to access (e.g.
#' when it is a mounted network filesystem).
#'
#' @return A list of package databases, invisibly -- one for each repository
#'   currently active in the \R session. Note that this function is normally
#'   called for its side effects.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # check available packages
#' db <- available.packages()
#'
#' # wait some time (suppose packages are uploaded / changed in this time)
#' Sys.sleep(5)
#'
#' # refresh the local available packages database
#' # (the old locally cached db will be removed)
#' db <- renv::refresh()
#'
#' }
refresh <- function() {

  pkgtype <- getOption("pkgType", default = "source")

  srcok <- pkgtype %in% c("both", "source") ||
    getOption("install.packages.check.source", default = "yes") %in% "yes"

  binok <- pkgtype %in% "both" ||
    grepl("binary", pkgtype, fixed = TRUE)

  list(
    binary = if (binok) available_packages(type = "binary", limit = 0L),
    source = if (srcok) available_packages(type = "source", limit = 0L)
  )

}


# regexps.R ------------------------------------------------------------------


renv_regexps_package_name <- function() {
  paste0("^", .standard_regexps()$valid_package_name, "$")
}

renv_regexps_package_version <- function() {
  paste0("^", .standard_regexps()$valid_package_version, "$")
}

renv_regexps_escape <- function(regexp) {
  pattern <- "([\\-\\[\\]\\{\\}\\(\\)\\*\\+\\?\\.\\,\\\\\\^\\$\\|\\#\\s])"
  gsub(pattern, "\\\\\\1", regexp, perl = TRUE)
}

renv_regexps_join <- function(regexps, capture = TRUE) {
  fmt <- if (capture) "(%s)" else "(?:%s)"
  sprintf(fmt, paste(regexps, collapse = "|"))
}


# rehash.R -------------------------------------------------------------------


#' Re-hash packages in the renv cache
#'
#' Re-hash packages in the renv cache, ensuring that any previously-cached
#' packages are copied to a new cache location appropriate for this version of
#' renv. This can be useful if the cache scheme has changed in a new version
#' of renv, but you'd like to preserve your previously-cached packages.
#'
#' Any packages which are re-hashed will retain links to the location of the
#' newly-hashed package, ensuring that prior installations of renv can still
#' function as expected.
#'
#' @inheritParams renv-params
#'
#' @export
rehash <- function(prompt = interactive(), ...) {
  renv_scope_error_handler()
  renv_dots_check(...)
  renv_scope_verbose_if(prompt)
  invisible(renv_rehash_impl(prompt))
}

renv_rehash_impl <- function(prompt) {

  # check for cache migration
  oldcache <- renv_paths_cache(version = renv_cache_version_previous())[[1L]]
  newcache <- renv_paths_cache(version = renv_cache_version())[[1L]]
  if (file.exists(oldcache) && !file.exists(newcache))
    renv_rehash_cache(oldcache, prompt, renv_file_copy, "copied")

  # re-cache packages as necessary
  renv_rehash_cache(newcache, prompt, renv_file_move, "moved")

}

renv_rehash_cache <- function(cache, prompt, action, label) {

  # re-compute package hashes
  old <- renv_cache_list(cache = cache)

  printf("- Re-computing package hashes ... ")
  new <- map_chr(old, renv_progress_callback(renv_cache_path, length(old)))
  writef("Done!")

  changed <- which(old != new & file.exists(old) & !file.exists(new))
  if (empty(changed)) {
    writef("- Your cache is already up-to-date -- nothing to do.")
    return(TRUE)
  }

  if (prompt) {

    fmt <- "%s [%s -> %s]"
    packages <- basename(old)[changed]
    oldhash <- renv_path_component(old[changed], 2L)
    newhash <- renv_path_component(new[changed], 2L)
    caution_bullets(
      "The following packages will be re-cached:",
      sprintf(fmt, format(packages), format(oldhash), format(newhash)),
      sprintf("Packages will be %s to their new locations in the cache.", label)
    )

    cancel_if(prompt && !proceed())

  }

  sources <- old[changed]
  targets <- new[changed]
  names(sources) <- targets
  names(targets) <- sources

  printf("- Re-caching packages ... ")
  enumerate(targets, renv_progress_callback(action, length(targets)))
  writef("Done!")

  n <- length(targets)
  fmt <- "Successfully re-cached %s."
  writef(fmt, nplural("package", n))

  renv_cache_clean_empty()

  TRUE
}


# release.R ------------------------------------------------------------------


renv_release_preflight <- function() {

  ok <- all(
    renv_release_preflight_urlcheck()
  )

  if (!ok)
    stop("one or more pre-flight release checks failed")

  ok

}

renv_release_preflight_urlcheck <- function() {

  # check for bad URLs
  urlchecker <- renv_namespace_load("urlchecker")
  result <- urlchecker$url_check()

  # report to user
  print(result)

  # return success
  nrow(result) == 0L

}


# remotes.R ------------------------------------------------------------------


#' Resolve a Remote
#'
#' Given a remote specification, resolve it into an renv package record that
#' can be used for download and installation (e.g. with [install]).
#'
#' @param spec A remote specification. This should be a string, conforming
#'   to the Remotes specification as defined in
#'   <https://remotes.r-lib.org/articles/dependencies.html>.
#'
remote <- function(spec) {
  renv_scope_error_handler()
  renv_remotes_resolve(spec)
}

# take a short-form remotes spec, parse that into a remote,
# and generate a corresponding package record
renv_remotes_resolve <- function(spec, latest = FALSE) {

  # check for already-resolved specs
  if (is.null(spec) || is.list(spec))
    return(spec)

  # remove a trailing slash
  # https://github.com/rstudio/renv/issues/1135
  spec <- gsub("/+$", "", spec, perl = TRUE)

  # check for archive URLs -- this is a bit hacky
  if (grepl("^(?:file|https?)://", spec)) {
    for (suffix in c(".zip", ".tar.gz", ".tgz", "/tarball"))
      if (endswith(spec, suffix))
        return(renv_remotes_resolve_url(spec, quiet = TRUE))
  }

  # remove github prefix
  spec <- gsub("^https?://(?:www\\.)?github\\.com/", "", spec)

  # check for paths to existing local files
  first <- substring(spec, 1L, 1L)
  local <- first %in% c("~", "/", ".") || renv_path_absolute(spec)

  if (local) {
    record <- catch(renv_remotes_resolve_path(spec))
    if (!inherits(record, "error"))
      return(record)
  }

  # define error handler (tag error with extra context when possible)
  error <- function(e) {

    # build error message
    fmt <- "failed to resolve remote '%s'"
    prefix <- sprintf(fmt, spec)
    message <- paste(prefix, e$message, sep = " -- ")

    # otherwise, propagate the error
    stop(simpleError(message = message, call = e$call))

  }

  # attempt the parse
  withCallingHandlers(
    renv_remotes_resolve_impl(spec, latest),
    error = error
  )

}

renv_remotes_resolve_impl <- function(spec, latest = FALSE) {

  remote <- renv_remotes_parse(spec)

  # fixup for bioconductor
  isbioc <-
    identical(remote$type, "repository") &&
    identical(remote$repository, "bioc")

  if (isbioc)
    remote$type <- "bioc"

  resolved <- switch(
    remote$type,
    bioc       = renv_remotes_resolve_bioc(remote),
    bitbucket  = renv_remotes_resolve_bitbucket(remote),
    gitlab     = renv_remotes_resolve_gitlab(remote),
    github     = renv_remotes_resolve_github(remote),
    repository = renv_remotes_resolve_repository(remote, latest),
    git        = renv_remotes_resolve_git(remote),
    url        = renv_remotes_resolve_url(remote$url, quiet = TRUE),
    stopf("unknown remote type '%s'", remote$type %||% "<NA>")
  )

  # ensure that attributes on the record are preserved, but drop NULL entries
  for (key in names(resolved))
    if (is.null(resolved[[key]]))
      resolved[[key]] <- NULL

  resolved

}

renv_remotes_parse_impl <- function(spec, pattern, fields, perl = FALSE) {

  matches <- regexec(pattern, spec, perl = perl)
  strings <- regmatches(spec, matches)[[1]]
  if (empty(strings))
    stopf("'%s' is not a valid remote", spec)

  if (length(fields) != length(strings))
    stop("internal error: field length mismatch in renv_remotes_parse_impl")

  names(strings) <- fields
  remote <- as.list(strings)
  lapply(remote, function(item) if (nzchar(item)) item)

}

renv_remotes_parse_repos <- function(spec) {

  pattern <- paste0(
    "^",                                           # start
    "(?:([^:]+)::)?",                              # optional repository name
    "([[:alnum:].]+)",                             # package name
    "(?:@([[:digit:]_.-]+))?",                     # optional package version
    "$"
  )

  fields <- c("spec", "repository", "package", "version")
  renv_remotes_parse_impl(spec, pattern, fields)

}

renv_remotes_parse_remote <- function(spec) {

  pattern <- paste0(
    "^",
    "(?:([[:alpha:]][[:alnum:].]*[[:alnum:]])=)?",  # optional package name
    "(?:([^@:]+)(?:@([^:]+))?::)?",                 # optional prefix, providing type + host
    "([^/#@:]+)",                                   # a username
    "(?:/([^@#:]+))?",                              # a repository (allow sub-repositories)
    "(?::([^@#:]+))?",                              # optional subdirectory
    "(?:#([^@#:]+))?",                              # optional hash (e.g. pull request)
    "(?:@([^@#:]+))?",                              # optional ref (e.g. branch or commit)
    "$"
  )

  fields <- c(
    "spec", "package", "type",
    "host", "user", "repo",
    "subdir", "pull", "ref"
  )

  remote <- renv_remotes_parse_impl(spec, pattern, fields)
  if (!nzchar(remote$repo))
    stopf("'%s' is not a valid remote", spec)

  renv_remotes_parse_finalize(remote)

}

renv_remotes_parse_gitssh <- function(spec) {

  pattern <- paste0(
    "^",
    "(?:([[:alpha:]][[:alnum:].]*[[:alnum:]])=)?",  # optional package name
    "(?:(git)::)?",                                 # optional git prefix
    "(",                                            # url start
      "([^@]+)@",                                   # user (typically, 'git')
      "([^:]+):",                                   # host
      "([^:#@]+)",                                  # the rest of the repo url
    ")",                                            # url end
    "(?::([^@#:]+))?",                              # optional sub-directory
    "(?:#([^@#:]+))?",                              # optional hash (e.g. pull request)
    "(?:@([^@#:]+))?",                              # optional ref (e.g. branch or commit)
    "$"
  )

  fields <- c(
    "spec", "package", "type", "url",
    "user", "host", "repo",
    "subdir", "pull", "ref"
  )

  remote <- renv_remotes_parse_impl(spec, pattern, fields, perl = TRUE)
  if (!nzchar(remote$repo))
    stopf("'%s' is not a valid remote", spec)

  remote$type <- remote$type %||% "git"
  renv_remotes_parse_finalize(remote)

}

renv_remotes_parse_git <- function(spec) {

  hostpattern <- paste0(
    "(",
      "(?:(?:(?!-))(?:xn--|_{1,1})?[a-z0-9-]{0,61}[a-z0-9]{1,1}\\.)*",
      "(?:xn--)?",
      "(?:[a-z0-9][a-z0-9\\-]{0,60}|[a-z0-9-]{1,30}\\.[a-z]{2,})",
    ")"
  )

  pattern <- paste0(
    "^",
    "(?:([[:alpha:]][[:alnum:].]*[[:alnum:]])=)?",  # optional package name
    "(?:(git)::)?",                                 # optional git prefix
    "(",                                            # URL start
      "(?:(https?|git|ssh)://)?",                   #   protocol
      "(?:([^@]+)@)?",                              #   login (probably git)
      hostpattern,                                  #   host
      "[/:]([\\w_.-]+)",                            #   a username
      "(?:/([^@#:]+?))?",                           #   a repository (allow sub-repositories)
      "(?:\\.(git))?",                              #   optional .git extension
    ")",                                            # URL end
    "(?::([^@#:]+))?",                              # optional sub-directory
    "(?:#([^@#:]+))?",                              # optional hash (e.g. pull request)
    "(?:@([^@#:]+))?",                              # optional ref (e.g. branch or commit)
    "$"
  )

  fields <- c(
    "spec", "package", "type",
    "url", "protocol", "login", "host", "user", "repo", "ext",
    "subdir", "pull", "ref"
  )

  remote <- renv_remotes_parse_impl(spec, pattern, fields, perl = TRUE)
  if (!nzchar(remote$repo))
    stopf("'%s' is not a valid remote", spec)

  # If type has not been found & repo looks like a git repo, set it as git
  # (note that this parser also accepts entries which are not truly git
  # references, so we try to "fix up" after the fact)
  if ("git" %in% c(remote$login, remote$type, remote$ext, remote$protocol))
    remote$type <- tolower(remote$type %||% "git")

  renv_remotes_parse_finalize(remote)

}

# NOTE: to avoid ambiguity with git remote specs, we require URL
# remotes to begin with a 'url::' prefix
renv_remotes_parse_url <- function(spec) {

  pattern <- paste0(
    "^",
    "(?:([[:alpha:]][[:alnum:].]*[[:alnum:]])=)?",  # optional package name
    "(url)::",                                      # type (required for URL remotes)
    "((https?)://([^:]+))",                         # url, protocol, path
    "(?::([^@#:]+))?",                              # optional subdir
    "$"
  )

  fields <- c("spec", "package", "type", "url", "protocol", "path", "subdir")
  remote <- renv_remotes_parse_impl(spec, pattern, fields, perl = TRUE)
  if (!nzchar(remote$url))
    stopf("'%s' is not a valid remote", spec)

  renv_remotes_parse_finalize(remote)
}

renv_remotes_parse_finalize <- function(remote) {

  # default remote type is github
  remote$type <- tolower(remote$type %||% "github")

  # custom finalization for different remote types
  case(
    remote$type == "github" ~ renv_remotes_parse_finalize_github(remote),
    TRUE                    ~ remote
  )

}

renv_remotes_parse_finalize_github <- function(remote) {

  # split repo spec into pieces
  repo <- remote$repo %||% ""
  parts <- strsplit(repo, "/", fixed = TRUE)[[1]]
  if (length(parts) < 2)
    return(remote)

  # form subdir from tail of repo
  remote$repo   <- paste(head(parts, n = 1L),  collapse = "/")
  remote$subdir <- paste(tail(parts, n = -1L), collapse = "/")

  # return modified remote
  remote

}

renv_remotes_parse <- function(spec) {

  remote <- catch(renv_remotes_parse_repos(spec))
  if (!inherits(remote, "error")) {
    remote$type <- "repository"
    return(remote)
  }

  remote <- catch(renv_remotes_parse_remote(spec))
  if (!inherits(remote, "error")) {
    remote$type <- remote$type %||% "github"
    return(remote)
  }

  remote <- catch(renv_remotes_parse_gitssh(spec))
  if (!inherits(remote, "error")) {
    remote$type <- remote$type %||% "git"
    return(remote)
  }

  remote <- catch(renv_remotes_parse_url(spec))
  if (!inherits(remote, "error")) {
    remote$type <- remote$type %||% "url"
    return(remote)
  }

  remote <- catch(renv_remotes_parse_git(spec))
  if (!inherits(remote, "error")) {
    remote$type <- remote$type %||% "git"
    return(remote)
  }

  stopf("failed to parse remote spec '%s'", spec)

}

renv_remotes_resolve_bioc_version <- function(version) {

  # initialize Bioconductor
  renv_bioconductor_init()
  BiocManager <- renv_scope_biocmanager()

  # handle versions like 'release' and 'devel'
  versions <- BiocManager$.version_map()
  row <- versions[versions$BiocStatus == version, ]
  if (nrow(row))
    return(row$Bioc)

  # otherwise, use the default version
  BiocManager$version()

}

renv_remotes_resolve_bioc_plain <- function(remote) {

  list(
    Package = remote$package,
    Version = remote$version,
    Source  = "Bioconductor"
  )

}

renv_remotes_resolve_bioc <- function(remote) {

  # if we parsed this as a repository remote, use that directly
  if (!is.null(remote$package))
    return(renv_remotes_resolve_bioc_plain(remote))

  # otherwise, this was parsed as a regular remote, declaring the package
  # should be obtained from a particular Bioconductor release
  package <- remote$repo
  biocversion <- renv_remotes_resolve_bioc_version(remote$user)
  biocrepos <- renv_bioconductor_repos(version = biocversion)
  record <- renv_available_packages_latest(package, repos = biocrepos)

  # update fields
  record$Source <- "Bioconductor"
  record$Repository <- NULL

  # return the resolved record
  record

}

renv_remotes_resolve_bitbucket <- function(remote) {

  user   <- remote$user
  repo   <- remote$repo
  subdir <- remote$subdir
  ref    <- remote$ref %||% getOption("renv.bitbucket.default_branch", "master")

  host <- remote$host %||% config$bitbucket.host()

  # scope authentication
  renv_scope_auth(repo)

  # get commit sha for ref
  fmt <- "%s/repositories/%s/%s/commit/%s"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo, ref)

  destfile <- renv_scope_tempfile("renv-bitbucket-")
  download(url, destfile = destfile, type = "bitbucket", quiet = TRUE)
  json <- renv_json_read(file = destfile)
  sha <- json$hash

  # get DESCRIPTION file
  fmt <- "%s/repositories/%s/%s/src/%s/DESCRIPTION"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo, ref)

  destfile <- renv_scope_tempfile("renv-description-")
  download(url, destfile = destfile, type = "bitbucket", quiet = TRUE)
  desc <- renv_dcf_read(destfile)

  list(
    Package        = desc$Package,
    Version        = desc$Version,
    Source         = "Bitbucket",
    RemoteType     = "bitbucket",
    RemoteHost     = host,
    RemoteUsername = user,
    RemoteRepo     = repo,
    RemoteSubdir   = subdir,
    RemoteRef      = ref,
    RemoteSha      = sha
  )

}

renv_remotes_resolve_repository <- function(remote, latest) {

  package <- remote$package
  if (package %in% renv_packages_base())
    return(renv_remotes_resolve_base(package))

  version <- remote$version
  repository <- remote$repository

  if (latest && is.null(version)) {
    remote <- renv_available_packages_latest(package)
    version <- remote$Version
  }

  list(
    Package    = package,
    Version    = version,
    Source     = "Repository",
    Repository = repository
  )

}

renv_remotes_resolve_base <- function(package) {

  list(
    Package = package,
    Version = renv_package_version(package),
    Source  = "R"
  )

}

renv_remotes_resolve_github_sha_pull <- function(host, user, repo, pull) {

  # scope authentication
  renv_scope_auth(repo)

  # make request
  fmt <- "%s/repos/%s/%s/pulls/%s"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo, pull)
  jsonfile <- renv_scope_tempfile("renv-json-")
  download(url, destfile = jsonfile, type = "github", quiet = TRUE)

  # read resulting JSON
  json <- renv_json_read(jsonfile)
  json$head$sha

}

renv_remotes_resolve_github_sha_ref <- function(host, user, repo, ref) {

  # scope authentication
  renv_scope_auth(repo)

  # build url for github commits endpoint
  fmt <- "%s/repos/%s/%s/commits/%s"
  origin <- renv_retrieve_origin(host)
  ref <- ref %||% getOption("renv.github.default_branch", default = "master")
  url <- sprintf(fmt, origin, user, repo, ref %||% "master")

  # prepare headers
  headers <- c(Accept = "application/vnd.github.sha")

  # make request to endpoint
  shafile <- renv_scope_tempfile("renv-sha-")
  download(
    url,
    destfile = shafile,
    type = "github",
    quiet = TRUE,
    headers = headers
  )

  # read downloaded content
  sha <- renv_file_read(shafile)

  # check for JSON response (in case our headers weren't sent)
  if (nchar(sha) > 40L) {
    json <- renv_json_read(text = sha)
    sha <- json$sha
  }

  sha

}

renv_remotes_resolve_github_modules <- function(host, user, repo, subdir, sha) {

  # form path to .gitmodules file
  subdir <- subdir %||% ""
  parts <- c(
    if (nzchar(subdir)) URLencode(subdir),
    ".gitmodules"
  )

  path <- paste(parts, collapse = "/")

  # scope authentication
  renv_scope_auth(repo)

  # add headers
  headers <- c(Accept = "application/vnd.github.raw")

  # get the file contents
  fmt <- "%s/repos/%s/%s/contents/%s?ref=%s"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo, path, sha)
  jsonfile <- renv_scope_tempfile("renv-json-")
  status <- suppressWarnings(
    catch(
      download(url, destfile = jsonfile, type = "github", quiet = TRUE, headers = headers)
    )
  )

  # just return a status code whether or not submodules are included
  !inherits(status, "error")

}

renv_remotes_resolve_github_description <- function(host, user, repo, subdir, sha) {

  # form DESCRIPTION path
  subdir <- subdir %||% ""
  parts <- c(
    if (nzchar(subdir)) URLencode(subdir),
    "DESCRIPTION"
  )

  descpath <- paste(parts, collapse = "/")

  # scope authentication
  renv_scope_auth(repo)

  # add headers
  headers <- c(
    Accept = "application/vnd.github.raw",
    renv_download_auth_github()
  )

  # get the DESCRIPTION contents
  fmt <- "%s/repos/%s/%s/contents/%s?ref=%s"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo, descpath, sha)
  destfile <- renv_scope_tempfile("renv-json-")
  download(url, destfile = destfile, type = "github", quiet = TRUE, headers = headers)

  # try to read the file; detect JSON versus raw content in case
  # headers were not sent for some reason
  contents <- renv_file_read(destfile)
  if (substring(contents, 1L, 1L) == "{") {
    json <- renv_json_read(text = contents)
    contents <- renv_base64_decode(json$content)
  }

  # normalize newlines
  contents <- gsub("\r\n", "\n", contents, fixed = TRUE)

  # read as DCF
  renv_dcf_read(text = contents)

}

renv_remotes_resolve_github_ref <- function(host, user, repo) {

  tryCatch(
    renv_remotes_resolve_github_ref_impl(host, user, repo),
    error = function(e) {
      warning(e)
      getOption("renv.github.default_branch", default = "master")
    }
  )

}

renv_remotes_resolve_github_ref_impl <- function(host, user, repo) {

  # scope authentication
  renv_scope_auth(repo)

  # build url to repos endpoint
  fmt <- "%s/repos/%s/%s"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo)

  # download JSON data at endpoint
  jsonfile <- renv_scope_tempfile("renv-github-ref-", fileext = ".json")
  download(url, destfile = jsonfile, type = "github", quiet = TRUE)
  json <- renv_json_read(jsonfile)

  # read default branch
  json$default_branch %||% getOption("renv.github.default_branch", default = "master")

}

renv_remotes_resolve_github <- function(remote) {

  # resolve the reference associated with this repository
  host   <- remote$host %||% config$github.host()
  user   <- remote$user
  repo   <- remote$repo
  spec   <- remote$spec
  subdir <- remote$subdir

  # resolve ref
  ref <- remote$ref %||% renv_remotes_resolve_github_ref(host, user, repo)

  # handle '*release' refs
  if (identical(ref, "*release"))
    ref <- renv_remotes_resolve_github_release(host, user, repo, spec)

  # resolve the sha associated with the ref / pull
  pull   <- remote$pull %||% ""
  sha <- case(
    nzchar(pull) ~ renv_remotes_resolve_github_sha_pull(host, user, repo, pull),
    nzchar(ref)  ~ renv_remotes_resolve_github_sha_ref(host, user, repo, ref)
  )

  # if an abbreviated sha was provided as the ref, expand it here
  if (nzchar(ref) && startswith(sha, ref))
    ref <- sha

  # check whether the repository has a .gitmodules file; if so, then we'll have
  # to use a plain 'git' client to retrieve the package
  modules <- renv_remotes_resolve_github_modules(host, user, repo, subdir, sha)
  url <- if (modules) {
    origin <- fsub("api.github.com", "github.com", renv_retrieve_origin(host))
    parts <- c(origin, user, repo)
    paste(parts, collapse = "/")
  }

  # read DESCRIPTION
  desc <- renv_remotes_resolve_github_description(host, user, repo, subdir, sha)

  list(
    Package        = desc$Package,
    Version        = desc$Version,
    Source         = if (modules) "git" else "GitHub",
    RemoteType     = if (modules) "git" else "github",
    RemoteUrl      = if (modules) url,
    RemoteHost     = host,
    RemoteUsername = user,
    RemoteRepo     = repo,
    RemoteSubdir   = subdir,
    RemoteRef      = ref,
    RemoteSha      = sha
  )

}

renv_remotes_resolve_github_release <- function(host, user, repo, spec) {

  # scope authentication
  renv_scope_auth(repo)

  # build url for github releases endpoint
  fmt <- "%s/repos/%s/%s/releases?per_page=1"
  origin <- renv_retrieve_origin(host)
  url <- sprintf(fmt, origin, user, repo)

  # prepare headers
  headers <- c(Accept = "application/vnd.github.raw+json")

  # make request to endpoint
  releases <- renv_scope_tempfile("renv-releases-")
  download(
    url      = url,
    destfile = releases,
    type     = "github",
    quiet    = TRUE,
    headers  = headers
  )

  # get reference associated with this tag
  json <- renv_json_read(releases)
  if (empty(json)) {
    fmt <- "could not find any releases associated with remote '%s'"
    stopf(fmt, sub("[*]release$", "", spec))
  }

  json[[1L]][["tag_name"]]

}

renv_remotes_resolve_git <- function(remote) {

  package <- remote$package %||% basename(remote$repo)
  url     <- remote$url
  subdir  <- remote$subdir

  # handle git ref
  pull <- remote$pull %||% ""
  ref  <- remote$ref %||% ""

  # resolve ref from pull if set
  if (nzchar(pull))
    ref <- renv_remotes_resolve_git_pull(ref)

  record <- list(
    Package        = package,
    Version        = "<unknown>",
    Source         = "git",
    RemoteType     = "git",
    RemoteUrl      = url,
    RemoteSubdir   = subdir,
    RemoteRef      = ref
  )

  desc <- renv_remotes_resolve_git_description(record)

  record$Package <- desc$Package
  record$Version <- desc$Version

  record
}


renv_remotes_resolve_git_sha_ref <- function(record) {

  renv_git_preflight()

  origin <- record$RemoteUrl
  ref <- record$RemoteRef %||% record$RemoteSha
  args <- c("ls-remote", origin, ref)

  output <- local({
    renv_scope_auth(record)
    renv_scope_git_auth()
    renv_system_exec("git", args, "checking git remote")
  })

  if (empty(output))
    return("")

  # format of output is, for example:
  #
  #   $ git ls-remote https://github.com/rstudio/renv refs/tags/0.14.0
  #   20ca74bdcc3c87848e5665effa2fc8ee8b039c69        refs/tags/0.14.0
  #
  # take first line of output, split on tab character, and take leftmost entry
  strsplit(output[[1L]], "\t", fixed = TRUE)[[1L]][[1L]]

}


renv_remotes_resolve_git_description <- function(record) {

  path <- renv_scope_tempfile("renv-git-")
  ensure_directory(path)

  # TODO: is there a cheaper way for us to accomplish this?
  # it'd be nice if we could retrieve the contents of a single
  # file, without needing to pull an entire repository branch
  local({
    renv_scope_options(renv.verbose = FALSE)
    renv_retrieve_git_impl(record, path)
  })

  # subdir may be NULL
  subdir <- record$RemoteSubdir
  desc <- renv_description_read(path, subdir = subdir)

  desc
}

renv_remotes_resolve_git_pull <- function(pr) {
  # to be able to checkout PR 760:
  # git fetch origin pull/760/head:pr-760
  # or:
  # git fetch origin pull/760/head:pull/760

  # so format for ref is:
  # pull/{ref_number}/head:pr-{ref_number}
  fmt <- "pull/%s/head:pull/%s"

  remote_ref <- sprintf(fmt, pr, pr)
  remote_ref
}

renv_remotes_resolve_gitlab_ref <- function(host, user, repo) {

  tryCatch(
    renv_remotes_resolve_gitlab_ref_impl(host, user, repo),
    error = function(e) {
      warning(e)
      getOption("renv.gitlab.default_branch", default = "master")
    }
  )

}

renv_remotes_resolve_gitlab_ref_impl <- function(host, user, repo) {

  # scope authentication
  renv_scope_auth(repo)

  # get list of available branches
  fmt <- "%s/api/v4/projects/%s/repository/branches"
  origin <- renv_retrieve_origin(host)
  id <- URLencode(paste(user, repo, sep = "/"), reserved = TRUE)
  url <- sprintf(fmt, origin, id)

  destfile <- renv_scope_tempfile("renv-gitlab-commits-")
  download(url, destfile = destfile, type = "gitlab", quiet = TRUE)
  json <- renv_json_read(file = destfile)

  # iterate through and find the default
  for (info in json)
    if (identical(info$default, TRUE))
      return(info$name)

  # if no default was found, use master branch
  # (for backwards compatibility with existing projects)
  getOption("renv.gitlab.default_branch", default = "master")

}

renv_remotes_resolve_gitlab <- function(remote) {

  host   <- remote$host %||% config$gitlab.host()
  user   <- remote$user
  repo   <- remote$repo
  subdir <- remote$subdir %||% ""

  ref <- remote$ref %||% renv_remotes_resolve_gitlab_ref(host, user, repo)

  parts <- c(if (nzchar(subdir)) subdir, "DESCRIPTION")
  descpath <- URLencode(paste(parts, collapse = "/"), reserved = TRUE)

  # scope authentication
  renv_scope_auth(repo)

  # retrieve sha associated with this ref
  fmt <- "%s/api/v4/projects/%s/repository/commits/%s"
  origin <- renv_retrieve_origin(host)
  id <- URLencode(paste(user, repo, sep = "/"), reserved = TRUE)
  ref <- URLencode(ref, reserved = TRUE)
  url <- sprintf(fmt, origin, id, ref)

  destfile <- renv_scope_tempfile("renv-gitlab-commits-")
  download(url, destfile = destfile, type = "gitlab", quiet = TRUE)
  json <- renv_json_read(file = destfile)
  sha <- json$id

  # retrieve DESCRIPTION file
  fmt <- "%s/api/v4/projects/%s/repository/files/%s/raw?ref=%s"
  origin <- renv_retrieve_origin(host)
  id <- URLencode(paste(user, repo, sep = "/"), reserved = TRUE)
  url <- sprintf(fmt, origin, id, descpath, ref)

  destfile <- renv_scope_tempfile("renv-description-")
  download(url, destfile = destfile, type = "gitlab", quiet = TRUE)
  desc <- renv_dcf_read(destfile)

  list(
    Package        = desc$Package,
    Version        = desc$Version,
    Source         = "GitLab",
    RemoteType     = "gitlab",
    RemoteHost     = host,
    RemoteUsername = user,
    RemoteRepo     = repo,
    RemoteSubdir   = subdir,
    RemoteRef      = ref,
    RemoteSha      = sha
  )

}

renv_remotes_resolve_url <- function(url, quiet = FALSE) {

  tempfile <- renv_scope_tempfile("renv-url-")
  writeLines(url, con = tempfile)
  hash <- tools::md5sum(tempfile)

  ext <- fileext(url, default = ".tar.gz")
  name <- paste(hash, ext, sep = "")
  path <- renv_paths_source("url", name)

  ensure_parent_directory(path)
  download(url, path, quiet = quiet)

  desc <- renv_description_read(path)

  list(
    Package    = desc$Package,
    Version    = desc$Version,
    Source     = "URL",
    RemoteType = "url",
    RemoteUrl  = url,
    Path       = path
  )

}

renv_remotes_resolve_path <- function(path) {

  # if this package lives within one of the cellar paths,
  # then treat it as a cellar source
  roots <- renv_cellar_roots()
  for (root in roots)
    if (renv_path_within(path, root))
      return(renv_remotes_resolve_path_cellar(path))

  # first, check for a common extension
  if (renv_archive_type(path) %in% c("tar", "zip"))
    return(renv_remotes_resolve_path_impl(path))

  # otherwise, if this is the path to a package project, use the sources as-is
  if (renv_project_type(path) == "package")
    return(renv_remotes_resolve_path_impl(path))

  stopf("there is no package at path '%s'", path)

}

renv_remotes_resolve_path_cellar <- function(path) {

  desc <- renv_description_read(path)
  list(
    Package    = desc$Package,
    Version    = desc$Version,
    Source     = "Cellar",
    Cacheable  = FALSE
  )

}

renv_remotes_resolve_path_impl <- function(path) {

  desc <- renv_description_read(path)
  list(
    Package    = desc$Package,
    Version    = desc$Version,
    Source     = "Local",
    RemoteType = "local",
    RemoteUrl  = path,
    Cacheable  = FALSE
  )

}


# remove.R -------------------------------------------------------------------


#' Remove packages
#'
#' Remove (uninstall) \R packages.
#'
#' @inherit renv-params
#'
#' @param packages A character vector of \R packages to remove.
#' @param library The library from which packages should be removed. When
#'   `NULL`, the active library (that is, the first entry reported in
#'   `.libPaths()`) is used instead.
#'
#' @return A vector of package records, describing the packages (if any) which
#'   were successfully removed.
#'
#' @export
#'
#' @example examples/examples-init.R
remove <- function(packages,
                   ...,
                   library = NULL,
                   project = NULL)
{
  renv_scope_error_handler()
  renv_dots_check(...)

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  library <- renv_path_normalize(library %||% renv_libpaths_active())

  # NOTE: users might request that we remove packages which aren't currently
  # installed, so we need to catch errors when trying to snapshot those packages
  descpaths <- file.path(library, packages, "DESCRIPTION")
  records <- lapply(descpaths, compose(catch, renv_snapshot_description))
  names(records) <- packages
  records <- Filter(function(record) !inherits(record, "error"), records)

  if (library == renv_paths_library(project = project)) {
    writef("- Removing package(s) from project library ...")
  } else {
    fmt <- "- Removing package(s) from library '%s' ..."
    writef(fmt, renv_path_aliased(library))
  }

  if (length(packages) == 1) {
    renv_remove_impl(packages, library)
    return(invisible(records))
  }

  count <- 0
  for (package in packages) {
    if (renv_remove_impl(package, library))
      count <- count + 1
  }

  writef("- Done! Removed %s.", nplural("package", count))
  invisible(records)
}

renv_remove_impl <- function(package, library) {

  path <- file.path(library, package)
  if (!renv_file_exists(path)) {
    writef("- Package '%s' is not installed -- nothing to do.", package)
    return(FALSE)
  }

  recursive <- renv_file_type(path) == "directory"
  printf("Removing package '%s' ... ", package)
  unlink(path, recursive = recursive)
  writef("Done!")

  TRUE

}


# renv-package.R -------------------------------------------------------------


#' Project-local Environments for R
#'
#' Project-local environments for \R.
#'
#' You can use renv to construct isolated, project-local \R libraries.
#' Each project using renv will share package installations from a global
#' cache of packages, helping to avoid wasting disk space on multiple
#' installations of a package that might otherwise be shared across projects.
#'
"_PACKAGE"


# renvignore.R ---------------------------------------------------------------


# given a path within a project, read all relevant ignore files
# and generate a pattern that can be used to filter file results
renv_renvignore_pattern <- function(path = getwd(), root = path) {

  if (is.null(root))
    return(NULL)

  stopifnot(
    renv_path_absolute(path),
    renv_path_absolute(root)
  )

  # prepare ignores
  ignores <- stack()

  # read ignore files
  parent <- path
  while (parent != dirname(parent)) {

    # attempt to read either .renvignore or .gitignore
    for (file in c(".renvignore", ".gitignore")) {
      candidate <- file.path(parent, file)
      if (file.exists(candidate)) {
        contents <- readLines(candidate, warn = FALSE)
        parsed <- renv_renvignore_parse(contents, parent)
        if (length(parsed))
          ignores$push(parsed)
        break
      }
    }

    # stop once we've hit the project root
    if (parent == root)
      break

    parent <- dirname(parent)

  }

  # collect patterns read
  patterns <- ignores$data()

  # separate exclusions, exclusions
  include <- unlist(extract(patterns, "include"))
  exclude <- unlist(extract(patterns, "exclude"))

  # allow for inclusion / exclusion via option
  # (primarily intended for internal use with packrat)
  include <- c(include, renv_renvignore_pattern_extra("include", root))
  exclude <- c(exclude, renv_renvignore_pattern_extra("exclude", root))

  # ignore hidden directories by default
  exclude <- c("/[.][^/]*/$", exclude)

  list(include = include, exclude = exclude)

}

# reads a .gitignore / .renvignore file, and translates the associated
# entries into PCREs which can be combined and used during directory traversal
renv_renvignore_parse <- function(contents, prefix = "") {

  # read the ignore entries
  contents <- grep("^\\s*(?:#|$)", contents, value = TRUE, invert = TRUE)
  if (empty(contents))
    return(list())

  # split into inclusion, exclusion patterns
  negate <- substring(contents, 1L, 1L) == "!"
  exclude <- contents[!negate]
  include <- substring(contents[negate], 2L)

  # For include rules, if we're explicitly including a file within
  # a sub-directory, then we need to force all parent directories
  # to also be included. In other words, a rule like:
  #
  #    !a/b/c
  #
  # needs to be implicitly treated like
  #
  #    !/a
  #    !/a/b
  #    !/a/b/c
  #
  # so we perform that transformation here.
  #
  # Note that this isn't perfect; for example, with the .gitignore file
  #
  #    dir
  #    !dir/matched
  #
  # The exclusion of 'dir' will take precedence, and dir/matched won't
  # get a chance to apply.
  include <- sort(unique(unlist(map(include, function(rule) {
    idx <- gregexpr("(?:/|$)", rule, perl = TRUE)[[1L]]
    gsub("^/*", "/", substring(rule, 1L, idx))
  }))))

  # parse patterns separately
  list(
    exclude = renv_renvignore_parse_impl(exclude, prefix),
    include = renv_renvignore_parse_impl(include, prefix)
  )

}

renv_renvignore_parse_impl <- function(entries, prefix = "") {

  # check for empty entries list
  if (empty(entries))
    return(character())

  # remove trailing whitespace
  entries <- gsub("\\s+$", "", entries)

  # entries without a slash (other than a trailing one) should match in tree
  noslash <- grep("/", gsub("/*$", "", entries), fixed = TRUE, invert = TRUE)
  entries[noslash] <- paste("**", entries[noslash], sep = "/")

  # remove a leading slash (avoid double-slashing)
  entries <- gsub("^/+", "", entries)

  # save any '**' entries seen
  entries <- gsub("**/",  "\001", entries, fixed = TRUE)
  entries <- gsub("/**",  "\002", entries, fixed = TRUE)

  # transform '*' and '?'
  entries <- gsub("*", "\\E[^/]*\\Q", entries, fixed = TRUE)
  entries <- gsub("?", "\\E[^/]\\Q",  entries, fixed = TRUE)

  # restore '**' entries
  entries <- gsub("\001", "\\E(?:.*/)?\\Q", entries, fixed = TRUE)
  entries <- gsub("\002", "/\\E.*\\Q",      entries, fixed = TRUE)

  # if we don't have a trailing slash, then we can match both files and dirs
  noslash <- grep("/$", entries, invert = TRUE)
  entries[noslash] <- paste0(entries[noslash], "\\E(?:/)?\\Q")

  # enclose in \\Q \\E to ensure e.g. plain '.' are not treated
  # as regex characters
  entries <- sprintf("\\Q%s\\E$", entries)

  # prepend prefix
  entries <- sprintf("^\\Q%s/\\E%s", prefix, entries)

  # remove \\Q\\E
  entries <- gsub("\\Q\\E", "", entries, fixed = TRUE)

  # all done!
  entries

}

renv_renvignore_exec <- function(path, root, children) {

  # the root directory is always included
  if (identical(root, children))
    return(FALSE)

  # compute exclusion patterns
  patterns <- renv_renvignore_pattern(path, root)

  # if we have no patterns, then we're not excluding anything
  if (empty(patterns) || empty(patterns$exclude))
    return(logical(length(children)))

  # append slashes to files which are directories
  info <- renv_file_info(children)
  dirs <- info$isdir %in% TRUE
  children[dirs] <- paste0(children[dirs], "/")

  # get the entries that need to be excluded
  excludes <- logical(length = length(children))
  for (pattern in patterns$exclude)
    if (nzchar(pattern))
      excludes <- excludes | grepl(pattern, children, perl = TRUE)

  if (length(patterns$include)) {

    # check for entries that should be explicitly included
    # (note that these override any excludes)
    includes <- logical(length = length(children))
    for (pattern in patterns$include)
      if (nzchar(pattern))
        includes <- includes | grepl(pattern, children, perl = TRUE)

    # unset those excludes
    excludes[includes] <- FALSE

  }

  # return vector of excludes
  excludes

}

renv_renvignore_pattern_extra <- function(key, root) {

  # check for value from option
  optname <- paste("renv.renvignore", key, sep = ".")
  patterns <- getOption(optname)
  if (is.null(patterns))
    return(NULL)

  # should we use the pattern as-is?
  asis <- attr(patterns, "asis", exact = TRUE)
  if (identical(asis, TRUE))
    return(patterns)

  # otherwise, process it as an .renvignore-style ignore
  root <- attr(patterns, "root", exact = TRUE) %||% root
  patterns <- renv_renvignore_parse(patterns, root)
  patterns[[key]]

}


# repair.R -------------------------------------------------------------------


#' Repair a project
#'
#' Use `repair()` to recover from some common issues that can occur with
#' a project. Currently, two operations are performed:
#'
#' 1. Packages with broken symlinks into the cache will be re-installed.
#'
#' 2. Packages that were installed from sources, but appear to be from
#'    an remote source (e.g. GitHub), will have their `DESCRIPTION` files
#'    updated to record that remote source explicitly.
#'
#' @inheritParams renv-params
#'
#' @param lockfile The path to a lockfile (if any). When available, renv
#'   will use the lockfile when attempting to infer the remote associated
#'   with the inaccessible version of each missing package. When `NULL`
#'   (the default), the project lockfile will be used.
#'
#' @export
repair <- function(library  = NULL,
                   lockfile = NULL,
                   project  = NULL)
{
  renv_consent_check()
  renv_scope_error_handler()

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  libpaths <- renv_path_normalize(library %||% renv_libpaths_all())
  library <- libpaths[[1L]]

  writef(header("Library cache links"))
  renv_repair_links(library, lockfile, project)
  writef()

  writef(header("Package sources"))
  renv_repair_sources(library, lockfile, project)
  writef()

  invisible()
}

renv_repair_links <- function(library, lockfile, project) {


  # figure out which library paths (junction points?) appear to be broken
  paths <- list.files(library, full.names = TRUE)
  broken <- renv_file_broken(paths)
  packages <- basename(paths[broken])
  if (empty(packages)) {
    writef("- No issues found with the project library's cache links.")
    return(invisible(packages))
  }

  # try to find records for these packages in the lockfile
  # TODO: what if one of the requested packages isn't in the lockfile?
  lockfile <- lockfile %||% renv_lockfile_load(project = project)
  records <- renv_repair_records(packages, lockfile, project)

  # install these records
  install(
    packages = records,
    library  = library,
    project  = project
  )

}

renv_repair_records <- function(packages, lockfile, project) {
  map(packages, function(package) {
    lockfile$Packages[[package]] %||% package
  })
}

renv_repair_sources <- function(library, lockfile, project) {

  # get package description files
  db <- installed_packages(lib.loc = library, priority = NA_character_)
  descpaths <- with(db, file.path(LibPath, Package, "DESCRIPTION"))
  dcfs <- map(descpaths, renv_description_read)
  names(dcfs) <- map_chr(dcfs, `[[`, "Package")

  # try to infer sources as necessary
  inferred <- map(dcfs, renv_repair_sources_infer)
  inferred <- filter(inferred, Negate(is.null))
  if (length(inferred) == 0L) {
    writef("- All installed packages appear to be from a known source.")
    return(TRUE)
  }

  # ask used
  renv_scope_options(renv.verbose = TRUE)
  caution_bullets(
    c(
      "The following package(s) do not have an explicitly-declared remote source.",
      "However, renv was available to infer remote sources from their DESCRIPTION file."
    ),
    sprintf("%s  [%s]", format(names(inferred)), inferred),
    "`renv::restore()` may fail for packages without an explicitly-declared remote source."
  )

  choice <- menu(

    choices =  c(
      update = "Let renv infer the remote sources for these packages.",
      cancel = "Do nothing and resolve the situation another way."
    ),

    title = "What would you like to do?"

  )

  cancel_if(identical(choice, "cancel"))

  enumerate(inferred, function(package, remote) {
    record <- renv_remotes_resolve(remote)
    record[["RemoteSha"]] <- NULL
    renv_package_augment(file.path(library, package), record)
  })

  n <- length(inferred)
  writef("- Updated %i package DESCRIPTION %s.", n, nplural("file", n))

  TRUE

}

renv_repair_sources_infer <- function(dcf) {

  # if this package appears to have a declared remote, use as-is
  for (field in c("RemoteType", "Repository", "biocViews"))
    if (!is.null(dcf[[field]]))
      return(NULL)

  # ok, this is a package installed from sources that "looks" like
  # the development version of a package; try to guess its remote
  guess <- function(pattern, field) {
    urls <- strsplit(dcf[[field]] %||% "", "\\s*,\\s*")[[1L]]
    for (url in urls) {
      matches <- regmatches(url, regexec(pattern, url, perl = TRUE))[[1L]]
      if (length(matches) == 3L)
        return(paste(matches[[2L]], matches[[3L]], sep = "/"))
    }
  }

  # first, check bug reports
  remote <- guess("^https://(?:www\\.)?github\\.com/([^/]+)/([^/]+)/issues$", "BugReports")
  if (!is.null(remote))
    return(remote)

  # next, check the URL field
  remote <- guess("^https://(?:www\\.)?github\\.com/([^/]+)/([^/]+)", "URL")
  if (!is.null(remote))
    return(remote)

}


# report.R -------------------------------------------------------------------


renv_report_ok <- function(message, elapsed = 0) {

  # treat 'quick' times specially
  if (!is_testing() && elapsed < 0.1)
    return(writef("OK [%s]", message))

  # otherwise, report step with elapsed time
  fmt <- "OK [%s in %s]"
  writef(fmt, message, renv_difftime_format_short(elapsed))

}


# repos.R --------------------------------------------------------------------


renv_repos_normalize <- function(repos = getOption("repos")) {

  # ensure repos are a character vector
  repos <- convert(repos, "character")

  # force a CRAN mirror when needed
  cran <- getOption("renv.repos.cran", "https://cloud.r-project.org")
  repos[repos == "@CRAN@"] <- cran

  # if repos is length 1 but has no names, then assume it's CRAN
  nms <- names(repos) %||% rep.int("", length(repos))
  if (identical(nms, ""))
    nms <- names(repos) <- "CRAN"

  # ensure all values are named
  unnamed <- !nzchar(nms)
  if (any(unnamed)) {
    nms[unnamed] <- paste0("V", seq_len(sum(unnamed)))
    names(repos) <- nms
  }

  # return normalized repository
  repos

}

renv_repos_validate <- function(repos = getOption("repos")) {

  # allow empty repository explicitly
  if (empty(repos))
    return(character())

  # otherwise, ensure it's a named list or character vector
  ok <- is.list(repos) || is.character(repos)
  if (!ok)
    stopf("repos has unexpected type '%s'", typeof(repos))

  # read repository names
  nm <- names(repos) %||% rep.int("", length(repos))
  if (any(nm %in% "")) {

    # if this is a length-one repository, assume it's CRAN
    if (length(repos) == 1L) {
      repos <- c(CRAN = repos)
      return(renv_repos_normalize(repos))
    }

    # otherwise, error
    stopf("all repository entries must be named")

  }

  # normalize the repos option
  renv_repos_normalize(repos)

}

renv_repos_info <- function(url) {

  memoize(
    key   = url,
    value = renv_repos_info_impl(url)
  )

}

renv_repos_info_impl <- function(url) {

  # make sure the repository URL includes a trailing slash
  url <- gsub("/*$", "/", url)

  # if this is a file repository, return early
  if (grepl("^file:", url))
    return(list(nexus = FALSE))

  # try to download it
  destfile <- renv_scope_tempfile("renv-repos-")
  status <- catch(download(url, destfile = destfile, quiet = TRUE))
  if (inherits(status, "error"))
    return(status)

  # read the contents of the page
  contents <- renv_file_read(destfile)

  # determine if this is a Nexus repository
  nexus <-
    grepl("Nexus Repository Manager", contents, fixed = TRUE) ||
    grepl("<div class=\"nexus-header\">", contents, fixed = TRUE)

  list(
    nexus = nexus
  )

}


# restart.R ------------------------------------------------------------------


# whether or not we're already trying to restart the session
the$restarting <- FALSE

renv_restart_request <- function(project = NULL, reason = "", ...) {

  project <- renv_project_resolve(project)

  # if we're running in RStudio, explicitly open the project
  # if it differs from the current project
  if (renv_rstudio_available()) {
    status <- renv_restart_request_rstudio(project, reason, ...)
    return(invisible(status))
  }

  renv_restart_request_default(project, reason, ...)

}

renv_restart_request_default <- function(project, reason, ...) {

  # use 'restart' helper defined by front-end (if any)
  restart <- getOption("restart")
  if (is.function(restart))
    return(renv_restart_invoke(restart))

  # otherwise, ask the user to restart
  if (interactive()) {
    fmt <- "- %s -- please restart the R session."
    writef(fmt, sprintf(reason, ...))
  }

}

renv_restart_request_rstudio <- function(project, reason, ...) {

  # if we're running tests, don't restart
  if (renv_tests_running())
    return(renv_restart_request_default(project, reason, ...))

  # if we don't have a tools env, bail
  tools <- catch(as.environment("tools:rstudio"))
  if (inherits(tools, "error"))
    return(renv_restart_request_default(project, reason, ...))

  # if RStudio is too old, use default restart impl
  old <-
    is.null(tools$.rs.getProjectDirectory) ||
    is.null(tools$.rs.api.openProject)

  if (old)
    return(renv_restart_request_default(project, reason, ...))

  # if the requested project matches the current project, just
  # restart the R session -- but note that we cannot respect
  # the 'restart' option here as the version RStudio uses
  # tries to preserve session state that we need to change.
  #
  # https://github.com/rstudio/renv/issues/1530
  projdir <- tools$.rs.getProjectDirectory() %||% ""
  if (renv_file_same(projdir, project)) {
    restart <- getOption("renv.restart.function", default = function() {
      tools$.rs.api.executeCommand("restartR", quiet = TRUE)
    })
    return(renv_restart_invoke(restart))
  }

  # otherwise, explicitly open the new project
  renv_restart_invoke(function() {
    invisible(tools$.rs.api.openProject(project, newSession = FALSE))
  })

}

renv_restart_invoke <- function(callback) {

  # avoid multiple attempts to restart in a single call, just in case
  if (!the$restarting) {
    the$restarting <- TRUE
    callback()
  }

}


# restore.R ------------------------------------------------------------------


the$restore_running <- FALSE
the$restore_state <- NULL

#' Restore project library from a lockfile
#'
#' Restore a project's dependencies from a lockfile, as previously generated by
#' [snapshot()]. `renv::restore()` compares packages recorded in the lockfile to
#' the packages installed in the project library. Where there are differences
#' it resolves them by installing the lockfile-recorded package into the
#' project library. If `clean = TRUE`, `restore()` will additionally delete any
#' packages in the project library that don't appear in the lockfile.
#'
#' @inherit renv-params
#'
#' @param library The library paths to be used during restore. See **Library**
#'   for details.
#'
#' @param packages A subset of packages recorded in the lockfile to restore.
#'   When `NULL` (the default), all packages available in the lockfile will be
#'   restored. Any required recursive dependencies of the requested packages
#'   will be restored as well.
#'
#' @param exclude A subset of packages to be excluded during restore. This can
#'  be useful for when you'd like to restore all but a subset of packages from
#'  a lockfile. Note that if you attempt to exclude a package which is required
#'  as the recursive dependency of another package, your request will be
#'  ignored.
#'
#' @return A named list of package records which were installed by renv.
#'
#' @family reproducibility
#'
#' @export
#'
#' @example examples/examples-init.R
restore <- function(project  = NULL,
                    ...,
                    library  = NULL,
                    lockfile = NULL,
                    packages = NULL,
                    exclude  = NULL,
                    rebuild  = FALSE,
                    repos    = NULL,
                    clean    = FALSE,
                    prompt   = interactive())
{
  renv_consent_check()
  renv_scope_error_handler()
  renv_dots_check(...)

  renv_scope_binding(the, "restore_running", TRUE)

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)
  renv_scope_verbose_if(prompt)

  # resolve library, lockfile arguments
  libpaths <- renv_libpaths_resolve(library)
  lockfile <- lockfile %||% renv_lockfile_load(project = project, strict = TRUE)

  # check and ask user if they need to activate first
  renv_activate_prompt("restore", library, prompt, project)

  # activate the requested library (place at front of library paths)
  library <- nth(libpaths, 1L)
  ensure_directory(library)
  renv_scope_libpaths(libpaths)

  # resolve the lockfile
  if (is.character(lockfile))
    lockfile <- renv_lockfile_read(lockfile)

  # inject overrides (if any)
  lockfile <- renv_lockfile_override(lockfile)

  # repair potential issues in the lockfile
  lockfile <- renv_lockfile_repair(lockfile)

  # override repositories if requested
  repos <- repos %||% config$repos.override() %||% lockfile$R$Repositories
  if (length(repos))
    renv_scope_options(repos = convert(repos, "character"))

  # if users have requested the use of pak, delegate there
  if (config$pak.enabled() && !recursing()) {
    renv_pak_init()
    renv_pak_restore(
      lockfile = lockfile,
      packages = packages,
      exclude  = exclude,
      project  = project
    )
  }

  # set up Bioconductor version + repositories
  biocversion <- lockfile$Bioconductor$Version
  if (!is.null(biocversion)) {
    renv_bioconductor_init(library = library)
    biocversion <- package_version(biocversion)
    renv_scope_options(renv.bioconductor.version = biocversion)
  }

  # get records for R packages currently installed
  current <- snapshot(project  = project,
                      library  = libpaths,
                      lockfile = NULL,
                      type     = "all")

  # compare lockfile vs. currently-installed packages
  diff <- renv_lockfile_diff_packages(current, lockfile)

  # don't remove packages unless 'clean = TRUE'
  diff <- renv_vector_diff(diff, if (!clean) "remove")

  # only remove packages from the project library
  is_package <- map_lgl(names(diff), function(package) {
    path <- find.package(package, lib.loc = libpaths, quiet = TRUE)
    identical(dirname(path), library)
  })
  diff <- diff[!(diff == "remove" & !is_package)]

  # don't take any actions with ignored packages
  ignored <- renv_project_ignored_packages(project = project)
  diff <- diff[renv_vector_diff(names(diff), ignored)]

  # only take action with requested packages
  packages <- setdiff(packages %||% names(diff), exclude)
  diff <- diff[intersect(names(diff), packages)]

  if (!length(diff)) {
    name <- if (!missing(library)) "library" else "project"
    writef("- The %s is already synchronized with the lockfile.", name)
    return(renv_restore_successful(diff, prompt, project))
  }

  # TODO: should we avoid double-prompting here?
  # we prompt once here for the preflight check, and then again below based
  # on the actions we'll perform.
  if (!renv_restore_preflight(project, libpaths, diff, current, lockfile))
    cancel_if(prompt && !proceed())

  if (prompt || renv_verbose()) {
    renv_restore_report_actions(diff, current, lockfile)
    cancel_if(prompt && !proceed())
  }

  # perform the restore
  records <- renv_restore_run_actions(project, diff, current, lockfile, rebuild)
  renv_restore_successful(records, prompt, project)
}

renv_restore_run_actions <- function(project, actions, current, lockfile, rebuild) {

  packages <- names(actions)

  renv_scope_restore(
    project  = project,
    library  = renv_libpaths_active(),
    records  = renv_lockfile_records(lockfile),
    packages = packages,
    rebuild  = rebuild
  )

  # first, handle package removals
  removes <- actions[actions == "remove"]
  enumerate(removes, function(package, action) {
    renv_restore_remove(project, package, current)
  })

  # next, handle installs
  installs <- actions[actions != "remove"]
  packages <- names(installs)

  # perform the install
  records <- retrieve(packages)
  renv_install_impl(records)

  # detect dependency tree repair
  diff <- renv_lockfile_diff_packages(renv_lockfile_records(lockfile), records)
  diff <- diff[diff != "remove"]
  if (!empty(diff)) {
    renv_pretty_print_records(
      "The dependency tree was repaired during package installation:",
      records[names(diff)],
      "Call `renv::snapshot()` to capture these dependencies in the lockfile."
    )
  }

  # check installed packages and prompt for reload if needed
  renv_install_postamble(names(records))

  # return status
  invisible(records)

}

renv_restore_state <- function(key = NULL) {
  state <- the$restore_state
  if (is.null(key)) state else state[[key]]
}

renv_restore_begin <- function(project = NULL,
                               library = NULL,
                               records = NULL,
                               packages = NULL,
                               handler = NULL,
                               rebuild = NULL,
                               recursive = TRUE)
{
  # resolve rebuild request
  rebuild <- case(
    identical(rebuild, TRUE)  ~ packages,
    identical(rebuild, FALSE) ~ character(),
    identical(rebuild, "*")   ~ NA_character_,
    as.character(rebuild)
  )

  # get previous restore state (so we can restore it after if needed)
  oldstate <- the$restore_state

  # set new restore state
  the$restore_state <- env(

    # the active project (if any) used for restore
    project = project,

    # the library path into which packages will be installed.
    # this is set because some behaviors depend on whether the target
    # library is the project library, but during staged installs the
    # library paths might be mutated during restore
    library = library,

    # the package records used for restore, providing information
    # on the packages to be installed (their version, source, etc)
    records = records,

    # the set of packages to be installed in this restore session;
    # as explicitly requested by the user / front-end API call.
    # packages in this list should be re-installed even if a compatible
    # version appears to be already installed
    packages = packages,

    # an optional handler, to be used during retrieve / restore
    # TODO: should we split this into separate handlers?
    handler = handler %||% function(package, action) action,

    # packages which should be rebuilt (skipping the cache)
    rebuild = rebuild,

    # should package dependencies be crawled recursively? this is useful if
    # the records list is incomplete and needs to be built as packages are
    # downloaded
    recursive = recursive,

    # packages which we have attempted to retrieve
    retrieved = new.env(parent = emptyenv()),

    # packages which need to be installed
    install = stack(),

    # a collection of the requirements imposed on dependent packages
    # as they are discovered
    requirements = new.env(parent = emptyenv()),

    # the number of packages that were downloaded
    downloaded = 0L

  )

  # return prior state
  oldstate

}

renv_restore_end <- function(state) {
  the$restore_state <- state
}

# nocov start

renv_restore_report_actions <- function(actions, current, lockfile) {

  if (!renv_verbose() || empty(actions))
    return(invisible(NULL))

  lhs <- renv_lockfile_records(current)
  rhs <- renv_lockfile_records(lockfile)
  renv_pretty_print_records_pair(
    "The following package(s) will be updated:",
    lhs[names(lhs) %in% names(actions)],
    rhs[names(rhs) %in% names(actions)]
  )

}

# nocov end

renv_restore_remove <- function(project, package, lockfile) {
  records <- renv_lockfile_records(lockfile)
  record <- records[[package]]
  printf("- Removing %s [%s] ... ", package, record$Version)
  paths <- renv_paths_library(project = project, package)
  recursive <- renv_file_type(paths) == "directory"
  unlink(paths, recursive = recursive)
  writef("OK [removed from library]")
  TRUE
}

renv_restore_preflight <- function(project, libpaths, actions, current, lockfile) {
  records <- renv_lockfile_records(lockfile)
  matching <- keep(records, names(actions))
  renv_install_preflight(project, libpaths, matching)
}

renv_restore_find <- function(package, record) {

  # skip packages whose installation was explicitly requested
  state <- renv_restore_state()
  record <- renv_record_validate(package, record)
  if (package %in% state$packages)
    return("")

  # check the active library paths to see if this package is already installed
  for (library in renv_libpaths_all()) {
    path <- renv_restore_find_impl(package, record, library)
    if (nzchar(path))
      return(path)
  }

  ""

}

renv_restore_find_impl <- function(package, record, library) {

  path <- file.path(library, package)
  if (!file.exists(path))
    return("")

  # attempt to read DESCRIPTION
  current <- catch(as.list(renv_description_read(path)))
  if (inherits(current, "error"))
    return("")

  # check for an up-to-date version from R package repository
  if (renv_record_source(record) %in% c("cran", "repository")) {
    fields <- c("Package", "Version")
    if (identical(record[fields], current[fields]))
      return(path)
  }

  # otherwise, match on remote fields
  fields <- renv_record_names(record, c("Package", "Version"))
  if (identical(record[fields], current[fields]))
    return(path)

  # failed to match; return empty path
  ""

}

renv_restore_rebuild_required <- function(record) {
  state <- renv_restore_state()
  any(c(NA_character_, record$Package) %in% state$rebuild)
}

renv_restore_successful <- function(records, prompt, project) {

  # ensure the activate script is up-to-date
  renv_infrastructure_write_activate(project, create = FALSE)

  # perform python-related restore steps
  renv_python_restore(project, prompt)

  # return restored records
  invisible(records)

}


# retrieve.R -----------------------------------------------------------------


the$repos_archive <- new.env(parent = emptyenv())

# this routine retrieves a package + its dependencies, and as a side
# effect populates the restore state's `retrieved` member with a
# list of package records which can later be used for install
retrieve <- function(packages) {

  # confirm that we have restore state set up
  state <- renv_restore_state()
  if (is.null(state))
    stopf("renv_restore_begin() must be called first")

  # normalize repositories (ensure @CRAN@ is resolved)
  options(repos = renv_repos_normalize())

  # transform repository URLs for PPM
  if (renv_ppm_enabled()) {
    repos <- getOption("repos")
    renv_scope_options(repos = renv_ppm_transform(repos))
  }

  # ensure HTTPUserAgent is set (required for PPM binaries)
  agent <- renv_http_useragent()
  if (!grepl("renv", agent)) {
    renv <- sprintf("renv (%s)", renv_metadata_version())
    agent <- paste(renv, agent, sep = "; ")
  }
  renv_scope_options(HTTPUserAgent = agent)

  before <- Sys.time()
  handler <- state$handler
  for (package in packages)
    handler(package, renv_retrieve_impl(package))
  after <- Sys.time()

  state <- renv_restore_state()
  count <- state$downloaded
  if (count) {
    elapsed <- difftime(after, before, units = "secs")
    writef("Successfully downloaded %s in %s.", nplural("package", count), renv_difftime_format(elapsed))
    writef("")
  }

  data <- state$install$data()
  names(data) <- extract_chr(data, "Package")
  data

}

renv_retrieve_impl <- function(package) {

  # skip packages with 'base' priority
  if (package %in% renv_packages_base())
    return()

  # if we've already attempted retrieval of this package, skip
  state <- renv_restore_state()
  if (visited(package, envir = state$retrieved))
    return()

  # extract record for package
  records <- state$records
  record <- records[[package]] %||% renv_retrieve_resolve(package)

  # normalize the record source
  source <- renv_record_source(record, normalize = TRUE)

  # don't install packages from incompatible OS
  ostype <- tolower(record[["OS_type"]] %||% "")

  skip <-
    renv_platform_unix() && identical(ostype, "windows") ||
    renv_platform_windows() && identical(ostype, "unix")

  if (skip)
    return()

  # if this is a package from Bioconductor, activate those repositories now
  if (source %in% c("bioconductor")) {
    project <- renv_restore_state(key = "project")
    renv_scope_bioconductor(project = project)
  }

  # if this is a package from R-Forge, activate its repository
  if (source %in% c("repository")) {
    repository <- record$Repository %||% ""
    if (tolower(repository) %in% c("rforge", "r-forge")) {
      repos <- getOption("repos")
      if (!"R-Forge" %in% names(repos)) {
        repos[["R-Forge"]] <- "https://R-Forge.R-project.org"
        renv_scope_options(repos = repos)
      }
    }
  }

  # if the record doesn't declare the package version,
  # treat it as a request for the latest version on CRAN
  # TODO: should make this behavior configurable
  uselatest <-
    source %in% c("repository", "bioconductor") &&
    is.null(record$Version)

  if (uselatest) {
    record <- withCallingHandlers(
      renv_available_packages_latest(package),
      error = function(err) stopf("package '%s' is not available", package)
    )
  }

  # if the requested record is incompatible with the set
  # of requested package versions thus far, request the
  # latest version on the R package repositories
  #
  # TODO: handle more explicit dependency requirements
  # TODO: report to the user if they have explicitly requested
  # installation of this package version despite it being incompatible
  compat <- renv_retrieve_incompatible(package, record)
  if (NROW(compat)) {

    # get the latest available package version
    replacement <- renv_available_packages_latest(package)
    if (is.null(replacement))
      stopf("package '%s' is not available", package)

    # if it's not compatible, then we might need to try again with
    # a source version (assuming type = "both")
    pkgtype <- getOption("pkgType")
    if (identical(pkgtype, "both")) {
      iscompat <- renv_retrieve_incompatible(package, replacement)
      if (NROW(iscompat)) {
        replacement <- renv_available_packages_latest(package, type = "source")
      }
    }

    # report if we couldn't find a compatible package
    renv_retrieve_incompatible_report(package, record, replacement, compat)
    record <- replacement

  }

  if (!renv_restore_rebuild_required(record)) {

    # if we have an installed package matching the requested record, finish early
    path <- renv_restore_find(package, record)
    if (file.exists(path)) {
      install <- !dirname(path) %in% renv_libpaths_all()
      return(renv_retrieve_successful(record, path, install = install))
    }

    # if the requested record already exists in the cache,
    # we'll use that package for install
    cacheable <-
      renv_cache_config_enabled(project = state$project) &&
      renv_record_cacheable(record)

    if (cacheable) {

      # try to find the record in the cache
      path <- renv_cache_find(record)
      if (nzchar(path) && renv_cache_package_validate(path))
        return(renv_retrieve_successful(record, path))
    }

  }

  # if this is a URL source, then it should already have a local path
  # check for the Path and Source fields and see if they resolve
  fields <- c("Path", "Source")
  for (field in fields) {

    # check for a valid field
    path <- record[[field]]
    if (is.null(path))
      next

    # check whether it looks like an explicit source
    isurl <-
      is.character(path) &&
      nzchar(path) &&
      grepl("[/\\]|[.](?:zip|tgz|gz)$", path)

    if (!isurl)
      next

    # error if the field is declared but doesn't exist
    if (!file.exists(path)) {
      fmt <- "record for package '%s' declares local source '%s', but that file does not exist"
      stopf(fmt, record$Package, path)
    }

    # otherwise, success
    path <- renv_path_normalize(path, mustWork = TRUE)
    return(renv_retrieve_successful(record, path))

  }

  if (!renv_restore_rebuild_required(record)) {

    # try some early shortcut methods
    shortcuts <- c(
      renv_retrieve_explicit,
      renv_retrieve_cellar,
      if (!renv_tests_running() && config$install.shortcuts())
        renv_retrieve_libpaths
    )

    for (shortcut in shortcuts) {
      retrieved <- catch(shortcut(record))
      if (identical(retrieved, TRUE))
        return(TRUE)
    }

  }

  state$downloaded <- state$downloaded + 1L
  if (state$downloaded == 1L)
    writef(header("Downloading packages"))

  # time to retrieve -- delegate based on previously-determined source
  switch(source,
         bioconductor = renv_retrieve_bioconductor(record),
         bitbucket    = renv_retrieve_bitbucket(record),
         git          = renv_retrieve_git(record),
         github       = renv_retrieve_github(record),
         gitlab       = renv_retrieve_gitlab(record),
         repository   = renv_retrieve_repos(record),
         url          = renv_retrieve_url(record),
         renv_retrieve_unknown_source(record)
  )

}

renv_retrieve_name <- function(record, type = "source", ext = NULL) {
  package <- record$Package
  version <- record$RemoteSha %||% record$Version
  ext <- ext %||% renv_package_ext(type)
  sprintf("%s_%s%s", package, version, ext)
}

renv_retrieve_path <- function(record, type = "source", ext = NULL) {

  # extract relevant record information
  package <- record$Package
  name <- renv_retrieve_name(record, type, ext)
  source <- renv_record_source(record)

  # check for packages from an PPM binary URL, and
  # update the package type if known
  if (renv_ppm_enabled()) {
    url <- attr(record, "url")
    if (is.character(url) && grepl("/__[^_]+__/", url))
      type <- "binary"
  }

  # form path for package to be downloaded
  if (type == "source")
    renv_paths_source(source, package, name)
  else if (type == "binary")
    renv_paths_binary(source, package, name)
  else
    stopf("unrecognized type '%s'", type)
}

renv_retrieve_bioconductor <- function(record) {

  # try to read the bioconductor version from the record
  version <- renv_retrieve_bioconductor_version(record)

  # activate Bioconductor repositories in this context
  project <- renv_restore_state(key = "project")
  renv_scope_bioconductor(project = project, version = version)

  # retrieve record using updated repositories
  renv_retrieve_repos(record)

}

renv_retrieve_bioconductor_version <- function(record) {

  # read git branch
  branch <- record[["git_branch"]]
  if (is.null(branch))
    return(NULL)

  # try and parse version
  parts <- strsplit(branch, "_", fixed = TRUE)[[1L]]
  ok <-
    length(parts) == 3L &&
    tolower(parts[[1L]]) == "release"

  if (!ok)
    return(NULL)

  # we have a version; use it
  paste(tail(parts, n = -1L), collapse = ".")

}

renv_retrieve_bitbucket <- function(record) {

  # query repositories endpoint to find download URL
  host <- record$RemoteHost %||% config$bitbucket.host()
  origin <- renv_retrieve_origin(host)
  username <- record$RemoteUsername
  repo <- record$RemoteRepo

  # scope authentication
  renv_scope_auth(repo)

  fmt <- "%s/repositories/%s/%s"
  url <- sprintf(fmt, origin, username, repo)

  destfile <- renv_scope_tempfile("renv-bitbucket-")
  download(url, destfile = destfile, quiet = TRUE)
  json <- renv_json_read(destfile)

  # now build URL to tarball
  base <- json$links$html$href
  ref <- record$RemoteSha %||% record$RemoteRef

  fmt <- "%s/get/%s.tar.gz"
  url <- sprintf(fmt, base, ref)

  path <- renv_retrieve_path(record)

  renv_retrieve_package(record, url, path)

}

renv_retrieve_github <- function(record) {

  host <- record$RemoteHost %||% config$github.host()
  origin <- renv_retrieve_origin(host)
  username <- record$RemoteUsername
  repo <- record$RemoteRepo
  ref <- record$RemoteSha %||% record$RemoteRef

  if (is.null(ref)) {
    fmt <- "GitHub record for package '%s' has no recorded 'RemoteSha' / 'RemoteRef'"
    stopf(fmt, record$Package)
  }

  fmt <- "%s/repos/%s/%s/tarball/%s"
  url <- with(record, sprintf(fmt, origin, username, repo, ref))
  path <- renv_retrieve_path(record)
  renv_retrieve_package(record, url, path)

}

renv_retrieve_gitlab <- function(record) {

  host <- record$RemoteHost %||% config$gitlab.host()
  origin <- renv_retrieve_origin(host)

  user <- record$RemoteUsername
  repo <- record$RemoteRepo
  id <- URLencode(paste(user, repo, sep = "/"), reserved = TRUE)

  fmt <- "%s/api/v4/projects/%s/repository/archive.tar.gz"
  url <- sprintf(fmt, origin, id)
  path <- renv_retrieve_path(record)

  sha <- record$RemoteSha %||% record$RemoteRef
  if (!is.null(sha))
    url <- paste(url, paste("sha", sha, sep = "="), sep = "?")

  renv_retrieve_package(record, url, path)

}

renv_retrieve_git <- function(record) {
  # NOTE: This path will later be used during the install step, so we don't
  # want to clean it up afterwards
  path <- tempfile("renv-git-")
  ensure_directory(path)
  renv_retrieve_git_impl(record, path)
  renv_retrieve_successful(record, path)
}

renv_retrieve_git_impl <- function(record, path) {

  renv_git_preflight()

  package <- record$Package
  url     <- record$RemoteUrl
  ref     <- record$RemoteRef
  sha     <- record$RemoteSha

  # figure out the default ref
  gitref <- case(
    nzchar(sha %||% "") ~ sha,
    nzchar(ref %||% "") ~ ref,
    "HEAD"
  )

  # be quiet if requested
  quiet <- getOption("renv.git.quiet", default = TRUE)
  quiet <- if (quiet) "--quiet" else ""

  template <- heredoc('
    git init ${QUIET}
    git remote add origin "${ORIGIN}"
    git fetch ${QUIET} --depth=1 origin "${REF}"
    git reset ${QUIET} --hard FETCH_HEAD
  ')

  data <- list(
    ORIGIN = url,
    REF    = gitref,
    QUIET  = quiet
  )

  commands <- renv_template_replace(template, data)
  command <- gsub("\n", " && ", commands, fixed = TRUE)
  if (renv_platform_windows())
    command <- paste(comspec(), "/C", command)

  printf("- Cloning '%s' ... ", url)

  before <- Sys.time()

  status <- local({
    ensure_directory(path)
    renv_scope_wd(path)
    renv_scope_auth(record)
    renv_scope_git_auth()
    system(command)
  })

  after <- Sys.time()

  if (status != 0L) {
    fmt <- "error cloning '%s' from '%s' [status code %i]"
    stopf(fmt, package, url, status)
  }

  fmt <- "OK [cloned repository in %s]"
  elapsed <- difftime(after, before, units = "auto")
  writef(fmt, renv_difftime_format(elapsed))

  TRUE

}


renv_retrieve_cellar_find <- function(record, project = NULL) {

  project <- renv_project_resolve(project)

  # packages installed with 'remotes::install_local()' will
  # have a RemoteUrl entry that we can use
  url <- record$RemoteUrl %||% ""
  if (file.exists(url)) {
    path <- renv_path_normalize(url, mustWork = TRUE)
    type <- if (fileext(path) %in% c(".tgz", ".zip")) "binary" else "source"
    return(named(path, type))
  }

  # otherwise, look in the cellar
  roots <- renv_cellar_roots(project)
  for (type in c("binary", "source")) {

    name <- renv_retrieve_name(record, type = type)
    for (root in roots) {

      package <- record$Package
      paths <- c(
        file.path(root, package, name),
        file.path(root, name)
      )

      for (path in paths)
        if (file.exists(path))
          return(named(path, type))

    }
  }

  fmt <- "%s [%s] is not available locally"
  stopf(fmt, record$Package, record$Version)

}

renv_retrieve_cellar_report <- function(record) {

  source <- renv_record_source(record)
  if (source == "cellar")
    return(record)

  fmt <- "- Package %s [%s] will be installed from the cellar."
  with(record, writef(fmt, Package, Version))

  record

}

renv_retrieve_cellar <- function(record) {
  source <- renv_retrieve_cellar_find(record)
  record <- renv_retrieve_cellar_report(record)
  renv_retrieve_successful(record, source)
}

renv_retrieve_libpaths <- function(record) {

  libpaths <- c(renv_libpaths_user(), renv_libpaths_site())
  for (libpath in libpaths)
    if (renv_retrieve_libpaths_impl(record, libpath))
      return(TRUE)

}

renv_retrieve_libpaths_impl <- function(record, libpath) {

  # form path to installed package's DESCRIPTION
  path <- file.path(libpath, record$Package)
  if (!file.exists(path))
    return(FALSE)

  # read DESCRIPTION
  desc <- renv_description_read(path = path)

  # check if it's compatible with the requested record
  fields <- c("Package", "Version", grep("^Remote", names(record), value = TRUE))
  compatible <- identical(record[fields], desc[fields])
  if (!compatible)
    return(FALSE)

  # check that it was built for a compatible version of R
  built <- desc[["Built"]]
  if (is.null(built))
    return(FALSE)

  ok <- catch(renv_description_built_version(desc))
  if (!identical(ok, TRUE))
    return(FALSE)

  # check that this package has a known source
  source <- renv_snapshot_description_source(desc)
  if (identical(source$Source, "unknown"))
    return(FALSE)

  # OK: copy this package as-is
  renv_retrieve_successful(record, path)

}

renv_retrieve_explicit <- function(record) {

  # try parsing as a local remote
  source <- record$Path %||% record$RemoteUrl %||% ""
  if (nzchar(source)) {
    resolved <- catch(renv_remotes_resolve_path(source))
    if (inherits(resolved, "error"))
      return(FALSE)
  }

  # treat as 'local' source but extract path
  normalized <- renv_path_normalize(source, mustWork = TRUE)
  resolved$Source <- "Local"
  renv_retrieve_successful(resolved, normalized)

}

renv_retrieve_repos <- function(record) {

  # if this record is tagged with a type + url, we can
  # use that directly for retrieval
  if (all(c("type", "url") %in% names(attributes(record))))
    return(renv_retrieve_repos_impl(record))

  # figure out what package sources are okay to use here
  pkgtype <- getOption("pkgType", default = "source")

  srcok <- pkgtype %in% c("both", "source") ||
    getOption("install.packages.check.source", default = "yes") %in% "yes"

  binok <- pkgtype %in% c("both") || grepl("binary", pkgtype, fixed = TRUE)

  # collect list of 'methods' for retrieval
  methods <- stack(mode = "list")

  # add binary package methods
  if (binok) {

    # prefer repository binaries if available
    methods$push(renv_retrieve_repos_binary)

    # also try fallback binary locations (for Nexus)
    methods$push(renv_retrieve_repos_binary_fallback)

    # if MRAN is enabled, check those binaries as well
    if (renv_mran_enabled())
      methods$push(renv_retrieve_repos_mran)

  }

  # next, try to retrieve from sources
  if (srcok) {

    # retrieve from source repositories
    methods$push(renv_retrieve_repos_source)

    # also try fallback source locations (for Nexus)
    methods$push(renv_retrieve_repos_source_fallback)

    # if this is a package from r-universe, try restoring from github
    # (currently inferred from presence for RemoteUrl field)
    unifields <- c("RemoteUrl", "RemoteRef", "RemoteSha")
    if (all(unifields %in% names(record)))
      methods$push(renv_retrieve_git)
    else
      methods$push(renv_retrieve_repos_archive)

  }

  # capture errors for reporting
  errors <- stack()

  for (method in methods$data()) {

    status <- catch(
      withCallingHandlers(
        method(record),
        renv.retrieve.error = function(error) {
          errors$push(error$data)
        }
      )
    )

    if (inherits(status, "error")) {
      errors$push(status)
      next
    }

    if (identical(status, TRUE))
      return(TRUE)

    if (!is.logical(status)) {
      fmt <- "internal error: unexpected status code '%s'"
      warningf(fmt, stringify(status))
    }

  }

  # if we couldn't download the package, report the errors we saw
  local({
    renv_scope_options(warn = 1)
    for (error in errors$data())
      warning(error)
  })

  stopf("failed to retrieve package '%s'", renv_record_format_remote(record))

}

renv_retrieve_repos_error_report <- function(record, errors) {

  if (empty(errors))
    return()

  messages <- extract(errors, "message")
  if (empty(messages))
    return()

  messages <- unlist(messages, recursive = TRUE, use.names = FALSE)
  if (empty(messages))
    return()

  fmt <- "The following error(s) occurred while retrieving '%s':"
  preamble <- sprintf(fmt, record$Package)

  caution_bullets(
    preamble = preamble,
    values   = paste("-", messages)
  )

  if (renv_verbose())
    str(errors)

}

renv_retrieve_url <- function(record) {

  if (is.null(record$RemoteUrl)) {
    fmt <- "package '%s' has no recorded RemoteUrl"
    stopf(fmt, record$Package)
  }

  resolved <- renv_remotes_resolve_url(record$RemoteUrl, quiet = FALSE)
  renv_retrieve_successful(record, resolved$Path)

}

renv_retrieve_repos_archive_name <- function(record, type = "source") {

  file <- record$File
  if (length(file) && !is.na(file))
    return(file)

  ext <- renv_package_ext(type)
  paste0(record$Package, "_", record$Version, ext)

}

renv_retrieve_repos_mran <- function(record) {

  # MRAN does not make binaries available on Linux
  if (renv_platform_linux())
    return(FALSE)

  # ensure local MRAN database is up-to-date
  renv_mran_database_refresh(explicit = FALSE)

  # check that we have an available database
  path <- renv_mran_database_path()
  if (!file.exists(path))
    return(FALSE)

  # attempt to read it
  database <- catch(renv_mran_database_load())
  if (inherits(database, "error")) {
    warning(database)
    return(FALSE)
  }

  # get entry for this version of R + platform
  suffix <- contrib.url("", type = "binary")
  entry <- database[[suffix]]
  if (is.null(entry))
    return(FALSE)

  # check for known entry for this package + version
  key <- paste(record$Package, record$Version)
  idate <- entry[[key]]
  if (is.null(idate))
    return(FALSE)

  # convert from integer to date
  date <- as.Date(idate, origin = "1970-01-01")

  # form url to binary package
  base <- renv_mran_url(date, suffix)
  name <- renv_retrieve_name(record, type = "binary")
  url <- file.path(base, name)

  # form path to saved file
  path <- renv_retrieve_path(record, "binary")

  # attempt to retrieve
  renv_retrieve_package(record, url, path)

}

renv_retrieve_repos_binary <- function(record) {
  renv_retrieve_repos_impl(record, "binary")
}

renv_retrieve_repos_binary_fallback <- function(record) {

  for (repo in getOption("repos")) {
    if (renv_nexus_enabled(repo)) {
      repourl <- contrib.url(repo, type = "binary")
      status <- catch(renv_retrieve_repos_impl(record, "binary", repo = repourl))
      if (!inherits(status, "error"))
        return(status)
    }
  }

  FALSE

}

renv_retrieve_repos_source <- function(record) {
  renv_retrieve_repos_impl(record, "source")
}

renv_retrieve_repos_source_fallback <- function(record, repo) {

  for (repo in getOption("repos")) {
    if (renv_nexus_enabled(repo)) {
      repourl <- contrib.url(repo, type = "source")
      status <- catch(renv_retrieve_repos_impl(record, "source", repo = repourl))
      if (!inherits(status, "error"))
        return(status)
    }
  }

  FALSE

}

renv_retrieve_repos_archive <- function(record) {

  for (repo in getOption("repos")) {

    # try to determine path to package in archive
    url <- renv_retrieve_repos_archive_path(repo, record)
    if (is.null(url))
      next

    # attempt download
    name <- renv_retrieve_repos_archive_name(record, type = "source")
    status <- catch(renv_retrieve_repos_impl(record, "source", name, url))
    if (identical(status, TRUE))
      return(TRUE)

  }

  return(FALSE)

}

renv_retrieve_repos_archive_path <- function(repo, record) {

  # allow users to provide a custom archive path for a record,
  # in case they're using a repository that happens to archive
  # packages with a different format than regular CRAN network
  # https://github.com/rstudio/renv/issues/602
  override <- getOption("renv.retrieve.repos.archive.path")
  if (is.function(override)) {
    result <- override(repo, record)
    if (!is.null(result))
      return(result)
  }

  # if we already know the format of the repository, use that
  if (exists(repo, envir = the$repos_archive)) {
    formatter <- get(repo, envir = the$repos_archive)
    root <- formatter(repo, record)
    return(root)
  }

  # otherwise, try determining the archive paths with a couple
  # custom locations, and cache the version that works for the
  # associated repository
  formatters <- list(

    # default CRAN format
    function(repo, record) {
      with(record, file.path(repo, "src/contrib/Archive", Package))
    },

    # format used by Artifactory
    # https://github.com/rstudio/renv/issues/602
    function(repo, record) {
      with(record, file.path(repo, "src/contrib/Archive", Package, Version))
    },

    # format used by Nexus
    # https://github.com/rstudio/renv/issues/595
    function(repo, record) {
      with(record, file.path(repo, "src/contrib"))
    }

  )

  name <- renv_retrieve_repos_archive_name(record, "source")
  for (formatter in formatters) {
    root <- formatter(repo, record)
    url <- file.path(root, name)
    if (renv_download_available(url)) {
      assign(repo, formatter, envir = the$repos_archive)
      return(root)
    }
  }

}

# NOTE: If 'repo' is provided, it should be the path to the appropriate 'arm'
# of a repository, which is normally generated from the repository URL via
# 'contrib.url()'.
renv_retrieve_repos_impl <- function(record,
                                     type = NULL,
                                     name = NULL,
                                     repo = NULL)
{
  package <- record$Package
  version <- record$Version

  type <- type %||% attr(record, "type", exact = TRUE)
  name <- name %||% renv_retrieve_repos_archive_name(record, type)
  repo <- repo %||% attr(record, "url", exact = TRUE)

  # if we weren't provided a repository for this package, try to find it
  if (is.null(repo)) {

    entry <- catch(
      renv_available_packages_entry(
        package = package,
        type    = type,
        filter  = version,
        prefer  = record[["Repository"]]
      )
    )

    if (inherits(entry, "error")) {
      attr(entry, "record") <- record
      renv_condition_signal("renv.retrieve.error", entry)
      return(FALSE)
    }

    # get repository path
    repo <- entry$Repository

    # add in the path if available
    path <- entry$Path
    if (length(path) && !is.na(path))
      repo <- file.path(repo, path)

    # update the tarball name if it was declared
    file <- entry$File
    if (length(file) && !is.na(file))
      name <- file

  }

  url <- file.path(repo, name)
  path <- renv_retrieve_path(record, type)

  renv_retrieve_package(record, url, path)

}


renv_retrieve_package <- function(record, url, path) {

  ensure_parent_directory(path)
  type <- renv_record_source(record)
  status <- local({
    renv_scope_auth(record)
    preamble <- renv_retrieve_package_preamble(record, url)
    catch(download(url, preamble = preamble, destfile = path, type = type))
  })

  # report error for logging upstream
  if (inherits(status, "error")) {
    attr(status, "record") <- record
    renv_condition_signal("renv.retrieve.error", status)
  }

  # handle FALSE returns (shouldn't normally happen?)
  if (identical(status, FALSE)) {
    fmt <- "an unknown error occurred installing '%s' (%s)"
    msg <- sprintf(fmt, record$Package, renv_record_format_remote(record))
    status <- simpleError(msg)
  }

  # handle errors
  if (inherits(status, "error"))
    stop(status)

  # handle success
  renv_retrieve_successful(record, path)

}

renv_retrieve_package_preamble <- function(record, url) {

  message <- sprintf(
    "- Downloading %s from %s ... ",
    record$Package,
    record$Repository %||% record$Source
  )

  format(message, width = the$install_step_width)

}

renv_retrieve_successful_subdir <- function(record, path) {

  # if it's a file, assume RemoteSubdir needs to be honored
  info <- file.info(path, extra_cols = FALSE)
  if (identical(info$isdir, FALSE))
    return(record$RemoteSubdir)

  # otherwise, respect RemoteSubdir only if it seems to
  # point at a valid DESCRPITION file
  if (!is.null(record$RemoteSubdir)) {
    parts <- c(path, record$RemoteSubdir, "DESCRIPTION")
    descpath <- paste(parts, collapse = "/")
    if (file.exists(descpath))
      return(record$RemoteSubdir)
  }

}

renv_retrieve_successful <- function(record, path, install = TRUE) {

  # if we downloaded an archive, adjust its permissions here
  mode <- Sys.getenv("RENV_CACHE_MODE", unset = NA)
  if (!is.na(mode)) {
    info <- file.info(path, extra_cols = FALSE)
    if (identical(info$isdir, FALSE)) {
      parent <- dirname(path)
      renv_system_exec(
        command = "chmod",
        args    = c("-Rf", renv_shell_quote(mode), renv_shell_path(parent)),
        action  = "chmoding cached package",
        quiet   = TRUE,
        success = NULL
      )
    }
  }

  # the handling of 'subdir' here is a little awkward, as this function
  # can receive:
  #
  # - archives, whose package might live within a sub-directory;
  # - folders, whose package might live within a sub-directory;
  # - cache paths, for which the subdir is no longer relevant
  #
  # this warrants a proper cleanup, but for now we we use a hack
  subdir <- renv_retrieve_successful_subdir(record, path)

  # augment record with information from DESCRIPTION file
  desc <- renv_description_read(path, subdir = subdir)

  # update the record's package name, version
  # TODO: should we warn if they didn't match for some reason?
  record$Package <- desc$Package
  record$Version <- desc$Version

  # add in path information to record (used later during install)
  record$Path <- path

  # record this package's requirements
  state <- renv_restore_state()
  requirements <- state$requirements

  # figure out the dependency fields to use -- if the user explicitly requested
  # this package be installed, but also provided a 'dependencies' argument in
  # the call to 'install()', then we want to use those
  fields <- if (record$Package %in% state$packages) the$install_dependency_fields else "strong"
  deps <- renv_dependencies_discover_description(path, subdir = subdir, fields = fields)
  if (length(deps$Source))
    deps$Source <- record$Package

  rowapply(deps, function(dep) {
    package <- dep$Package
    requirements[[package]] <- requirements[[package]] %||% stack()
    requirements[[package]]$push(dep)
  })

  # read and handle remotes declared by this package
  remotes <- desc$Remotes
  if (length(remotes) && config$install.remotes())
    renv_retrieve_remotes(remotes)

  # ensure its dependencies are retrieved as well
  if (state$recursive) local({
    repos <- if (is.null(desc$biocViews)) getOption("repos") else renv_bioconductor_repos()
    renv_scope_options(repos = repos)
    renv_retrieve_successful_recurse(deps)
  })

  # mark package as requiring install if needed
  if (install)
    state$install$push(record)

  TRUE

}

renv_retrieve_successful_recurse <- function(deps) {
  remotes <- unique(deps$Package)
  for (remote in remotes)
    renv_retrieve_successful_recurse_impl(remote)
}

renv_retrieve_successful_recurse_impl <- function(remote) {

  dynamic(
    key   = list(remote = remote),
    value = renv_retrieve_successful_recurse_impl_one(remote)
  )

}

renv_retrieve_successful_recurse_impl_one <- function(remote) {

  # ignore base packages
  base <- renv_packages_base()
  if (remote %in% base)
    return(list())

  # if this is a 'plain' package remote, retrieve it
  if (grepl(renv_regexps_package_name(), remote)) {
    renv_retrieve_impl(remote)
    return(list())
  }

  # otherwise, handle custom remotes
  record <- renv_retrieve_remotes_impl(remote)
  if (length(record)) {
    renv_retrieve_impl(record$Package)
    return(list())
  }

  list()

}

renv_retrieve_unknown_source <- function(record) {

  # try to find a matching local package
  status <- catch(renv_retrieve_cellar(record))
  if (!inherits(status, "error"))
    return(status)

  # failed; parse as though from R package repository
  record$Source <- "Repository"
  renv_retrieve_repos(record)

}

# TODO: what should we do if we detect incompatible remotes?
# e.g. if pkg A requests 'r-lib/rlang@0.3' but pkg B requests
# 'r-lib/rlang@0.2'.
renv_retrieve_remotes <- function(remotes) {
  remotes <- strsplit(remotes, "\\s*,\\s*")[[1L]]
  for (remote in remotes)
    renv_retrieve_remotes_impl(remote)
}

renv_retrieve_remotes_impl <- function(remote) {

  dynamic(
    key   = list(remote = remote),
    value = renv_retrieve_remotes_impl_one(remote)
  )

}

renv_retrieve_remotes_impl_one <- function(remote) {

  # TODO: allow customization of behavior when remote parsing fails?
  resolved <- catch(renv_remotes_resolve(remote))
  if (inherits(resolved, "error")) {
    warningf("failed to resolve remote '%s'; skipping", remote)
    return(invisible(NULL))
  }

  # get the current package record
  state <- renv_restore_state()
  package <- resolved$Package
  record <- state$records[[package]]

  # if we already have a package record, and it's not a 'plain'
  # repository record, skip
  skip <-
    !is.null(record) &&
    !identical(record, list(Package = package, Source = "Repository"))

  if (skip) {
    dlog("retrieve", "skipping remote '%s'; it's already been declared", remote)
    dlog("retrieve", "using existing remote '%s'", stringify(record))
    return(invisible(NULL))
  }

  # update the requested record
  dlog("retrieve", "using remote '%s'", remote)
  state$records[[package]] <- resolved

  # mark the record as needing retrieval
  state$retrieved[[package]] <- FALSE

  # return new record
  invisible(resolved)

}

renv_retrieve_resolve <- function(package) {
  tryCatch(
    renv_snapshot_description(package = package),
    error = function(e) {
      renv_retrieve_missing_record(package)
    }
  )
}

renv_retrieve_missing_record <- function(package) {

  # TODO: allow users to configure the action to take here, e.g.
  #
  #   1. retrieve latest from R repositories (the default),
  #   2. request a package + version to be retrieved,
  #   3. hard error
  #
  record <- renv_available_packages_latest(package)
  if (!is.null(record))
    return(record)

  fmt <- heredoc("
    renv was unable to find a compatible version of package '%1$s'.

    The latest-available version %1$s is '%2$s', but that version
    does not appear to be compatible with this version of R.

    You may need to manually re-install a different version of '%1$s'.
  ")

  entry <- renv_available_packages_entry(package, type = "source")
  version <- entry$Version %||% "<unknown>"

  writef(fmt, package, version)

  stopf("failed to find a compatible version of the '%s' package", package)

}

# check to see if this requested record is incompatible
# with the set of required dependencies recorded thus far
# during the package retrieval process
renv_retrieve_incompatible <- function(package, record) {

  state <- renv_restore_state()
  record <- renv_record_validate(package, record)

  # check and see if the installed version satisfies all requirements
  requirements <- state$requirements[[package]]
  if (is.null(requirements))
    return(NULL)

  data <- bind(requirements$data())
  explicit <- data[nzchar(data$Require) & nzchar(data$Version), ]
  if (nrow(explicit) == 0)
    return(NULL)

  # drop 'Dev' column
  explicit$Dev <- NULL

  # retrieve record version
  version <- record$Version
  if (is.null(version))
    return(NULL)

  # for each row, compute whether we're compatible
  rversion <- numeric_version(version)
  compatible <- map_lgl(seq_len(nrow(explicit)), function(i) {
    expr <- call(explicit$Require[[i]], rversion, explicit$Version[[i]])
    eval(expr, envir = baseenv())
  })

  # keep whatever wasn't compatible
  explicit[!compatible, ]

}

renv_retrieve_incompatible_report <- function(package, record, replacement, compat) {

  # only report if the user explicitly requesting installation of a particular
  # version of a package, but that package isn't actually compatible
  state <- renv_restore_state()
  if (!package %in% state$packages)
    return()

  fmt <- "%s (requires %s %s %s)"
  values <- with(compat, sprintf(fmt, Source, Package, Require, Version))

  fmt <- "Installation of '%s %s' was requested, but the following constraints are not met:"
  preamble <- with(record, sprintf(fmt, Package, Version))

  fmt <- "renv will try to install '%s %s' instead."
  postamble <- with(replacement, sprintf(fmt, Package, Version))

  if (!renv_tests_running()) {
    caution_bullets(
      preamble = preamble,
      values = values,
      postamble = postamble
    )
  }

}

renv_retrieve_origin <- function(host) {

  # NOTE: some host URLs may come with a protocol already formed;
  # if we find a protocol, use it as-is
  if (grepl("://", host, fixed = TRUE))
    return(host)

  # otherwise, prepend protocol (assume https)
  paste("https", host, sep = "://")

}


# robocopy.R -----------------------------------------------------------------


renv_robocopy_exec <- function(source, target, flags = NULL) {

  source <- path.expand(source)
  target <- path.expand(target)

  # add other flags
  flags <- c(flags, "/E", "/Z", "/R:5", "/W:10")

  # https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/robocopy
  # > Any value greater than 8 indicates that there was at least one failure
  # > during the copy operation.
  renv_system_exec(
    command = "robocopy",
    args    = c(flags, renv_shell_path(source), renv_shell_path(target)),
    action  = "copying directory",
    success = 0:8,
    quiet   = TRUE
  )

}

renv_robocopy_copy <- function(source, target) {
  renv_robocopy_exec(source, target)
}

renv_robocopy_move <- function(source, target) {
  renv_robocopy_exec(source, target, "/MOVE")
}


# roxygen.R ------------------------------------------------------------------


#' @param project The project directory. If `NULL`, then the active project will
#'   be used. If no project is currently active, then the current working
#'   directory is used instead.
#'
#' @param type The type of package to install ("source" or "binary"). Defaults
#'   to the value of `getOption("pkgType")`.
#'
#' @param lockfile Path to a lockfile. When `NULL` (the default), the
#'   `renv.lock` located in the root of the current project will be used.
#'
#' @param library The \R library to be used. When `NULL`, the active project
#'  library will be used instead.
#'
#' @param prompt Boolean; prompt the user before taking any action? For backwards
#'   compatibility, `confirm` is accepted as an alias for `prompt`.
#'
#' @param ... Unused arguments, reserved for future expansion. If any arguments
#'   are matched to `...`, renv will signal an error.
#'
#' @param clean Boolean; remove packages not recorded in the lockfile from
#'   the target library? Use `clean = TRUE` if you'd like the library state
#'   to exactly reflect the lockfile contents after `restore()`.
#'
#' @param rebuild Force packages to be rebuilt, thereby bypassing any installed
#'   versions of the package available in the cache? This can either be a
#'   boolean (indicating that all installed packages should be rebuilt), or a
#'   vector of package names indicating which packages should be rebuilt.
#'
#' @param repos The repositories to use when restoring packages installed
#'   from CRAN or a CRAN-like repository. By default, the repositories recorded
#'   in the lockfile will be, ensuring that (e.g.) CRAN packages are
#'   re-installed from the same CRAN mirror.
#'
#'   Use `repos = getOptions(repos)` to override with the repositories set
#'   in the current session, or see the `repos.override` option in [config] for
#'   an alternate way override.
#'
#' @param profile The profile to be activated. When `NULL`, the default
#'   profile is activated instead. See `vignette("profiles", package = "renv")`
#'   for more information.
#'
#' @param dependencies A vector of DESCRIPTION field names that should be used
#'   for package dependency resolution. When `NULL` (the default), the value
#'   of `renv::settings$package.dependency.fields` is used. The aliases
#'   "strong", "most", and "all" are also supported.
#'   See [tools::package_dependencies()] for more details.
#'
#' @return The project directory, invisibly. Note that this function is normally
#'   called for its side effects.
#'
#' @name renv-params
NULL

renv_roxygen_config_section <- function() {

  # read config
  config <- yaml::read_yaml("inst/config.yml")

  # generate items
  items <- map_chr(config, function(entry) {

    # extract fields
    name <- entry$name
    type <- entry$type
    default <- entry$default
    description <- entry$description

    # deparse default value
    default <- case(
      identical(default, list()) ~ "NULL",
      TRUE                       ~ deparse(default)
    )

    # generate table row
    fmt <- "\\subsection{renv.config.%s}{%s Defaults to \\code{%s}.}"
    sprintf(fmt, name, description, default)

  })

  c(
    "@section Configuration:",
    "",
    "The following renv configuration options are available:",
    "",
    items,
    ""
  )

}


# rstudio.R ------------------------------------------------------------------


renv_rstudio_available <- function() {

  # NOTE: detecting whether we're running within RStudio is a bit
  # tricky because not all of the expected RStudio bits have been
  # initialized when the R session is being initialized (e.g.
  # when the .Rprofile is being executed)
  args <- commandArgs(trailingOnly = FALSE)
  args[[1L]] == "RStudio" || .Platform$GUI == "RStudio"

}

renv_rstudio_initialize <- function(project) {

  tools <- catch(as.environment("tools:rstudio"))
  if (inherits(tools, "error"))
    return(FALSE)

  if (is.null(tools$.rs.api.initializeProject))
    return(FALSE)

  tools$.rs.api.initializeProject(project)
  TRUE

}

renv_rstudio_fixup <- function() {

  # if RStudio's tools are on the search path, we should try
  # to fix them up so that renv's own routines don't get seen
  tools <- catch(as.environment("tools:rstudio"))
  if (inherits(tools, "error"))
    return(FALSE)

  helper <- tools[[".rs.clearVar"]]
  if (is.null(helper))
    return(FALSE)

  # if the helper environment has been fixed up (as e.g. by
  # newer versions of RStudio) then nothing to do
  if (identical(tools, environment(helper)))
    return(FALSE)

  # put common tools from base into the environment
  envir <- environment(helper)
  for (var in c("assign", "exists", "get", "remove", "paste"))
    envir[[var]] <- get(var, envir = baseenv())

  TRUE

}


# rtools.R -------------------------------------------------------------------


renv_rtools_list <- function() {

  drive <- Sys.getenv("SYSTEMDRIVE", unset = "C:")

  roots <- c(

    renv_rtools_registry(),

    Sys.getenv("RTOOLS43_HOME", unset = file.path(drive, "rtools43")),
    Sys.getenv("RTOOLS42_HOME", unset = file.path(drive, "rtools42")),
    Sys.getenv("RTOOLS40_HOME", unset = file.path(drive, "rtools40")),
    file.path(drive, "Rtools"),
    list.files(file.path(drive, "RBuildTools"), full.names = TRUE),

    "~/Rtools",
    list.files("~/RBuildTools", full.names = TRUE)

  )

  roots <- unique(roots[file.exists(roots)])
  lapply(roots, renv_rtools_read)

}

renv_rtools_find <- function() {

  for (spec in renv_rtools_list())
    if (renv_rtools_compatible(spec))
      return(spec)

  NULL

}

renv_rtools_read <- function(root) {

  list(
    root    = root,
    version = renv_rtools_version(root)
  )

}

renv_rtools_version <- function(root) {

  name <- basename(root)

  # check for 'rtools<xyz>' folder
  # e.g. C:/rtools42
  pattern <- "^rtools(\\d)(\\d)$"
  if (grepl(pattern, name, perl = TRUE, ignore.case = TRUE))
    return(gsub(pattern, "\\1.\\2", name, perl = TRUE, ignore.case = TRUE))

  # check for versioned installation path
  # e.g. C:/RBuildTools/4.2
  version <- catch(numeric_version(name))
  if (!inherits(version, "error"))
    return(format(version))

  # detect older Rtools installations
  path <- file.path(root, "VERSION.txt")
  if (!file.exists(path))
    return(NULL)

  contents <- readLines(path, warn = FALSE)
  version <- gsub("[^[:digit:].]", "", contents)
  numeric_version(version)

}

renv_rtools_compatible <- function(spec) {

  if (is.null(spec$version))
    return(FALSE)

  ranges <- list(
    "4.3" = c("4.3.0", "9.9.9"),
    "4.2" = c("4.2.0", "4.3.0"),
    "4.0" = c("4.0.0", "4.2.0"),
    "3.5" = c("3.3.0", "4.0.0"),
    "3.4" = c("3.3.0", "4.0.0"),
    "3.3" = c("3.2.0", "3.3.0"),
    "3.2" = c("3.1.0", "3.2.0"),
    "3.1" = c("3.0.0", "3.1.0")
  )

  version <- numeric_version(spec$version)[1, 1:2]
  range <- ranges[[format(version)]]
  if (is.null(range))
    return(FALSE)

  rversion <- getRversion()
  range[[1]] <= rversion && rversion < range[[2]]

}

renv_rtools_registry <- function() {

  status <- tryCatch(
    utils::readRegistry(
      key = "SOFTWARE\\R-Core\\Rtools",
      hive = "HLM"
    ),
    error = function(e) list()
  )

  path <- status$InstallPath %||% ""
  if (file.exists(path))
    return(renv_path_normalize(path))

}

renv_rtools_envvars <- function(root) {

  version <- renv_rtools_version(root)

  if (version < "4.0")
    renv_rtools_envvars_default(root)
  else if (version < "4.2")
    renv_rtools_envvars_rtools40(root)
  else if (version < "4.3")
    renv_rtools_envvars_rtools42(root)
  else
    renv_rtools_envvars_rtools43(root)

}

renv_rtools_envvars_default <- function(root) {

  # add Rtools utilities to path
  bin <- normalizePath(file.path(root, "bin"), mustWork = FALSE)
  path <- paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)

  # set BINPREF (note: trailing slash is required)
  # file.path drops trailing separators on Windows, so we use paste
  binpref <- paste(renv_path_normalize(root), "mingw_$(WIN)/bin/", sep = "/")

  list(PATH = path, BINPREF = binpref)

}

renv_rtools_envvars_rtools43 <- function(root) {

  # add Rtools utilities to path
  bin <- normalizePath(file.path(root, "usr/bin"), mustWork = FALSE)
  path <- paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)

  # set BINPREF
  binpref <- ""

  list(PATH = path, BINPREF = binpref)

}

renv_rtools_envvars_rtools42 <- function(root) {

  # add Rtools utilities to path
  bin <- normalizePath(file.path(root, "usr/bin"), mustWork = FALSE)

  path <- paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)

  # set BINPREF
  binpref <- ""

  list(PATH = path, BINPREF = binpref)

}

renv_rtools_envvars_rtools40 <- function(root) {

  # add Rtools utilities to path
  bin <- normalizePath(file.path(root, "usr/bin"), mustWork = FALSE)
  path <- paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep)

  # set BINPREF (note: trailing slash is required)
  binpref <- "/mingw$(WIN)/bin/"

  list(PATH = path, BINPREF = binpref)

}


# run.R ----------------------------------------------------------------------


#' Run a script
#'
#' Run an \R script, in the context of a project using renv. The script will
#' be run within an \R sub-process.
#'
#' @inherit renv-params
#'
#' @param script The path to an \R script.
#'
#' @param job Run the requested script as an RStudio job? Requires a recent
#'   version of both RStudio and the rstudioapi packages. When `NULL`, the
#'   script will be run as a job if possible, and as a regular \R process
#'   launched by [system2()] if not.
#'
#' @param name The name to associate with the job, for scripts run as a job.
#'
#' @param project The path to the renv project. This project will be loaded
#'   before the requested script is executed. When `NULL` (the default), renv
#'   will automatically determine the project root for the associated script
#'   if possible.
#'
#' @export
run <- function(script, ..., job = NULL, name = NULL, project = NULL) {

  renv_scope_error_handler()
  renv_dots_check(...)

  script <- renv_path_normalize(script, mustWork = TRUE)

  # find the project directory
  project <- project %||% renv_file_find(script, function(path) {
    paths <- file.path(path, c("renv", "renv.lock"))
    if (any(file.exists(paths)))
      return(path)
  })

  if (is.null(project)) {
    fmt <- "could not determine project root for script '%s'"
    stopf(fmt, renv_path_aliased(script))
  }

  # ensure that it has an activate script
  activate <- renv_paths_activate(project = project)
  if (!file.exists(activate)) {
    fmt <- "project '%s' does not have an renv activate script"
    stopf(fmt, renv_path_aliased(project))
  }

  # run as a job when possible in RStudio
  jobbable <-
    !identical(job, FALSE) &&
    renv_rstudio_available() &&
    renv_package_installed("rstudioapi") &&
    renv_package_version("rstudioapi") >= "0.10" &&
    rstudioapi::verifyAvailable("1.2.1335")

  if (identical(job, TRUE) && identical(jobbable, FALSE))
    stopf("cannot run script as job: required versions of RStudio + rstudioapi not available")

  if (jobbable)
    renv_run_job(script = script, name = name, project = project)
  else
    renv_run_impl(script = script, name = name, project = project)

}

renv_run_job <- function(script, name, project) {

  activate <- renv_paths_activate(project = project)
  jobscript <- tempfile("renv-job-", fileext = ".R")

  exprs <- substitute(local({
    defer(unlink(jobscript))
    source(activate)
    source(script)
  }), list(activate = activate, script = script, jobscript = jobscript))

  code <- deparse(exprs)
  writeLines(code, con = jobscript)

  rstudioapi::jobRunScript(
    path       = jobscript,
    workingDir = project,
    name       = name
  )

}

renv_run_impl <- function(script, name, project) {
  renv_scope_wd(project)
  system2(R(), c("-s", "-f", renv_shell_path(script)))
}


# sandbox.R ------------------------------------------------------------------


renv_sandbox_init <- function() {

  # check for envvar override
  enabled <- Sys.getenv("RENV_SANDBOX_LOCKING_ENABLED", unset = NA)
  if (!is.na(enabled)) {
    enabled <- truthy(enabled, default = TRUE)
    options(renv.sandbox.locking_enabled = enabled)
  }

  # if renv was launched with a sandbox path on the library paths,
  # then immediately try to activate the sandbox
  # https://github.com/rstudio/renv/issues/1565
  for (libpath in .libPaths()) {
    if (file.exists(file.path(libpath, ".renv-sandbox"))) {
      renv_sandbox_activate_impl(sandbox = libpath)
      break
    }
  }

}

renv_sandbox_activate <- function(project = NULL) {

  # record start time
  before <- Sys.time()

  # attempt the activation
  status <- catch(renv_sandbox_activate_impl(project))
  if (inherits(status, "error"))
    warnify(status)

  # record end time
  after <- Sys.time()

  # check for long elapsed time
  elapsed <- difftime(after, before, units = "secs")

  # if it took too long to activate the sandbox, warn the user
  if (elapsed > 10) {

    fmt <- heredoc("
    renv took longer than expected (%s) to activate the sandbox.

    The sandbox can be disabled by setting:

        RENV_CONFIG_SANDBOX_ENABLED = FALSE

    within an appropriate start-up .Renviron file.

    See `?renv::config` for more details.
    ")


    warningf(fmt, renv_difftime_format(elapsed))

  }

  # return status
  status

}

renv_sandbox_activate_impl <- function(project = NULL, sandbox = NULL) {

  # lock access to the sandbox
  if (config$sandbox.enabled()) {
    sandbox <- sandbox %||% renv_sandbox_path(project = project)
    lockfile <- paste(sandbox, "lock", sep = ".")
    ensure_parent_directory(lockfile)
    renv_scope_lock(lockfile)
    ensure_directory(sandbox)
  }

  # get current library paths
  oldlibs <- .libPaths()
  syslibs <- c(renv_libpaths_site(), renv_libpaths_system())
  syslibs <- renv_path_normalize(syslibs)

  # override .Library.site
  base <- .BaseNamespaceEnv
  renv_binding_replace(base, ".Library.site", NULL)

  # generate sandbox
  if (config$sandbox.enabled()) {
    renv_sandbox_generate(sandbox)
    renv_binding_replace(base, ".Library", sandbox)
  }

  # update library paths
  newlibs <- renv_vector_diff(oldlibs, syslibs)
  renv_libpaths_set(newlibs)

  # protect against user profiles that might update library paths
  if (config$sandbox.enabled())
    renv_sandbox_activate_check(newlibs)

  # return new library paths
  renv_libpaths_all()

}

renv_sandbox_activated <- function() {
  !identical(.Library, renv_libpaths_system())
}

renv_sandbox_activate_check <- function(libs) {

  envir <- globalenv()

  danger <-
    exists(".First", envir = envir, inherits = FALSE) &&
    identical(getOption("renv.autoloader.running"), TRUE)

  if (!danger)
    return(FALSE)

  .First <- get(".First", envir = envir, inherits = FALSE)
  wrapper <- function() {

    # scope the library paths as currently defined
    renv_scope_libpaths()

    # call the user-defined .First function
    status <- tryCatch(.First(), error = warnify)

    # double-check if we should restore .First (this is extra
    # paranoid but in theory .First could remove itself)
    if (identical(wrapper, get(".First", envir = envir)))
      assign(".First", .First, envir = envir)

    # return result of .First
    invisible(status)

  }

  assign(".First", wrapper, envir = envir)
  return(TRUE)

}

renv_sandbox_generate <- function(sandbox) {

  # make the library temporarily writable
  lock <- getOption("renv.sandbox.locking_enabled", default = TRUE)

  if (lock) {
    dlog("sandbox", "unlocking sandbox")
    renv_sandbox_unlock(sandbox)
  }

  # find system packages in the system library
  priority <- getOption("renv.sandbox.priority", default = c("base", "recommended"))
  syspkgs <- installed_packages(
    lib.loc = renv_libpaths_system(),
    priority = priority
  )

  # link into sandbox
  sources <- with(syspkgs, file.path(LibPath, Package))
  targets <- with(syspkgs, file.path(sandbox, Package))
  names(targets) <- sources
  enumerate(targets, function(source, target) {
    if (!renv_file_same(source, target))
      renv_file_link(source, target, overwrite = TRUE)
  })

  # create marker indicating this is a sandbox
  marker <- file.path(sandbox, ".renv-sandbox")
  file.create(marker)

  # make the library unwritable again
  if (lock) {
    dlog("sandbox", "locking sandbox")
    renv_sandbox_lock(sandbox)
  }

  # return sandbox path
  sandbox

}

renv_sandbox_deactivate <- function() {

  # get library paths sans .Library, .Library.site
  old <- renv_libpaths_all()
  syslibs <- renv_path_normalize(c(.Library, .Library.site))

  # restore old bindings
  base <- .BaseNamespaceEnv
  renv_binding_replace(base, ".Library",      renv_libpaths_system())
  renv_binding_replace(base, ".Library.site", renv_libpaths_site())

  # update library paths
  new <- renv_vector_diff(old, syslibs)
  renv_libpaths_set(new)

  renv_libpaths_all()

}

renv_sandbox_task <- function(...) {

  # check if we're enabled
  if (!renv_sandbox_activated())
    return()

  enabled <- getOption("renv.sandbox.task", default = TRUE)
  if (!enabled)
    return()

  # make sure the sandbox exists
  sandbox <- tail(.libPaths(), n = 1L)
  if (!file.exists(sandbox)) {
    warning("the renv sandbox was deleted; it will be re-generated", call. = FALSE)
    ensure_directory(sandbox)
    renv_sandbox_generate(sandbox)
  }

}

renv_sandbox_path <- function(project = NULL) {
  renv_paths_sandbox(project = project)
}

renv_sandbox_lock <- function(sandbox = NULL, project = NULL) {
  sandbox <- sandbox %||% renv_sandbox_path(project = project)
  Sys.chmod(sandbox, mode = "0555")
}

renv_sandbox_locked <- function(sandbox = NULL, project = NULL) {
  sandbox <- sandbox %||% renv_sandbox_path(project = project)
  mode <- suppressWarnings(file.mode(sandbox))
  mode == 365L  # as.integer(as.octmode("0555"))
}

renv_sandbox_unlock <- function(sandbox = NULL, project = NULL) {
  sandbox <- sandbox %||% renv_sandbox_path(project = project)
  Sys.chmod(sandbox, mode = "0755")
}

#' The default library sandbox
#'
#' @description
#' An \R installation can have up to three types of library paths available
#' to the user:
#'
#' - The _user library_, where \R packages downloaded and installed by the
#'   current user are installed. This library path is only visible to that
#'   specific user.
#'
#' - The _site library_, where \R packages maintained by administrators of a
#'   system are installed. This library path, if it exists, is visible to all
#'   users on the system.
#'
#' - The _default library_, where \R packages distributed with \R itself are
#'   installed. This library path is visible to all users on the system.
#'
#' Normally, only so-called "base" and "recommended" packages should be installed
#' in the default library. (You can get a list of these packages with
#' `installed.packages(priority = c("base", "recommended"))`). However, it is
#' possible for users and administrators to install packages into the default
#' library, if the filesystem permissions permit them to do so. (This, for
#' example, is the default behavior on macOS.)
#'
#' Because the site and default libraries are visible to all users, having those
#' accessible in renv projects can potentially break isolation -- that is,
#' if a package were updated in the default library, that update would be visible
#' to all \R projects on the system.
#'
#' To help defend against this, renv uses something called the "sandbox" to
#' isolate renv projects from non-"base" packages that are installed into the
#' default library. When an renv project is loaded, renv will:
#'
#' - Create a new, empty library path (called the "sandbox"),
#'
#' - Link only the "base" and "recommended" packages from the default library
#'   into the sandbox,
#'
#' - Mark the sandbox as read-only, so that users are unable to install packages
#'   into this library,
#'
#' - Instruct the \R session to use the "sandbox" as the default library.
#'
#' This process is mostly transparent to the user. However, because the sandbox
#' is read-only, if you later need to remove the sandbox, you'll need to reset
#' file permissions manually; for example, with `renv::sandbox$unlock()`.
#'
#' If you'd prefer to keep the sandbox unlocked, you can also set:
#'
#' ```
#' RENV_SANDBOX_LOCKING_ENABLED = FALSE
#' ```
#'
#' in an appropriate startup `.Renviron` or `Renviron.site` file.
#'
#' The sandbox can also be disabled entirely with:
#'
#' ```
#' RENV_CONFIG_SANDBOX_ENABLED = FALSE
#' ```
#'
#' The sandbox library path can also be configured using the `RENV_PATHS_SANDBOX`
#' environment variable: see [paths] for more details.
#'
#' @format NULL
#' @export
sandbox <- list(
  path   = renv_sandbox_path,
  lock   = renv_sandbox_lock,
  locked = renv_sandbox_locked,
  unlock = renv_sandbox_unlock
)


# scaffold.R -----------------------------------------------------------------

#' Generate project infrastructure
#'
#' @description
#' Create the renv project infrastructure. This will:
#'
#' - Create a project library, `renv/library`.
#'
#' - Install renv into the project library.
#'
#' - Update the project `.Rprofile` to call `source("renv/activate.R")` so
#'   that renv is automatically loaded for new \R sessions launched in
#'   this project.
#'
#' - Create `renv/.gitignore`, which tells git to ignore the project library.
#'
#' - Create `.Rbuildignore`, if the project is also a package. This tells
#'   `R CMD build` to ignore the renv infrastructure,
#'
#' - Write a (bare) [lockfile], `renv.lock`.
#'
#' @inheritParams renv-params
#'
#' @param version The version of renv to associate with this project. By
#'   default, the version of renv currently installed is used.
#'
#' @param repos The \R repositories to associate with this project.
#'
#' @param settings A list of renv settings, to be applied to the project
#'   after creation. These should map setting names to the desired values.
#'   See [settings] for more details.
#'
#' @examples
#'
#' \dontrun{
#' # create scaffolding with 'devtools' ignored
#' renv::scaffold(settings = list(ignored.packages = "devtools"))
#' }
#'
#' @export
scaffold <- function(project  = NULL,
                     version  = NULL,
                     repos    = getOption("repos"),
                     settings = NULL)
{
  renv_scope_error_handler()
  renv_scope_options(repos = repos)

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  # install renv into project library
  renv_imbue_impl(project, version)

  # write out project infrastructure
  renv_infrastructure_write(project, version)

  # update project settings
  if (is.list(settings))
    renv_settings_persist(project, settings)

  # generate a lockfile
  lockfile <- renv_lockfile_create(
    project  = project,
    libpaths = renv_paths_library(project = project),
    type     = "implicit"
  )

  renv_lockfile_write(lockfile, file = renv_lockfile_path(project))

  # notify user
  fmt <- "- renv infrastructure has been generated for project %s."
  writef(fmt, renv_path_pretty(project))

  # return project invisibly
  invisible(project)
}


# scope.R --------------------------------------------------------------------


renv_scope_tempdir <- function(pattern = "renv-tempdir-",
                               tmpdir = tempdir(),
                               umask = NULL,
                               scope = parent.frame())
{
  dir <- renv_scope_tempfile(pattern = pattern, tmpdir = tmpdir, scope = scope)
  ensure_directory(dir, umask = umask)

  renv_scope_wd(dir, scope = scope)
  dir
}

renv_scope_auth <- function(record, scope = parent.frame()) {

  package <- if (is.list(record)) record$Package else record
  auth <- renv_options_override("renv.auth", package, extra = record)

  if (empty(auth))
    return(FALSE)

  envvars <- catch({
    if (is.function(auth))
      auth(record)
    else
      auth
  })

  # warn user if auth appears invalid
  if (inherits(envvars, "error")) {
    warning(envvars)
    return(FALSE)
  }

  if (empty(envvars))
    return(FALSE)

  renv_scope_envvars(list = as.list(envvars), scope = scope)
  return(TRUE)

}

renv_scope_libpaths <- function(new = .libPaths(), scope = parent.frame()) {
  old <- renv_libpaths_set(new)
  defer(renv_libpaths_set(old), scope = scope)
}

renv_scope_options <- function(..., scope = parent.frame()) {
  new <- list(...)
  old <- options(new)
  defer(options(old), scope = scope)
}

renv_scope_locale <- function(category = "LC_ALL", locale = "", scope = parent.frame()) {
  saved <- Sys.getlocale(category)
  Sys.setlocale(category, locale)
  defer(Sys.setlocale(category, saved), scope = scope)
}

renv_scope_envvars <- function(..., list = NULL, scope = parent.frame()) {

  dots <- list %||% list(...)
  old <- as.list(Sys.getenv(names(dots), unset = NA))
  names(old) <- names(dots)

  unset <- map_lgl(dots, is.null)
  Sys.unsetenv(names(dots[unset]))
  if (length(dots[!unset]))
    do.call(Sys.setenv, dots[!unset])

  defer({
    na <- is.na(old)
    Sys.unsetenv(names(old[na]))
    if (length(old[!na]))
      do.call(Sys.setenv, old[!na])
  }, scope = scope)

}

renv_scope_error_handler <- function(scope = parent.frame()) {

  error <- getOption("error")
  if (!is.null(error))
    return(FALSE)

  call <- renv_error_handler_call()
  options(error = call)

  defer({
    if (identical(getOption("error"), call))
      options(error = error)
  }, scope = scope)

  TRUE

}

# used to enforce usage of curl 7.64.1 within the
# renv_paths_extsoft folder when available on Windows

# nocov start
renv_scope_downloader <- function(scope = parent.frame()) {

  if (!renv_platform_windows())
    return(FALSE)

  if (nzchar(Sys.which("curl")))
    return(FALSE)

  curlroot <- sprintf("curl-%s-win32-mingw", renv_extsoft_curl_version())
  curl <- renv_paths_extsoft(curlroot, "bin/curl.exe")
  if (!file.exists(curl))
    return(FALSE)

  old <- Sys.getenv("PATH", unset = NA)
  if (is.na(old))
    return(FALSE)

  new <- paste(renv_path_normalize(dirname(curl)), old, sep = .Platform$path.sep)

  renv_scope_envvars(PATH = new, scope = scope)

}
# nocov end

# nocov start
renv_scope_rtools <- function(scope = parent.frame()) {

  if (!renv_platform_windows())
    return(FALSE)

  # check for Rtools
  root <- renv_paths_rtools()
  if (!file.exists(root))
    return(FALSE)

  # get environment variables appropriate for version of Rtools
  vars <- renv_rtools_envvars(root)

  # scope envvars in parent
  renv_scope_envvars(list = vars, scope = scope)

}
# nocov end

# nocov start
renv_scope_install <- function(scope = parent.frame()) {

  if (renv_platform_macos())
    renv_scope_install_macos(scope)

  if (renv_platform_wsl())
    renv_scope_install_wsl(scope)

}

renv_scope_install_macos <- function(scope = parent.frame()) {

  # check that we have command line tools available before invoking
  # R CMD config, as this might fail otherwise
  if (once()) {
    if (!renv_xcode_available()) {
      message("- macOS is reporting that command line tools (CLT) are not installed.")
      message("- Run 'xcode-select --install' to install command line tools.")
      message("- Without CLT, attempts to install packages from sources may fail.")
    }
  }

  # get the current compiler
  args <- c("CMD", "config", "CC")
  cc <- system2(R(), args, stdout = TRUE, stderr = TRUE)

  # check to see if we're using the system toolchain
  # (need to be careful since users might put e.g. ccache or other flags
  # into the CC variable)

  # helper for creating regex matching compiler bits
  matches <- function(pattern) {
    regex <- paste("(?:[[:space:]]|^)", pattern, "(?:[[:space:]]|$)", sep = "")
    grepl(regex, cc)
  }

  sysclang <- case(
    matches("/usr/bin/clang") ~ TRUE,
    matches("clang")          ~ Sys.which("clang") == "/usr/bin/clang",
    FALSE
  )

  # check for an appropriate LLVM toolchain -- if it exists, use it
  spec <- renv_equip_macos_spec()
  if (sysclang && !is.null(spec) && file.exists(spec$dst)) {
    path <- paste(file.path(spec$dst, "bin"), Sys.getenv("PATH"), sep = ":")
    renv_scope_envvars(PATH = path, scope = scope)
  }

  # generate a custom makevars that should better handle compilation
  # with the system toolchain (or other toolchains)
  makevars <- stack()

  # if we don't have an LLVM toolchain available, then try to generate
  # a Makeconf that shields compilation from usages of '-fopenmp'
  if (sysclang) {

    makeconf <- readLines(file.path(R.home("etc"), "Makeconf"), warn = FALSE)
    mplines <- grep(" -fopenmp", makeconf, fixed = TRUE, value = TRUE)

    # read a user makevars (if any)
    contents <- character()
    mvsite <- Sys.getenv(
      "R_MAKEVARS_SITE",
      unset = file.path(R.home("etc"), "Makevars.site")
    )

    if (file.exists(mvsite))
      contents <- readLines(mvsite, warn = FALSE)

    # override usages of '-fopenmp'
    replaced <- gsub(" -fopenmp", "", mplines, fixed = TRUE)
    amended <- unique(c(contents, replaced))
    makevars$push(amended)

  }

  # write makevars to file
  path <- tempfile("Makevars-")
  contents <- unlist(makevars$data(), recursive = TRUE, use.names = FALSE)
  if (length(contents)) {
    writeLines(contents, con = path)
    renv_scope_envvars(R_MAKEVARS_SITE = path, scope = scope)
  }

  TRUE

}

renv_scope_install_wsl <- function(scope = parent.frame()) {
  renv_scope_envvars(R_INSTALL_STAGED = "FALSE", scope = scope)
}
# nocov end

renv_scope_restore <- function(..., scope = parent.frame()) {
  state <- renv_restore_begin(...)
  defer(renv_restore_end(state), scope = scope)
}

renv_scope_git_auth <- function(scope = parent.frame()) {

  # try and tell git to be non-interactive by default
  if (renv_platform_windows()) {
    renv_scope_envvars(
      GIT_TERMINAL_PROMPT = "0",
      scope              = scope
    )
  } else {
    renv_scope_envvars(
      GIT_TERMINAL_PROMPT = "0",
      GIT_ASKPASS         = "/bin/echo",
      scope              = scope
    )
  }

  # use GIT_PAT when provided
  pat <- Sys.getenv("GIT_PAT", unset = NA)
  if (!is.na(pat)) {
    renv_scope_envvars(
      GIT_USERNAME = pat,
      GIT_PASSWORD = "x-oauth-basic",
      scope = scope
    )
  }

  # only set askpass when GIT_USERNAME + GIT_PASSWORD are set
  user <-
    Sys.getenv("GIT_USERNAME", unset = NA) %NA%
    Sys.getenv("GIT_USER",     unset = NA)

  pass <-
    Sys.getenv("GIT_PASSWORD", unset = NA) %NA%
    Sys.getenv("GIT_PASS",     unset = NA)

  if (is.na(user) || is.na(pass))
    return(FALSE)

  askpass <- if (renv_platform_windows())
    system.file("resources/scripts-git-askpass.cmd", package = "renv")
  else
    system.file("resources/scripts-git-askpass.sh", package = "renv")

  renv_scope_envvars(GIT_ASKPASS = askpass, scope = scope)
  return(TRUE)

}

renv_scope_bioconductor <- function(project = NULL,
                                    version = NULL,
                                    scope = parent.frame())
{
  # get current repository
  repos <- getOption("repos")

  # remove old / stale bioc repositories
  stale <- grepl("Bioc", names(repos))
  repos <- repos[!stale]

  # retrieve bioconductor repositories appropriate for this project
  biocrepos <- renv_bioconductor_repos(project = project, version = version)

  # put it all together
  allrepos <- c(repos, biocrepos)

  # activate repositories in this context
  renv_scope_options(repos = renv_vector_unique(allrepos), scope = scope)
}

renv_scope_lock <- function(path = NULL, scope = parent.frame()) {
  renv_lock_acquire(path)
  defer(renv_lock_release(path), scope = scope)
}

renv_scope_trace <- function(what, tracer, scope = parent.frame()) {

  call <- sys.call()
  call[[1L]] <- base::trace
  call[["print"]] <- FALSE
  defer(suppressMessages(untrace(substitute(what))), scope = scope)

  suppressMessages(eval(call, envir = parent.frame()))

}


renv_scope_binding <- function(envir, symbol, replacement, scope = parent.frame()) {
  if (exists(symbol, envir, inherits = FALSE)) {
    old <- renv_binding_replace(envir, symbol, replacement)
    defer(renv_binding_replace(envir, symbol, old), scope = scope)
  } else {
    assign(symbol, replacement, envir)
    defer(rm(list = symbol, envir = envir, inherits = FALSE), scope = scope)
  }
}

renv_scope_tempfile <- function(pattern = "renv-tempfile-",
                                tmpdir  = tempdir(),
                                fileext = "",
                                scope  = parent.frame())
{
  path <- renv_path_normalize(tempfile(pattern, tmpdir, fileext))
  defer(unlink(path, recursive = TRUE, force = TRUE), scope = scope)
  invisible(path)
}

renv_scope_umask <- function(umask, scope = parent.frame()) {
  oldmask <- Sys.umask(umask)
  defer(Sys.umask(oldmask), scope = scope)
  invisible(oldmask)
}

renv_scope_wd <- function(dir = getwd(), scope = parent.frame()) {
  owd <- setwd(dir)
  defer(setwd(owd), scope = scope)
  invisible(owd)
}

renv_scope_sandbox <- function(scope = parent.frame()) {
  sandbox <- renv_sandbox_activate()
  defer(renv_sandbox_deactivate(), scope = scope)
  invisible(sandbox)
}

renv_scope_biocmanager <- function(scope = parent.frame()) {

  # silence BiocManager messages when setting repositories
  renv_scope_options(BiocManager.check_repositories = FALSE, scope = scope)

  # R-devel (4.4.0) warns when BiocManager calls .make_numeric_version() without
  # a character argument, so just suppress those warnings in this scope
  #
  # https://github.com/wch/r-source/commit/1338a95618ddcc8a0af77dc06e4018625de06ec3
  renv_scope_options(warn = -1L, scope = scope)

  # return reference to BiocManager namespace
  renv_namespace_load("BiocManager")

}

renv_scope_caution <- function(value) {
  renv_scope_options(
    renv.caution.verbose = value,
    scope = parent.frame()
  )
}

renv_scope_verbose_if <- function(value, scope = parent.frame()) {
  if (value) {
    renv_scope_options(
      renv.verbose = TRUE,
      scope = scope
    )
  }
}


# sdkroot.R ------------------------------------------------------------------


renv_sdkroot_init <- function() {

  if (!renv_platform_macos())
    return()

  enabled <- Sys.getenv("RENV_SDKROOT_ENABLED", unset = "TRUE")
  if (!truthy(enabled, default = TRUE))
    return()

  sdkroot <- Sys.getenv("SDKROOT", unset = NA)
  if (!is.na(sdkroot))
    return()

  sdk <- "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk"
  if (!file.exists(sdk))
    return()

  makeconf <- file.path(R.home("etc"), "Makeconf")
  if (!file.exists(makeconf))
    return()

  contents <- readLines(makeconf)
  cxx <- grep("^CXX\\s*=", contents, value = TRUE, perl = TRUE)
  if (length(cxx) == 0L)
    return()

  if (!grepl("(?:/usr/local|/opt/homebrew)/opt/llvm", cxx))
    return()

  Sys.setenv(SDKROOT = sdk)

}


# session.R ------------------------------------------------------------------


renv_session_quiet <- function() {

  args <- commandArgs(trailingOnly = FALSE)

  index <- match("--args", args)
  if (!is.na(index))
    args <- head(args, n = index - 1L)

  quiet <- c("-s", "--slave", "--no-echo")
  any(quiet %in% args)

}


# settings.R -----------------------------------------------------------------


the$settings <- new.env(parent = emptyenv())

renv_settings_default <- function(name) {
  default <- the$settings[[name]]$default
  renv_options_override("renv.settings", name, default)
}

renv_settings_defaults <- function() {

  keys <- ls(envir = the$settings, all.names = TRUE)
  vals <- lapply(keys, renv_settings_default)
  names(vals) <- keys
  vals[order(names(vals))]

}

renv_settings_validate <- function(name, value) {

  # NULL implies restore default value
  if (is.null(value))
    return(renv_settings_default(name))

  # run coercion method
  value <- the$settings[[name]]$coerce(value)

  # validate the user-provided value
  validate <- the$settings[[name]]$validate
  ok <- case(
    is.character(validate) ~ value %in% validate,
    is.function(validate)  ~ validate(value),
    TRUE
  )

  if (identical(ok, TRUE))
    return(value)

  # validation failed; warn the user and use default
  fmt <- "%s is an invalid value for setting '%s'; using default %s instead"
  default <- renv_settings_default(name)
  warningf(fmt, deparsed(value), name, deparsed(default))
  default

}

renv_settings_read <- function(path) {

  filebacked(
    context  = "renv_settings_read",
    path     = path,
    callback = renv_settings_read_impl
  )

}

renv_settings_read_impl <- function(path) {

  # check that file exists
  if (!file.exists(path))
    return(NULL)

  # read settings
  settings <- case(
    endswith(path, ".dcf")  ~ renv_settings_read_impl_dcf(path),
    endswith(path, ".json") ~ renv_settings_read_impl_json(path),
    ~ stopf("don't know how to read settings file %s", renv_path_pretty(path))
  )

  # keep only known settings
  known <- ls(envir = the$settings, all.names = TRUE)
  settings <- keep(settings, known)

  # validate
  settings <- enumerate(settings, renv_settings_validate)

  # merge in defaults
  defaults <- renv_settings_defaults()
  missing <- renv_vector_diff(names(defaults), names(settings))
  settings[missing] <- defaults[missing]

  # and return
  settings

}

renv_settings_read_impl_dcf <- function(path) {

  # try to read it
  dcf <- catch(renv_dcf_read(path))
  if (inherits(dcf, "error")) {
    warning(dcf)
    return(NULL)
  }

  # decode encoded values
  enumerate(dcf, function(name, value) {

    case(
      value == "NULL"  ~ NULL,
      value == "NA"    ~ NA,
      value == "NaN"   ~ NaN,
      value == "TRUE"  ~ TRUE,
      value == "FALSE" ~ FALSE,
      ~ strsplit(value, "\\s*,\\s*")[[1]]
    )

  })

}

renv_settings_read_impl_json <- function(path) {

  json <- catch(renv_json_read(path))
  if (inherits(json, "error")) {
    warning(json)
    return(NULL)
  }

  json

}

renv_settings_get <- function(project, name = NULL, default = NULL) {

  # when 'name' is NULL, return all settings
  if (is.null(name)) {
    names <- ls(envir = the$settings, all.names = TRUE)
    settings <- lapply(names, renv_settings_get, project = project)
    names(settings) <- names
    return(settings[order(names(settings))])
  }

  # check for an override via option
  override <- renv_options_override("renv.settings", name)
  if (!is.null(override))
    return(override)

  # try to read settings file
  path <- renv_settings_path(project)
  settings <- renv_settings_read(path)
  if (!is.null(settings))
    return(settings[[name]])

  # if a 'default' value was provided, use it
  if (!missing(default))
    return(default)

  # no value recorded; use default
  renv_settings_default(name)

}

renv_settings_set <- function(project, name, value, persist = TRUE) {

  # read old settings
  settings <- renv_settings_get(project)

  # update setting value
  old <- settings[[name]] %||% renv_settings_default(name)
  new <- renv_settings_validate(name, value)
  settings[[name]] <- new

  # persist if requested
  if (persist)
    renv_settings_persist(project, settings)

  # save session-cached value
  path <- renv_settings_path(project)
  value <- renv_filebacked_set("renv_settings_read", path, settings)

  # invoke update callback if value changed
  if (!identical(old, new))
    renv_settings_updated(project, name, old, new)

  # return value
  invisible(value)

}

renv_settings_updated <- function(project, name, old, new) {
  update <- the$settings[[name]]$update %||% function(...) {}
  update(project, old, new)
}

renv_settings_persist <- function(project, settings) {

  path <- renv_settings_path(project)
  settings <- settings[order(names(settings))]

  # figure out which settings are scalar
  scalar <- map_lgl(names(settings), function(name) {
    the$settings[[name]]$scalar
  })

  # use that to determine which objects should be boxed
  config <- renv_json_config(box = names(settings)[!scalar])

  # write json
  ensure_parent_directory(path)
  renv_json_write(
    object = settings,
    config = config,
    file   = path
  )

}

renv_settings_merge <- function(settings, merge) {
  settings[names(merge)] <- merge
  settings
}

renv_settings_path <- function(project) {
  renv_paths_settings(project = project)
}


# nocov start

renv_settings_updated_cache <- function(project, old, new) {

  # if the cache is being disabled, then copy packages from their
  # symlinks back into the library. note that we don't use symlinks
  # on windows (we use hard links) so in that case there's nothing
  # to be done
  if (renv_platform_windows())
    return(FALSE)

  library <- renv_paths_library(project = project)
  pkgpaths <- list.files(library, full.names = TRUE)
  cachepaths <- map_chr(pkgpaths, renv_cache_path)
  names(pkgpaths) <- cachepaths

  if (empty(pkgpaths)) {
    fmt <- "- The cache has been %s for this project."
    writef(fmt, if (new) "enabled" else "disabled")
    return(TRUE)
  }

  printf("- Synchronizing project library with the cache ... ")

  if (new) {

    # enabling the cache: for any package in the project library, replace
    # that copy with a symlink into the cache, moving the associated package
    # into the cache if appropriate

    # ignore existing symlinks; only copy 'real' packages into the cache
    pkgtypes <- renv_file_type(pkgpaths)
    cachepaths <- cachepaths[pkgtypes != "symlink"]

    # move packages from project library into cache
    callback <- renv_progress_callback(renv_cache_move, length(cachepaths))
    enumerate(cachepaths, callback, overwrite = FALSE)

  } else {

    # disabling the cache: for any package which is a symlink into the cache,
    # replace that symlink with a copy of the cached package

    # figure out which package directories are symlinks
    pkgtypes <- renv_file_type(pkgpaths)
    pkgpaths <- pkgpaths[pkgtypes == "symlink"]

    # remove the existing symlinks
    unlink(pkgpaths)

    # overwrite these symlinks with packages from the cache
    callback <- renv_progress_callback(renv_file_copy, length(pkgpaths))
    enumerate(pkgpaths, callback, overwrite = TRUE)

  }

  writef("Done!")

  fmt <- "- The cache has been %s for this project."
  writef(fmt, if (new) "enabled" else "disabled")

}

renv_settings_updated_ignore <- function(project, old, new) {
  renv_infrastructure_write_gitignore(project = project)
}

renv_settings_migrate <- function(project) {

  old <- renv_paths_renv("settings.dcf",  project = project)
  if (!file.exists(old))
    return()

  new <- renv_paths_renv("settings.json", project = project)
  if (file.exists(new))
    return()

  # update settings
  settings <- renv_settings_read(old)
  renv_settings_persist(project, settings)

}

renv_settings_impl <- function(name, default, scalar, validate, coerce, update) {

  force(name)

  the$settings[[name]] <- list(
    default  = default,
    coerce   = coerce,
    scalar   = scalar,
    validate = validate,
    update   = update
  )

  function(value, project = NULL, persist = TRUE) {
    project <- renv_project_resolve(project)
    if (missing(value))
      renv_settings_get(project, name)
    else
      renv_settings_set(project, name, value, persist)
  }

}


# nocov end

#' Project settings
#'
#' @description
#' Define project-local settings that can be used to adjust the behavior of
#' renv with your particular project.
#'
#' * Get the current value of a setting with (e.g.) `settings$snapshot.type()`
#' * Set current value of a setting with (e.g.)
#'   `settings$snapshot.type("explicit")`.
#'
#' Settings are automatically persisted across project sessions by writing to
#' `renv/settings.json`. You can also edit this file by hand, but you'll need
#' to restart the session for those changes to take effect.
#'
#' ## `bioconductor.version`
#'
#' The Bioconductor version to be used with this project. Use this if you'd
#' like to lock the version of Bioconductor used on a per-project basis.
#' When unset, renv will try to infer the appropriate Bioconductor release
#' using the BiocVersion package if installed; if not, renv uses
#' `BiocManager::version()` to infer the appropriate Bioconductor version.
#'
#' ## `external.libraries`
#'
#' A vector of library paths, to be used in addition to the project's own
#' private library. This can be useful if you have a package available for use
#' in some system library, but for some reason renv is not able to install
#' that package (e.g. sources or binaries for that package are not publicly
#' available, or you have been unable to orchestrate the pre-requisites for
#' installing some packages from source on your machine).
#'
#' ## `ignored.packages`
#'
#' A vector of packages, which should be ignored when attempting to snapshot
#' the project's private library. Note that if a package has already been
#' added to the lockfile, that entry in the lockfile will not be ignored.
#'
#' ## `package.dependency.fields`
#'
#' When explicitly installing a package with `install()`, what fields
#' should be used to determine that packages dependencies? The default
#' uses `Imports`, `Depends` and `LinkingTo` fields, but you also want
#' to install `Suggests` dependencies for a package, you can set this to
#' `c("Imports", "Depends", "LinkingTo", "Suggests")`.
#'
#' ## `ppm.enabled`
#'
#' Enable [Posit Package Manager](https://packagemanager.posit.co/)
#' integration in this project? When `TRUE`, renv will attempt to transform
#' repository URLs used by PPM into binary URLs as appropriate for the
#' current Linux platform. Set this to `FALSE` if you'd like to continue using
#' source-only PPM URLs, or if you find that renv is improperly transforming
#' your repository URLs. You can still set and use PPM repositories with this
#' option disabled; it only controls whether renv tries to transform source
#' repository URLs into binary URLs on your behalf.
#'
#' ## `ppm.ignored.urls`
#'
#' When [Posit Package Manager](https://packagemanager.posit.co/) integration
#' is enabled, `renv` will attempt to transform source repository URLs into
#' binary repository URLs. This setting can be used if you'd like to avoid this
#' transformation with some subset of repository URLs.
#'
#' ## `r.version`
#'
#' The version of \R to encode within the lockfile. This can be set as a
#' project-specific option if you'd like to allow multiple users to use
#' the same renv project with different versions of \R. renv will
#' still warn the user if the major + minor version of \R used in a project
#' does not match what is encoded in the lockfile.
#'
#' ## `snapshot.type`
#'
#' The type of snapshot to perform by default. See [snapshot] for more
#' details.
#'
#' ## `use.cache`
#'
#' Enable the renv package cache with this project. When active, renv will
#' install packages into a global cache, and link packages from the cache into
#' your renv projects as appropriate. This can greatly save on disk space
#' and install time when for \R packages which are used across multiple
#' projects in the same environment.
#'
#' ## `vcs.manage.ignores`
#'
#' Should renv attempt to manage the version control system's ignore files
#' (e.g. `.gitignore`) within this project? Set this to `FALSE` if you'd
#' prefer to take control. Note that if this setting is enabled, you will
#' need to manually ensure internal data in the project's `renv/` folder
#' is explicitly ignored.
#'
#' ## `vcs.ignore.cellar`
#'
#' Set whether packages within a project-local package cellar are excluded
#' from version control. See `vignette("cellar", package = "renv")` for
#' more information.
#'
#' ## `vcs.ignore.library`
#'
#' Set whether the renv project library is excluded from version control.
#'
#' ## `vcs.ignore.local`
#'
#' Set whether renv project-specific local sources are excluded from version
#' control.
#'
#' # Defaults
#'
#' You can change the default values of these settings for newly-created renv
#' projects by setting \R options for `renv.settings` or `renv.settings.<name>`.
#' For example:
#'
#' ```R
#' options(renv.settings = list(snapshot.type = "all"))
#' options(renv.settings.snapshot.type = "all")
#' ```
#'
#' If both of the `renv.settings` and `renv.settings.<name>` options are set
#' for a particular key, the option associated with `renv.settings.<name>` is
#' used instead. We recommend setting these in an appropriate startup profile,
#' e.g. `~/.Rprofile` or similar.
#'
#' @return
#'   A named list of renv settings.
#'
#' @format NULL
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # view currently-ignored packaged
#' renv::settings$ignored.packages()
#'
#' # ignore a set of packages
#' renv::settings$ignored.packages("devtools", persist = FALSE)
#'
#' }
settings <- list(

  bioconductor.version = renv_settings_impl(
    name     = "bioconductor.version",
    default  = NULL,
    scalar   = TRUE,
    validate = is.character,
    coerce   = as.character,
    update   = NULL
  ),

  ignored.packages = renv_settings_impl(
    name     = "ignored.packages",
    default  = character(),
    scalar   = FALSE,
    validate = is.character,
    coerce   = as.character,
    update   = NULL
  ),

  external.libraries = renv_settings_impl(
    name     = "external.libraries",
    default  = character(),
    scalar   = FALSE,
    validate = is.character,
    coerce   = as.character,
    update   = NULL
  ),

  package.dependency.fields = renv_settings_impl(
    name     = "package.dependency.fields",
    default  = c("Imports", "Depends", "LinkingTo"),
    scalar   = FALSE,
    validate = is.character,
    coerce   = as.character,
    update   = NULL
  ),

  ppm.enabled = renv_settings_impl(
    name     = "ppm.enabled",
    default  = NULL,
    scalar   = TRUE,
    validate = is.logical,
    coerce   = as.logical,
    update   = FALSE
  ),

  ppm.ignored.urls = renv_settings_impl(
    name     = "ppm.ignored.urls",
    default  = NULL,
    scalar   = FALSE,
    validate = is.character,
    coerce   = as.character,
    update   = NULL
  ),

  r.version = renv_settings_impl(
    name     = "r.version",
    default  = NULL,
    scalar   = TRUE,
    validate = is.character,
    coerce   = as.character,
    update   = NULL
  ),

  snapshot.type = renv_settings_impl(
    name     = "snapshot.type",
    default  = "implicit",
    scalar   = TRUE,
    validate = c("all", "custom", "implicit", "explicit", "packrat", "simple"),
    coerce   = as.character,
    update   = NULL
  ),

  use.cache = renv_settings_impl(
    name     = "use.cache",
    default  = TRUE,
    scalar   = TRUE,
    validate = is.logical,
    coerce   = as.logical,
    update   = renv_settings_updated_cache
  ),

  vcs.manage.ignores = renv_settings_impl(
    name     = "vcs.manage.ignores",
    default  = TRUE,
    scalar   = TRUE,
    validate = is.logical,
    coerce   = as.logical,
    update   = NULL
  ),

  vcs.ignore.cellar = renv_settings_impl(
    name     = "vcs.ignore.cellar",
    default  = TRUE,
    scalar   = TRUE,
    validate = is.logical,
    coerce   = as.logical,
    update   = renv_settings_updated_ignore
  ),

  vcs.ignore.library = renv_settings_impl(
    name     = "vcs.ignore.library",
    default  = TRUE,
    scalar   = TRUE,
    validate = is.logical,
    coerce   = as.logical,
    update   = renv_settings_updated_ignore
  ),

  vcs.ignore.local = renv_settings_impl(
    name     = "vcs.ignore.local",
    default  = TRUE,
    scalar   = TRUE,
    validate = is.logical,
    coerce   = as.logical,
    update   = renv_settings_updated_ignore
  )

)


# shell.R --------------------------------------------------------------------


renv_shell_quote <- function(x) {
  if (length(x))
    shQuote(x)
}

renv_shell_path <- function(x) {
  if (length(x))
    shQuote(path.expand(x))
}


# shims.R --------------------------------------------------------------------


the$shims <- new.env(parent = emptyenv())

renv_shim_install_packages <- function(pkgs, ...) {

  # place Rtools on PATH
  renv_scope_rtools()

  # currently we only handle the case where only 'pkgs' was specified
  if (missing(pkgs) || nargs() != 1) {
    call <- sys.call()
    call[[1L]] <- quote(utils::install.packages)
    return(eval(call, envir = parent.frame()))
  }

  # otherwise, we get to handle it
  install(pkgs)

}

renv_shim_update_packages <- function(lib.loc = NULL, ...) {

  # handle only 0-argument case
  if (nargs() != 0) {
    call <- sys.call()
    call[[1L]] <- quote(utils::update.packages)
    return(eval(call, envir = parent.frame()))
  }

  update(library = lib.loc)

}

renv_shim_remove_packages <- function(pkgs, lib) {

  # handle single-argument case
  if (nargs() != 1) {
    call <- sys.call()
    call[[1L]] <- quote(utils::remove.packages)
    return(eval(call, envir = parent.frame()))
  }

  remove(pkgs)

}

renv_shim_create <- function(shim, sham) {
  formals(shim) <- formals(sham)
  shim
}

renv_shims_enabled <- function(project) {
  config$shims.enabled()
}

renv_shims_activate <- function() {

  renv_shims_deactivate()

  install_shim <- renv_shim_create(renv_shim_install_packages, utils::install.packages)
  assign("install.packages", install_shim, envir = the$shims)

  update_shim <- renv_shim_create(renv_shim_update_packages, utils::update.packages)
  assign("update.packages", update_shim, envir = the$shims)

  remove_shim <- renv_shim_create(renv_shim_remove_packages, utils::remove.packages)
  assign("remove.packages", remove_shim, envir = the$shims)

  args <- list(the$shims, name = "renv:shims", warn.conflicts = FALSE)
  do.call(base::attach, args)

}

renv_shims_deactivate <- function() {
  while ("renv:shims" %in% search())
    detach("renv:shims")
}


# snapshot-auto.R ------------------------------------------------------------


# information about the project library; used to detect whether
# the library appears to have been modified or updated
the$library_info <- NULL

# are we forcing automatic snapshots?
the$auto_snapshot_forced <- FALSE

# did the last attempt at an automatic snapshot fail?
the$auto_snapshot_failed <- FALSE

# are we currently running an automatic snapshot?
the$auto_snapshot_running <- FALSE

# is the next automatic snapshot suppressed?
the$auto_snapshot_suppressed <- FALSE

# nocov start
renv_snapshot_auto <- function(project) {

  # set some state so we know we're running
  the$auto_snapshot_running <- TRUE
  defer(the$auto_snapshot_running <- FALSE)

  # passed pre-flight checks; snapshot the library
  updated <- withCallingHandlers(

    tryCatch(
      renv_snapshot_auto_impl(project),
      error = function(err) FALSE
    ),

    cancel = function() FALSE

  )

  if (updated) {
    lockfile <- renv_path_aliased(renv_lockfile_path(project))
    writef("- Automatic snapshot has updated '%s'.", lockfile)
  }

  invisible(updated)

}

renv_snapshot_auto_impl <- function(project) {

  # validation messages can be noisy; turn off for auto snapshot
  renv_scope_options(
    renv.config.snapshot.validate = FALSE,
    renv.verbose = FALSE
  )

  # get current lockfile state
  lockfile <- renv_paths_lockfile(project)
  old <- file.info(lockfile, extra_cols = FALSE)$mtime

  # perform snapshot without prompting
  snapshot(project = project, prompt = FALSE)

  # check for change in lockfile
  new <- file.info(lockfile, extra_cols = FALSE)$mtime
  old != new

}

renv_snapshot_auto_enabled <- function(project = renv_project_get()) {

  # respect override
  if (the$auto_snapshot_forced)
    return(TRUE)

  # respect config setting
  enabled <- config$auto.snapshot(project = project)
  if (!enabled)
    return(FALSE)

  # only snapshot interactively
  if (!interactive())
    return(FALSE)

  # only automatically snapshot the current project
  if (!renv_project_loaded(project))
    return(FALSE)

  # don't auto-snapshot if the project hasn't been initialized
  if (!renv_project_initialized(project = project))
    return(FALSE)

  # don't auto-snapshot if we don't have a library
  library <- renv_paths_library(project = project)
  if (!file.exists(library))
    return(FALSE)

  # don't auto-snapshot unless the active library is the project library
  if (!renv_file_same(renv_libpaths_active(), library))
    return(FALSE)

  TRUE

}

renv_snapshot_auto_update <- function(project = renv_project_get() ) {

  # check for enabled
  if (!renv_snapshot_auto_enabled(project = project))
    return(FALSE)

  # get path to project library
  libpath <- renv_paths_library(project = project)
  if (!file.exists(libpath))
    return(FALSE)

  # list files + get file info for files in project library
  info <- renv_file_info(libpath)

  # only keep relevant fields
  fields <- c("size", "mtime", "ctime")
  new <- c(info[fields])

  # update our cached info
  old <- the$library_info
  the$library_info <- new

  # if we've suppressed the next automatic snapshot, bail here
  if (the$auto_snapshot_suppressed) {
    the$auto_snapshot_suppressed <- FALSE
    return(FALSE)
  }

  # report if things have changed
  !is.null(old) && !identical(old, new)

}

renv_snapshot_task <- function() {

  # if the previous snapshot attempt failed, do nothing
  if (the$auto_snapshot_failed)
    return(FALSE)

  # treat warnings as errors in this scope
  renv_scope_options(warn = 2L)

  # attempt automatic snapshot, but disable on failure
  tryCatch(
    renv_snapshot_task_impl(),
    error = function(cnd) {
      caution("Error generating automatic snapshot: %s", conditionMessage(cnd))
      caution("Automatic snapshots will be disabled. Use `renv::snapshot()` to manually update the lockfile.")
      the$auto_snapshot_failed <- TRUE
    }
  )

}

renv_snapshot_task_impl <- function() {

  # check for active renv project
  project <- renv_project_get()
  if (is.null(project))
    return(invisible(FALSE))

  # see if library state has updated
  updated <- renv_snapshot_auto_update(project = project)
  if (!updated)
    return(invisible(FALSE))

  # library has updated; perform auto snapshot
  renv_snapshot_auto(project = project)

}

renv_snapshot_auto_suppress_next <- function() {

  # if we're currently running an automatic snapshot, then nothing to do
  if (the$auto_snapshot_running)
    return()

  # otherwise, set the suppressed flag
  the$auto_snapshot_suppressed <- TRUE

}

# nocov end


# snapshot.R -----------------------------------------------------------------


# controls whether hashes are computed when computing a snapshot
# can be scoped to FALSE when hashing is not necessary
the$auto_snapshot_hash <- TRUE

#' Record current state of the project library in the lockfile
#'
#' @description
#' Call `renv::snapshot()` to update a [lockfile] with the current state of
#' dependencies in the project library. The lockfile can be used to later
#' [restore] these dependencies as required.
#'
#' It's also possible to call `renv::snapshot()` with a non-renv project,
#' in which case it will record the current state of dependencies in the
#' current library paths. This makes it possible to [restore] the current packages,
#' providing lightweight portability and reproducibility without isolation.
#'
#' If you want to automatically snapshot after each change, you can
#' set `config$config$auto.snapshot(TRUE)`, see `?config` for more details.
#'
#' # Snapshot types
#'
#' Depending on how you prefer to manage dependencies, you might prefer
#' selecting a different snapshot mode. The modes available are as follows:
#'
#' \describe{
#'
#' \item{`"implicit"`}{
#' (The default) Capture only packages which appear to be used in your project,
#' as determined by `renv::dependencies()`. This ensures that only the packages
#' actually required by your project will enter the lockfile; the downside
#' if it might be slow if your project contains a large number of files.
#' If speed becomes an issue, you might consider using `.renvignore` files to
#' limit which files renv uses for dependency discovery, or switching to
#' explicit mode, as described next.
#' }
#'
#' \item{`"explicit"`}{
#' Only capture packages which are explicitly listed in the project
#' `DESCRIPTION` file. This workflow is recommended for users who wish to
#' manage their project's \R package dependencies directly.
#' }
#'
#' \item{`"all"`}{
#' Capture all packages within the active \R libraries in the lockfile.
#' This is the quickest and simplest method, but may lead to undesired
#' packages (e.g. development dependencies) entering the lockfile.
#' }
#'
#' \item{`"custom"`}{
#' Like `"implicit"`, but use a custom user-defined filter instead. The filter
#' should be specified by the \R option `renv.snapshot.filter`, and should
#' either be a character vector naming a function (e.g. `"package::method"`),
#' or be a function itself. The function should only accept one argument (the
#' project directory), and should return a vector of package names to include
#' in the lockfile.
#' }
#'
#' }
#'
#' You can change the snapshot type for the current project with [settings()].
#' For example, the following code will switch to using `"explicit"` snapshots:
#'
#' ```
#' renv::settings$snapshot.type("explicit")
#' ```
#'
#' When the `packages` argument is set, `type` is ignored, and instead only the
#' requested set of packages, and their recursive dependencies, will be written
#' to the lockfile.
#'
#' @inherit renv-params
#'
#' @param library The \R libraries to snapshot. When `NULL`, the active \R
#'   libraries (as reported by `.libPaths()`) are used.
#'
#' @param lockfile The location where the generated lockfile should be written.
#'   By default, the lockfile is written to a file called `renv.lock` in the
#'   project directory. When `NULL`, the lockfile (as an \R object) is returned
#'   directly instead.
#'
#' @param type The type of snapshot to perform:
#'   * `"implict"`, (the default), uses all packages captured by [dependencies()].
#'   * `"explicit"` uses packages recorded in `DESCRIPTION`.
#'   * `"all"` uses all packages in the project library.
#'   * `"custom` uses a custom filter.
#'
#'   See **Snapshot type** below for more details.
#'
#' @param repos The \R repositories to be recorded in the lockfile. Defaults
#'   to the currently active package repositories, as retrieved by
#'   `getOption("repos")`.
#'
#' @param packages A vector of packages to be included in the lockfile. When
#'   `NULL` (the default), all packages relevant for the type of snapshot being
#'   performed will be included. When set, the `type` argument is ignored.
#'   Recursive dependencies of the specified packages will be added to the
#'   lockfile as well.
#'
#' @param exclude A vector of packages to be explicitly excluded from the lockfile.
#'   Note that transitive package dependencies will always be included, to avoid
#'   potentially creating an incomplete / non-functional lockfile.
#'
#' @param update Boolean; if the lockfile already exists, then attempt to update
#'   that lockfile without removing any prior package records.
#'
#' @param force Boolean; force generation of a lockfile even when pre-flight
#'   validation checks have failed?
#'
#' @param reprex Boolean; generate output appropriate for embedding the lockfile
#'   as part of a [reprex](https://www.tidyverse.org/help/#reprex)?
#'
#' @return The generated lockfile, as an \R object (invisibly). Note that
#'   this function is normally called for its side effects.
#'
#' @family reproducibility
#'
#' @export
#'
#' @example examples/examples-init.R
snapshot <- function(project  = NULL,
                     ...,
                     library  = NULL,
                     lockfile = paths$lockfile(project = project),
                     type     = settings$snapshot.type(project = project),
                     repos    = getOption("repos"),
                     packages = NULL,
                     exclude  = NULL,
                     prompt   = interactive(),
                     update   = FALSE,
                     force    = FALSE,
                     reprex   = FALSE)
{
  renv_consent_check()
  renv_scope_error_handler()
  renv_dots_check(...)

  renv_snapshot_auto_suppress_next()

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)
  renv_scope_verbose_if(prompt)

  repos <- renv_repos_validate(repos)
  renv_scope_options(repos = repos)

  if (!is.null(lockfile))
    renv_activate_prompt("snapshot", library, prompt, project)

  libpaths <- renv_path_normalize(library %||% renv_libpaths_all())
  if (config$snapshot.validate())
    renv_snapshot_preflight(project, libpaths)

  # when packages is set, we treat this as an 'all' type snapshot, but
  # with explicit package filters turned on
  if (!is.null(packages)) {

    if (!missing(type)) {
      fmt <- "packages argument is set; type argument %s will be ignored"
      warningf(fmt, stringify(type))
    }

    type <- "packages"

  }

  alt <- new <- renv_lockfile_create(
    project  = project,
    type     = type,
    libpaths = libpaths,
    packages = packages,
    exclude  = exclude,
    prompt   = prompt,
    force    = force
  )

  if (is.null(lockfile))
    return(new)

  # if running as part of 'reprex', then render output inline
  if (reprex)
    return(renv_snapshot_reprex(new))

  # check for missing dependencies and warn if any are discovered
  # (note: use 'new' rather than 'alt' here as we don't want to attempt
  # validation on uninstalled packages)
  valid <- renv_snapshot_validate(project, new, libpaths)
  renv_snapshot_validate_report(valid, prompt, force)

  # get prior lockfile state
  old <- list()
  if (file.exists(lockfile)) {

    # read a pre-existing lockfile (if any)
    old <- renv_lockfile_read(lockfile)

    # preserve records from alternate OSes in lockfile
    alt <- renv_snapshot_preserve(old, new)

    # check if there are any changes in the lockfile
    diff <- renv_lockfile_diff(old, alt)
    if (empty(diff)) {
      writef("- The lockfile is already up to date.")
      return(renv_snapshot_successful(alt, prompt, project))
    }

  }

  # update new reference
  new <- alt

  # if we're only updating the lockfile, then merge any missing records
  # from 'old' back into 'new'
  if (update)
    for (package in names(old$Packages))
      new$Packages[[package]] <- new$Packages[[package]] %||% old$Packages[[package]]

  # report actions to the user
  actions <- renv_lockfile_diff_packages(old, new)
  if (prompt || renv_verbose())
    renv_snapshot_report_actions(actions, old, new)

  # request user confirmation
  cancel_if(length(actions) && file.exists(lockfile) && prompt && !proceed())

  # write it out
  ensure_parent_directory(lockfile)
  renv_lockfile_write(new, file = lockfile)

  # ensure the lockfile is .Rbuildignore-d
  renv_infrastructure_write_rbuildignore(project)

  # ensure the activate script is up-to-date
  renv_infrastructure_write_activate(project, create = FALSE)

  # return new records
  renv_snapshot_successful(new, prompt, project)
}

renv_snapshot_preserve <- function(old, new) {
  records <- filter(old$Packages, renv_snapshot_preserve_impl)
  if (length(records))
    new$Packages[names(records)] <- records
  new
}

renv_snapshot_preserve_impl <- function(record) {

  ostype <- tolower(record[["OS_type"]] %||% "")
  if (!nzchar(ostype))
    return(FALSE)

  altos <- if (renv_platform_unix()) "windows" else "unix"
  identical(ostype, altos)

}

renv_snapshot_preflight <- function(project, libpaths) {
  lapply(libpaths, renv_snapshot_preflight_impl, project = project)
}

renv_snapshot_preflight_impl <- function(project, library) {
  renv_snapshot_preflight_library_exists(project, library)
}

renv_snapshot_preflight_library_exists <- function(project, library) {

  # check that we have a directory
  type <- renv_file_type(library, symlinks = FALSE)
  if (type == "directory")
    return(TRUE)

  # if the file exists but isn't a directory, fail
  if (nzchar(type)) {
    fmt <- "library '%s' exists but is not a directory"
    stopf(fmt, renv_path_aliased(library))
  }

  # the directory doesn't exist; perhaps the user hasn't called init
  if (identical(library, renv_paths_library(project = project))) {
    fmt <- "project '%s' has no private library -- have you called `renv::init()`?"
    stopf(fmt, renv_path_aliased(project))
  }

  # user tried to snapshot arbitrary but missing path
  fmt <- "library '%s' does not exist; cannot proceed"
  stopf(fmt, renv_path_aliased(library))

}

renv_snapshot_validate <- function(project, lockfile, libpaths) {

  # allow user to disable snapshot validation, just in case
  enabled <- config$snapshot.validate()
  if (!enabled)
    return(TRUE)

  methods <- list(
    renv_snapshot_validate_bioconductor,
    renv_snapshot_validate_dependencies_available,
    renv_snapshot_validate_dependencies_compatible,
    renv_snapshot_validate_sources
  )

  ok <- map_lgl(methods, function(method) {
    tryCatch(
      method(project, lockfile, libpaths),
      error = function(e) { warning(e); FALSE }
    )
  })

  all(ok)

}

renv_snapshot_validate_report <- function(valid, prompt, force) {

  # nothing to do if everything is valid
  if (valid) {
    dlog("snapshot", "passed pre-flight validation checks")
    return(TRUE)
  }

  # if we're forcing snapshot, ignore the failures
  if (force) {
    dlog("snapshot", "ignoring error in pre-flight validation checks as 'force = TRUE'")
    return(TRUE)
  }

  # in interactive sessions, if 'prompt' is set, then ask the user
  # if they would like to proceed
  if (interactive() && !is_testing() && prompt) {
    cancel_if(!proceed())
    return(TRUE)
  }

  # otherwise, bail on error (need to use 'force = TRUE')
  stop("aborting snapshot due to pre-flight validation failure")

}

# nocov start
renv_snapshot_validate_bioconductor <- function(project, lockfile, libpaths) {

  ok <- TRUE

  # check whether any packages are installed from Bioconductor
  records <- renv_lockfile_records(lockfile)
  sources <- extract_chr(records, "Source")
  if (!"Bioconductor" %in% sources)
    return(ok)

  # check for BiocManager or BiocInstaller
  package <- renv_bioconductor_manager()
  if (!package %in% names(records)) {

    text <- c(
      "One or more Bioconductor packages are used in your project,",
      "but the %s package is not available.",
      "",
      "Consider installing %s before snapshot.",
      ""
    )
    caution(text, package)

    ok <- FALSE
  }

  # check that Bioconductor packages are from correct release
  version <-
    lockfile$Bioconductor$Version %||%
    renv_bioconductor_version(project = project)

  biocrepos <- renv_bioconductor_repos(version = version)
  renv_scope_options(repos = biocrepos)

  # collect Bioconductor records
  bioc <- records %>%
    filter(function(record) renv_record_source(record) == "bioconductor") %>%
    map(function(record) record[c("Package", "Version")]) %>%
    bind()

  # collect latest versions of these packages
  bioc$Latest <- vapply(bioc$Package, function(package) {
    entry <- catch(renv_available_packages_latest(package))
    if (inherits(entry, "error"))
      return("<NA>")
    entry$Version
  }, FUN.VALUE = character(1))

  # check for version mismatches (allow mismatch in minor version)
  bioc$Mismatch <- mapply(function(current, latest) {

    if (identical(latest, "<NA>"))
      return(TRUE)

    current <- renv_version_maj_min(current)
    latest <- renv_version_maj_min(latest)
    current != latest

  }, bioc$Version, bioc$Latest)

  bad <- bioc[bioc$Mismatch, ]
  if (nrow(bad)) {

    fmt <- "%s [installed %s != latest %s]"
    msg <- sprintf(fmt, format(bad$Package), format(bad$Version), bad$Latest)
    caution_bullets(
      "The following Bioconductor packages appear to be from a separate Bioconductor release:",
      msg,
      c(
        "renv may be unable to restore these packages.",
        paste("Bioconductor version:", version)
      )
    )

    ok <- FALSE
  }

  ok

}
# nocov end

renv_snapshot_validate_dependencies_available <- function(project, lockfile, libpaths) {

  # use library to collect package dependency versions
  records <- renv_lockfile_records(lockfile)
  packages <- extract_chr(records, "Package")
  locs <- find.package(packages, lib.loc = libpaths, quiet = TRUE)
  deps <- bapply(locs, renv_dependencies_discover_description)
  if (empty(deps))
    return(TRUE)

  splat <- split(deps, deps$Package)

  # exclude base R packages
  splat <- splat[renv_vector_diff(names(splat), renv_packages_base())]

  # check for required packages not currently installed
  requested <- names(splat)
  missing <- renv_vector_diff(requested, packages)
  if (empty(missing))
    return(TRUE)

  # exclude ignored packages
  missing <- renv_vector_diff(missing, settings$ignored.packages(project = project))
  if (empty(missing))
    return(TRUE)

  usedby <- map_chr(missing, function(package) {

    revdeps <- sort(unique(basename(deps$Source)[deps$Package == package]))

    items <- revdeps; limit <- 3L
    if (length(revdeps) > limit) {
      rest <- length(revdeps) - limit
      suffix <- paste("and", length(revdeps) - 3L, plural("other", rest))
      items <- c(revdeps[seq_len(limit)], suffix)
    }

    paste(items, collapse = ", ")

  })

  caution_bullets(
    "The following required packages are not installed:",
    sprintf("%s  [required by %s]", format(missing), usedby),
    "Consider reinstalling these packages before snapshotting the lockfile."
  )

  FALSE

}

renv_snapshot_validate_dependencies_compatible <- function(project, lockfile, libpaths) {

  # use library to collect package dependency versions
  records <- renv_lockfile_records(lockfile)
  packages <- extract_chr(records, "Package")
  locs <- find.package(packages, lib.loc = libpaths, quiet = TRUE)
  deps <- bapply(locs, renv_dependencies_discover_description)
  if (empty(deps))
    return(TRUE)

  splat <- split(deps, deps$Package)

  # exclude base R packages
  splat <- splat[renv_vector_diff(names(splat), renv_packages_base())]

  # collapse requirements for each package
  bad <- enumerate(splat, function(package, requirements) {

    # skip NULL records (should be handled above)
    record <- records[[package]]
    if (is.null(record))
      return(NULL)

    version <- record$Version

    # drop packages without explicit version requirement
    requirements <- requirements[nzchar(requirements$Require), ]
    if (nrow(requirements) == 0)
      return(NULL)

    # add in requested version
    requirements$Requested <- version

    # generate expressions to evaluate
    fmt <- "package_version('%s') %s package_version('%s')"
    code <- with(requirements, sprintf(fmt, Requested, Require, Version))
    parsed <- parse(text = code)
    ok <- map_lgl(parsed, eval, envir = baseenv())

    # return requirements that weren't satisfied
    requirements[!ok, ]

  })

  bad <- bind(bad)
  if (empty(bad))
    return(TRUE)

  package  <- basename(bad$Source)
  requires <- sprintf("%s (%s %s)", bad$Package, bad$Require, bad$Version)
  request  <- bad$Requested

  fmt <- "%s requires %s, but version %s is installed"
  txt <- sprintf(fmt, format(package), format(requires), format(request))
  caution_bullets(
    "The following package(s) have unsatisfied dependencies:",
    txt,
    "Consider updating the required dependencies as appropriate."
  )

  FALSE

}

renv_snapshot_validate_sources <- function(project, lockfile, libpaths) {
  records <- renv_lockfile_records(lockfile)
  renv_check_unknown_source(records, project)
}

# NOTE: if packages are found in multiple libraries,
# then the first package found in the library paths is
# kept and others are discarded
renv_snapshot_libpaths <- function(libpaths = NULL,
                                   project  = NULL)
{
  dynamic(
    key   = list(libpaths = libpaths, project = project),
    value = renv_snapshot_libpaths_impl(libpaths, project)
  )
}

renv_snapshot_libpaths_impl <- function(libpaths = NULL,
                                        project  = NULL)
{
  records <- uapply(
    libpaths,
    renv_snapshot_library,
    project = project
  )

  dupes <- duplicated(names(records))
  records[!dupes]
}

renv_snapshot_library <- function(library = NULL,
                                  records = TRUE,
                                  project = NULL)
{
  # list packages in the library
  library <- renv_path_normalize(library %||% renv_libpaths_active())
  paths <- list.files(library, full.names = TRUE)

  # remove 'base' packages
  paths <- paths[!basename(paths) %in% renv_packages_base()]

  # remove ignored packages
  ignored <- renv_project_ignored_packages(project = project)
  paths <- paths[!basename(paths) %in% ignored]

  # remove paths that are not valid package names
  pattern <- sprintf("^%s$", .standard_regexps()$valid_package_name)
  paths <- paths[grep(pattern, basename(paths))]

  # validate the remaining set of packages
  valid <- renv_snapshot_library_diagnose(library, paths)

  # remove duplicates (so only first package entry discovered in library wins)
  duplicated <- duplicated(basename(valid))
  packages <- valid[!duplicated]

  # early exit if we're just collecting the list of packages
  if (!records)
    return(basename(packages))

  # snapshot description files
  descriptions <- file.path(packages, "DESCRIPTION")
  records <- lapply(descriptions, compose(catch, renv_snapshot_description))
  names(records) <- basename(packages)

  # report any snapshot failures
  broken <- filter(records, inherits, what = "error")
  if (length(broken)) {

    messages <- map_chr(broken, conditionMessage)
    text <- sprintf("'%s': %s", names(broken), messages)
    caution_bullets(
      "renv was unable to snapshot the following packages:",
      text,
      "These packages will likely need to be repaired and / or reinstalled."
    )

    stopf("snapshot of library %s failed", renv_path_pretty(library))

  }

  # name results and return
  names(records) <- map_chr(records, `[[`, "Package")
  records

}

renv_snapshot_library_diagnose <- function(library, paths) {

  paths <- grep("00LOCK", paths, invert = TRUE, value = TRUE)
  paths <- renv_snapshot_library_diagnose_broken_link(library, paths)
  paths <- renv_snapshot_library_diagnose_tempfile(library, paths)
  paths <- renv_snapshot_library_diagnose_missing_description(library, paths)
  paths

}

renv_snapshot_library_diagnose_broken_link <- function(library, paths) {

  broken <- !file.exists(paths)
  if (!any(broken))
    return(paths)

  caution_bullets(
    "The following package(s) have broken symlinks into the cache:",
    basename(paths)[broken],
    "Use `renv::repair()` to try and reinstall these packages."
  )

  paths[!broken]

}

renv_snapshot_library_diagnose_tempfile <- function(library, paths) {

  names <- basename(paths)
  missing <- grepl("^file(?:\\w){12}", names)
  if (!any(missing))
    return(paths)

  caution_bullets(
    "The following folder(s) appear to be left-over temporary directories:",
    map_chr(paths[missing], renv_path_pretty),
    "Consider removing these folders from your R library."
  )

  paths[!missing]

}

renv_snapshot_library_diagnose_missing_description <- function(library, paths) {

  desc <- file.path(paths, "DESCRIPTION")
  missing <- !file.exists(desc)
  if (!any(missing))
    return(paths)

  caution_bullets(
    "The following package(s) are missing their DESCRIPTION files:",
    sprintf("%s [%s]", format(basename(paths[missing])), paths[missing]),
    c(
      "These may be left over from a prior, failed installation attempt.",
      "Consider removing or reinstalling these packages."
    )
  )

  paths[!missing]

}

renv_snapshot_description <- function(path = NULL, package = NULL) {

  # resolve path
  path <- path %||% {
    path <- renv_package_find(package)
    if (!nzchar(path))
      stopf("package '%s' is not installed", package)
  }

  # read and snapshot DESCRIPTION file
  dcf <- renv_description_read(path, package)
  renv_snapshot_description_impl(dcf, path)

}

renv_snapshot_description_impl <- function(dcf, path = NULL) {

  # figure out the package source
  source <- renv_snapshot_description_source(dcf)
  dcf[names(source)] <- source

  # check for required fields
  required <- c("Package", "Version", "Source")
  missing <- renv_vector_diff(required, names(dcf))
  if (length(missing)) {
    fmt <- "required fields %s missing from DESCRIPTION at path '%s'"
    stopf(fmt, paste(shQuote(missing), collapse = ", "), path %||% "<unknown>")
  }

  # generate a hash if we can
  dcf[["Hash"]] <- if (the$auto_snapshot_hash) {
    if (is.null(path))
      renv_hash_description_impl(dcf)
    else
      renv_hash_description(path)
  }

  # generate a Requirements field -- primarily for use by 'pak'
  fields <- c("Depends", "Imports", "LinkingTo")
  deps <- bind(map(dcf[fields], renv_description_parse_field))
  all <- unique(csort(unlist(deps$Package)))
  dcf[["Requirements"]] <- all

  # get remotes fields
  git <- grep("^git", names(dcf), value = TRUE)
  remotes <- grep("^Remote", names(dcf), value = TRUE)

  is_repo <-
    is.null(dcf[["RemoteType"]]) ||
    identical(dcf[["RemoteType"]], "standard")

  # only keep relevant fields
  extra <- c("Repository", "OS_type")
  all <- c(
    required, extra,
    if (!is_repo) c(remotes, git),
    "Requirements", "Hash"
  )
  keep <- renv_vector_intersect(all, names(dcf))

  # return as list
  as.list(dcf[keep])

}

renv_snapshot_description_source <- function(dcf) {

  # first, check for a declared remote type
  # treat 'standard' remotes as packages installed from a repository
  # https://github.com/rstudio/renv/issues/998
  type <- dcf[["RemoteType"]]
  repository <- dcf[["Repository"]]
  if (identical(type, "standard") && !is.null(repository))
    return(list(Source = "Repository", Repository = repository))
  else if (!is.null(type))
    return(list(Source = alias(type)))

  # packages from Bioconductor are normally tagged with a 'biocViews' entry;
  # use that to infer a Bioconductor source
  if (!is.null(dcf[["biocViews"]]))
    return(list(Source = "Bioconductor"))

  # check for a declared repository
  if (!is.null(repository))
    return(list(Source = "Repository", Repository = repository))

  # check for a valid package name
  package <- dcf[["Package"]]
  if (is.null(package))
    return(list(Source = "unknown"))

  # if this is running as part of the synchronization check, skip CRAN queries
  # https://github.com/rstudio/renv/issues/812
  if (the$project_synchronized_check_running)
    return(list(Source = "unknown"))

  # NOTE: this is sort of a hack that allows renv to declare packages which
  # appear to be installed from sources, but are actually available on the
  # active R package repositories, as though they were retrieved from that
  # repository. however, this is often what users intend, especially if
  # they haven't configured their repository to tag the packages it makes
  # available with the 'Repository:' field in the DESCRIPTION file.
  #
  # still, this has the awkward side-effect of a package's source potentially
  # depending on what repositories happen to be active at the time of snapshot,
  # so it'd be nice to tighten up the logic here if possible
  #
  # NOTE: local sources are also searched here as part of finding the 'latest'
  # available package, so we need to handle local packages discovered here
  tryCatch(
    renv_snapshot_description_source_hack(package, dcf),
    error = function(e) list(Source = "unknown")
  )

}

renv_snapshot_description_source_hack <- function(package, dcf) {

  # check cellar
  for (type in renv_package_pkgtypes()) {
    cellar <- renv_available_packages_cellar(type)
    if (package %in% cellar$Package)
      return(list(Source = "Cellar"))
  }

  # check available packages
  latest <- catch(renv_available_packages_latest(package))
  if (is.null(latest) || inherits(latest, "error"))
    return(list(Source = "unknown"))

  # check version; use unknown if it's too new
  if (renv_version_gt(dcf[["Version"]], latest[["Version"]]))
    return(list(Source = "unknown"))

  # ok, this package appears to be from a package repository
  list(Source = "Repository", Repository = latest[["Repository"]])

}


# nocov start
renv_snapshot_report_actions <- function(actions, old, new) {

  if (!renv_verbose())
    return(invisible())

  if (length(actions)) {
    lhs <- renv_lockfile_records(old)
    rhs <- renv_lockfile_records(new)
    renv_pretty_print_records_pair(
      "The following package(s) will be updated in the lockfile:",
      lhs[names(lhs) %in% names(actions)],
      rhs[names(rhs) %in% names(actions)]
    )
  }

  oldr <- old$R$Version
  newr <- new$R$Version
  rdiff <- renv_version_compare(oldr %||% "0", newr %||% "0")

  if (rdiff != 0L) {
    n <- max(nchar(names(actions)), 0)
    fmt <- paste("-", format("R", width = n), " ", "[%s -> %s]")
    msg <- sprintf(fmt, oldr %||% "*", newr %||% "*")
    writef(
      c("The version of R recorded in the lockfile will be updated:", msg, "")
    )
  }

}
# nocov end

# compute the package dependencies inferred for a project,
# respecting the snapshot type selected (or currently configured)
# for the associated project
renv_snapshot_dependencies <- function(project, type = NULL, dev = FALSE) {

  type <- type %||% settings$snapshot.type(project = project)

  packages <- dynamic(
    list(project = project, type = type, dev = dev),
    renv_snapshot_dependencies_impl(project, type, dev)
  )

  if (!renv_tests_running())
    packages <- unique(c(packages, "renv"))

  packages

}

renv_snapshot_dependencies_impl <- function(project, type = NULL, dev = FALSE) {

  if (type %in% "all") {
    packages <- installed_packages(field = "Package")
    return(setdiff(packages, renv_packages_base()))
  }

  if (type %in% "custom") {
    filter <- renv_snapshot_filter_custom_resolve()
    return(filter(project))
  }

  path <- case(
    type %in% c("packrat", "implicit") ~ project,
    type %in% "explicit" ~ file.path(project, "DESCRIPTION"),
    ~ {
      fmt <- "internal error: unhandled snapshot type '%s' in %s"
      stopf(fmt, type, stringify(sys.call()))
    }
  )

  # count the number of files in each directory, so we can report
  # to the user if we scanned a folder containing many files
  count <- integer()

  packages <- withCallingHandlers(

    renv_dependencies_impl(
      path = path,
      root = project,
      field = "Package",
      errors = config$dependency.errors(),
      dev = dev
    ),

    # require user confirmation to proceed if there's a reported error
    renv.dependencies.problems = function(cnd) {

      if (identical(config$dependency.errors(), "ignored"))
        return()

      if (interactive() && !proceed())
        cancel()

    },

    # collect information about folders containing lots of files
    renv.dependencies.count = function(cnd) {
      count[[cnd$data$path]] <<- cnd$data$count
    },

    # notify the user if we took a long time to discover dependencies
    renv.dependencies.elapsed_time = function(cnd) {

      # only relevant for implicit-type snapshots
      if (!type %in% c("packrat", "implicit"))
        return()

      # check for timeout
      elapsed <- cnd$data
      limit <- getOption("renv.dependencies.elapsed_time_threshold", default = 10L)
      if (elapsed < limit)
        return()

      # tally up directories with lots of files
      count <- count[order(count)]
      count <- count[count >= 200]

      # report to user
      lines <- c(
        "",
        "NOTE: Dependency discovery took %s during snapshot.",
        "Consider using .renvignore to ignore files, or switching to explicit snapshots.",
        "See `?renv::dependencies` for more information.",
        if (length(count)) c(
          "",
          sprintf("- %s: %s", format(names(count)), nplural("file", count))
        ),
        ""
      )

      # force output in this scope
      renv_scope_caution(TRUE)
      caution(lines, renv_difftime_format(elapsed))

    }

  )

  unique(packages)

}

# compute package records from the provided library paths,
# normally to be included as part of an renv lockfile
renv_snapshot_packages <- function(packages, libpaths, project) {

  ignored <- c(
    renv_packages_base(),
    renv_project_ignored_packages(project = project),
    if (renv_tests_running()) "renv"
  )

  callback <- function(package, location, project) {
    if (nzchar(location) && !package %in% ignored)
      return(location)
  }

  # expand package dependency tree
  paths <- renv_package_dependencies(
    packages = packages,
    libpaths = libpaths,
    callback = callback,
    project = project
  )

  # keep only packages with known locations
  paths <- convert(filter(paths, is.character), "character")

  # diagnose issues with the scanned packages
  paths <- uapply(libpaths, function(library) {
    renv_snapshot_library_diagnose(
      library = library,
      paths   = filter(paths, startswith, prefix = library))
  })

  # now, snapshot the remaining packages
  records <- map(paths, renv_snapshot_description)

}

renv_snapshot_report_missing <- function(missing, type) {

  missing <- setdiff(missing, "renv")
  if (empty(missing))
    return(invisible())

  preamble <- "The following required packages are not installed:"

  postamble <- c(
    "Packages must first be installed before renv can snapshot them.",
    if (type %in% "explicit")
      "If these packages are no longer required, consider removing them from your DESCRIPTION file."
    else
      "Use `renv::dependencies()` to see where this package is used in your project."
  )

  caution_bullets(
    preamble = preamble,
    values = sort(unique(missing)),
    postamble = postamble
  )

  # only prompt the user to install if a restart is available
  restart <- findRestart("renv_recompute_records")
  if (is.null(restart))
    return(invisible())

  choices <- c(
    snapshot = "Snapshot, just using the currently installed packages.",
    install  = "Install the packages, then snapshot.",
    cancel   = "Cancel, and resolve the situation on your own."
  )

  choice <- menu(choices, title = "What do you want to do?")

  if (choice == "snapshot") {
    # do nothing
  } else if (choice == "install") {
    install(missing, prompt = FALSE)
    invokeRestart(restart)
  } else {
    cancel()
  }

  invisible()

}

renv_snapshot_filter_custom_resolve <- function() {

  # check for custom filter
  filter <- getOption("renv.snapshot.filter", default = NULL)
  if (is.null(filter)) {
    fmt <- "snapshot of type '%s' requested, but '%s' is not registered"
    stopf(fmt, "custom", "renv.snapshot.filter")
  }

  # allow for filter naming a function to use
  if (is.character(filter))
    filter <- eval(parse(text = filter), envir = baseenv())

  # check we got a function
  if (!is.function(filter)) {
    fmt <- "snapshot of type '%s' requested, but '%s' is not a function"
    stopf(fmt, "custom", "renv.snapshot.filter")
  }

  # return resolved function
  filter

}

renv_snapshot_fixup <- function(records) {

  records <- renv_snapshot_fixup_renv(records)
  records

}

renv_snapshot_fixup_renv <- function(records) {

  # don't run when testing renv
  if (renv_tests_running())
    return(records)

  # check for an existing valid record
  record <- records$renv
  if (is.null(record))
    return(records)

  source <- renv_record_source(record)
  if (source != "unknown")
    return(records)

  # no valid record available; construct a synthetic one
  remote <- renv_metadata_remote()

  # add it to the set of records
  records$renv <- renv_remotes_resolve(remote)

  # return it
  records

}

renv_snapshot_reprex <- function(lockfile) {

  fmt <- "<sup>Lockfile generated by renv %s.</sup>"
  version <- sprintf(fmt, renv_metadata_version_friendly())

  text <- c(
    "<details style=\"margin-bottom: 10px;\">",
    "<summary>Lockfile</summary>",
    "```",
    renv_lockfile_write(lockfile, file = NULL),
    "```",
    version,
    "</details>"
  )

  output <- paste(text, collapse = "\n")
  class(output) <- "knit_asis"
  attr(output, "knit_cacheable") <- NA

  output

}

renv_snapshot_successful <- function(records, prompt, project) {

  # update snapshot flag
  the$auto_snapshot_failed <- FALSE

  # perform python snapshot on success
  renv_python_snapshot(project, prompt)

  # return generated records
  invisible(records)

}


# socket.R -------------------------------------------------------------------


# avoid R CMD check errors with older R
if (getRversion() < "4.0") {
  utils::globalVariables(c("serverSocket", "socketAccept"))
}

renv_socket_server <- function(min = 49152, max = 65535) {

  # create the socket server
  port <- socket <- NULL
  for (i in 1:2000) catch({
    port <- sample(min:max, size = 1L)
    socket <- serverSocket(port)
    break
  })

  # if we still don't have a socket here, we failed
  if (is.null(socket))
    stop("error creating socket server: couldn't find open port")

  # return information about the server
  list(
    socket = socket,
    port = port,
    pid = Sys.getpid()
  )
}

renv_socket_connect <- function(port, open, timeout = getOption("timeout")) {
  socketConnection(
    host = "127.0.0.1",
    port = port,
    open = open,
    blocking = TRUE,
    encoding = "native.enc",
    timeout = timeout
  )
}

renv_socket_accept <- function(socket, open, timeout = getOption("timeout")) {
  socketAccept(
    socket = socket,
    open = open,
    blocking = TRUE,
    encoding = "native.enc",
    timeout = timeout
  )
}


# stack.R --------------------------------------------------------------------


stack <- function(mode = "list") {

  .data <- list()
  storage.mode(.data) <- mode

  list(

    push = function(...) {
      dots <- list(...)
      for (data in dots) {
        if (is.null(data))
          .data[length(.data) + 1] <<- list(NULL)
        else
          .data[[length(.data) + 1]] <<- data
      }
    },

    pop = function() {
      item <- .data[[length(.data)]]
      length(.data) <<- length(.data) - 1
      item
    },

    peek = function() {
      .data[[length(.data)]]
    },

    contains = function(data) {
      data %in% .data
    },

    empty = function() {
      length(.data) == 0
    },

    get = function(index) {
      if (index <= length(.data)) .data[[index]]
    },

    set = function(index, value) {
      .data[[index]] <<- value
    },

    clear = function() {
      .data <<- list()
    },

    data = function() {
      .data
    }

  )

}


# status.R -------------------------------------------------------------------


the$status_running <- FALSE

#' Report inconsistencies between lockfile, library, and dependencies
#'
#' @description
#' `renv::status()` reports issues caused by inconsistencies across the project
#' lockfile, library, and [dependencies()]. In general, you should strive to
#' ensure that `status()` reports no issues, as this maximises your chances of
#' successfully `restore()`ing the project in the future or on another machine.
#'
#' `renv::load()` will report if any issues are detected when starting an
#' renv project; we recommend resolving these issues before doing any
#' further work on your project.
#'
#' See the headings below for specific advice on resolving any issues
#' revealed by `status()`.
#'
#' # Missing packages
#'
#' `status()` first checks that all packages used by the project are installed. 
#' This must be done first because if any packages are missing we can't tell for
#' sure that a package isn't used; it might be a dependency that we don't know
#' about. Once you have resolve any installation issues, you'll need to run
#' `status()` again to reveal the next set of potential problems.
#'
#' There are four possibilities for an uninstalled package:
#'
#' * If it's used and recorded, call `renv::restore()` to install the version
#'   specified in the lockfile.
#' * If it's used and not recorded, call `renv::install()` to install it
#'   from CRAN or elsewhere.
#' * If it's not used and recorded, call `renv::snapshot()` to
#'   remove it from the lockfile.
#' * If it's not used and not recorded, there's nothing to do. This the most
#'   common state because you only use a small fraction of all available
#'   packages in any one project.
#'
#' If you have multiple packages in an inconsistent state, we recommend
#' `renv::restore()`, then `renv::install()`, then `renv::snapshot()`, but
#' that also suggests you should be running status more frequently.
#'
#' # Lockfile vs `dependencies()`
#'
#' Next we need to ensure that packages are recorded in the lockfile if and
#' only if they are used by the project. Fixing issues of this nature only
#' requires calling  `snapshot()` because there are four possibilities for
#' a package:
#'
#' * If it's used and recorded, it's ok.
#' * If it's used and not recorded, call `renv::snapshot()` to add it to the
#'   lockfile.
#' * If it's not used but is recorded, call `renv::snapshot()` to remove
#'   it from the lockfile.
#' * If it's not used and not recorded, it's also ok, as it may be a
#'   development dependency.
#'
#' # Out-of-sync sources
#'
#' The final issue to resolve is any inconsistencies between the version of
#' the package recorded in the lockfile and the version installed in your
#' library. To fix these issues you'll need to either call `renv::restore()`
#' or `renv::snapshot()`:
#'
#' * Call `renv::snapshot()` if your project code is working. This implies that
#'   the library is correct and you need to update your lockfile.
#' * Call `renv::restore()` if your project code isn't working. This probably
#'   implies that you have the wrong package versions installed and you need
#'   to restore from known good state in the lockfile.
#'
#' If you're not sure which case applies, it's generally safer to call
#' `renv::snapshot()`. If you want to rollback to an earlier known good
#' status, see [renv::history()] and [renv::revert()].
#'
#' @inherit renv-params
#'
#' @param library The library paths. By default, the library paths associated
#'   with the requested project are used.
#'
#' @param sources Boolean; check that each of the recorded packages have a
#'   known installation source? If a package has an unknown source, renv
#'   may be unable to restore it.
#'
#' @param cache Boolean; perform diagnostics on the global package cache?
#'   When `TRUE`, renv will validate that the packages installed into the
#'   cache are installed at the expected + proper locations, and validate the
#'   hashes used for those storage locations.
#'
#' @return This function is normally called for its side effects, but
#'   it invisibly returns a list containing the following components:
#'
#'   * `library`: packages in your library.
#'   * `lockfile`: packages in the lockfile.
#'   * `synchronized`: are the library and lockfile in sync?
#'
#' @export
#'
#' @example examples/examples-init.R
status <- function(project = NULL,
                   ...,
                   library = NULL,
                   lockfile = NULL,
                   sources = TRUE,
                   cache = FALSE)
{
  renv_scope_error_handler()
  renv_dots_check(...)

  renv_snapshot_auto_suppress_next()
  renv_scope_options(renv.prompt.enabled = FALSE)

  the$status_running <- TRUE
  defer(the$status_running <- FALSE)

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  # check to see if we've initialized this project
  if (!renv_status_check_initialized(project, library, lockfile)) {
    result <- list(
      library = list(Packages = named(list())),
      lockfile = list(Packages = named(list())),
      synchronized = FALSE
    )
    return(invisible(result))
  }

  libpaths <- library %||% renv_libpaths_resolve()
  lockpath <- lockfile %||% renv_paths_lockfile(project = project)

  # get all dependencies, including transitive
  dependencies <- renv_snapshot_dependencies(project, dev = FALSE)
  packages <- sort(union(dependencies, "renv"))
  paths <- renv_package_dependencies(packages, libpaths = libpaths, project = project)
  packages <- as.character(names(paths))

  # read project lockfile
  lockfile <- if (file.exists(lockpath))
    renv_lockfile_read(lockpath)
  else
    renv_lockfile_init(project = project)

  # get lockfile capturing current library state
  library <- renv_lockfile_create(
    libpaths = libpaths,
    type     = "all",
    prompt   = FALSE,
    project  = project
  )

  # remove ignored packages
  ignored <- c(
    renv_project_ignored_packages(project),
    renv_packages_base(),
    if (renv_tests_running()) "renv"
  )

  packages <- setdiff(packages, ignored)
  renv_lockfile_records(lockfile) <- exclude(renv_lockfile_records(lockfile), ignored)
  renv_lockfile_records(library) <- exclude(renv_lockfile_records(library), ignored)

  synchronized <-
    renv_status_check_consistent(lockfile, library, packages) &&
    renv_status_check_synchronized(lockfile, library)

  if (sources) {
    synchronized <- synchronized &&
      renv_status_check_unknown_sources(project, lockfile)
  }

  if (cache)
    renv_status_check_cache(project)

  if (synchronized)
    writef("No issues found -- the project is in a consistent state.")
  else
    writef(c("", "See ?renv::status() for advice on resolving these issues."))

  result <- list(
    library      = library,
    lockfile     = lockfile,
    synchronized = synchronized
  )

  invisible(result)

}

renv_status_check_unknown_sources <- function(project, lockfile) {
  renv_check_unknown_source(renv_lockfile_records(lockfile), project)
}

renv_status_check_consistent <- function(lockfile, library, used) {

  lockfile <- renv_lockfile_records(lockfile)
  library <- renv_lockfile_records(library)

  packages <- sort(unique(c(names(library), names(lockfile), used)))

  status <- data.frame(
    package = packages,
    installed = packages %in% names(library),
    recorded = packages %in% names(lockfile),
    used = packages %in% used
  )

  ok <- status$installed & (status$used == status$recorded)
  if (all(ok))
    return(TRUE)

  if (renv_verbose()) {
    # If any packages are not installed, we don't know for sure what's used
    # because our dependency graph is incomplete
    issues <- status[!ok, , drop = FALSE]
    missing <- !issues$installed
    issues$installed <- ifelse(issues$installed, "y", "n")
    issues$recorded <- ifelse(issues$recorded, "y", "n")
    issues$used <- ifelse(issues$used, "y", if (any(missing)) "?" else "n")

    if (any(missing)) {
      msg <- "The following package(s) are missing:"
      issues <- issues[missing, ]
    } else {
      msg <- "The following package(s) are in an inconsistent state:"
    }
    writef(msg)
    writef()
    print(issues, row.names = FALSE, right = FALSE)
  }

  FALSE

}

renv_status_check_initialized <- function(project, library = NULL, lockfile = NULL) {

  # only done if library and lockfile are NULL; that is, if the user
  # is calling `renv::status()` without arguments
  if (!is.null(library) || !is.null(lockfile))
    return(TRUE)

  # resolve paths to lockfile, primary library path
  library  <- library  %||% renv_paths_library(project = project)
  lockfile <- lockfile %||% renv_paths_lockfile(project = project)

  # check whether the lockfile + library exist
  haslib  <- all(file.exists(library))
  haslock <- file.exists(lockfile)
  if (haslib && haslock)
    return(TRUE)

  # TODO: what about the case where the library exists but no packages are installed?
  # TODO: should this check for an 'renv/activate.R' script?
  # TODO: what if a different project is loaded?
  if (haslib && !haslock) {
    writef(c(
      "This project does not contain a lockfile.",
      "Use `renv::snapshot()` to create a lockfile."
    ))
  } else if (!haslib && haslock) {
    writef(c(
      "There are no packages installed in the project library.",
      "Use `renv::restore()` to install the packages defined in lockfile."
    ))
  } else {
    writef(c(
      "This project does not appear to be using renv.",
      "Use `renv::init()` to initialize the project."
    ))
  }

  FALSE

}

renv_status_check_synchronized <- function(lockfile, library) {

  lockfile <- renv_lockfile_records(lockfile)
  library <- renv_lockfile_records(library)

  actions <- renv_lockfile_diff_packages(lockfile, library)
  rest <- c("upgrade", "downgrade", "crossgrade")

  if (all(!rest %in% actions)) {
    return(TRUE)
  }

  pkgs <- names(actions[actions %in% rest])
  renv_pretty_print_records_pair(
    preamble = "The following package(s) are out of sync [lockfile -> library]:",
    lockfile[pkgs],
    library[pkgs],
  )

  FALSE

}

renv_status_check_cache <- function(project) {

  if (renv_cache_config_enabled(project = project))
    renv_cache_diagnose()

}



# system.R -------------------------------------------------------------------


renv_system_exec <- function(command,
                             args    = NULL,
                             action  = "executing command",
                             success = 0L,
                             stream  = FALSE,
                             quiet   = NULL)
{
  # be quiet when running tests by default
  quiet <- quiet %||% renv_tests_running()

  # handle 'stream' specially
  if (stream) {

    # form stdout, stderr
    stdout <- stderr <- if (quiet) FALSE else ""

    # execute command
    status <- suppressWarnings(
      if (is.null(args))
        system(command, ignore.stdout = quiet, ignore.stderr = quiet)
      else
        system2(command, args, stdout = stdout, stderr = stderr)
    )

    # check for error
    status <- status %||% 0L
    if (!is.null(success) && !status %in% success) {
      fmt <- "error %s [error code %i]"
      stopf(fmt, action, status)
    }

    # return status code
    return(status)

  }

  # suppress warnings as some successful commands may return a non-zero exit
  # code, whereas R will always warn on such error codes
  output <- suppressWarnings(
    if (is.null(args))
      system(command, intern = TRUE)
    else
      system2(command, args, stdout = TRUE, stderr = TRUE)
  )

  # extract status code from result
  status <- attr(output, "status") %||% 0L

  # if this status matches an expected 'success' code, return output
  if (is.null(success) || status %in% success)
    return(output)

  # otherwise, notify the user that things went wrong
  abort(
    sprintf("error %s [error code %i]", action, status),
    body = renv_system_exec_details(command, args, output)
  )

}

renv_system_exec_details <- function(command, args, output) {

  # get header, giving the command that was run
  cmdline <- paste(command, paste(args, collapse = " "))
  underline <- paste(rep.int("=", min(80L, nchar(cmdline))), collapse = "")
  header <- c(cmdline, underline)

  # truncate output (avoid overwhelming console)
  body <- if (length(output) > 200L)
    c(head(output, n = 100L), "< ... >", tail(output, n = 100L))
  else
    output

  c(header, "", body)

}


# tar.R ----------------------------------------------------------------------


renv_tar_exe <- function() {

  # allow override
  tar <- getOption("renv.tar.exe")
  if (!is.null(tar))
    return(tar)

  # on unix, just use default
  if (renv_platform_unix())
    return(Sys.which("tar"))

  # on Windows, use system tar.exe if available
  root <- Sys.getenv("SystemRoot", unset = NA)
  if (is.na(root))
    root <- "C:/Windows"

  # use tar if it exists
  tarpath <- file.path(root, "System32/tar.exe")
  if (file.exists(tarpath))
    return(tarpath)

  # otherwise, give up (don't trust the arbitrary tar on PATH)
  ""

}

renv_tar_decompress <- function(tar, archive, files = NULL, exdir = ".", ...) {

  # build argument list
  args <- c(
    "xf", renv_shell_path(archive),
    if (!identical(exdir, "."))
      c("-C", renv_shell_path(exdir)),
    if (length(files))
      renv_shell_path(files)
  )

  # make sure exdir exists
  ensure_directory(exdir)

  # perform decompress
  return(renv_system_exec(tar, args, action = "decompressing archive"))

}


# task.R ---------------------------------------------------------------------


renv_task_create <- function(callback, name = NULL) {

  # create name for task callback
  name <- name %||% as.character(substitute(callback))
  name <- paste("renv", name, sep = ":::")

  # remove an already-existing task of the same name
  removeTaskCallback(name)

  # otherwise, add our new task
  addTaskCallback(
    renv_task_callback(callback, name),
    name = name
  )

}

renv_task_callback <- function(callback, name) {

  force(callback)
  force(name)

  function(...) {

    status <- tryCatch(callback(), error = identity)
    if (inherits(status, "error")) {
      caution("Error in background task '%s': %s", name, conditionMessage(status))
      caution("Background task '%s' will be stopped.", name)
      return(FALSE)
    }

    TRUE

  }

}

renv_task_unload <- function() {
  callbacks <- getTaskCallbackNames()
  for (callback in callbacks)
    for (prefix in c("renv_", "renv:::"))
      if (startswith(callback, prefix))
        removeTaskCallback(callback)
}


# template.R -----------------------------------------------------------------


renv_template_create <- function(template) {
  gsub("^\\n+|\\n+$", "", template)
}

renv_template_replace <- function(text, replacements, format = "${%s}") {

  enumerate(replacements, function(key, value) {
    key <- sprintf(format, key)
    text <<- gsub(key, value, text, fixed = TRUE)
  })

  text

}


# tests.R --------------------------------------------------------------------


the$tests_root <- NULL

# NOTE: Prefer using 'is_testing()' to 'renv_tests_running()' for behavior
# that should apply regardless of the package currently being tested.
#
# renv_tests_running() is appropriate when running renv's own tests.
renv_tests_running <- function() {
  getOption("renv.tests.running", default = FALSE)
}

renv_test_code <- function(code, data = list(), fileext = ".R", scope = parent.frame()) {
  code <- do.call(substitute, list(substitute(code), data))
  file <- renv_scope_tempfile("renv-code-", fileext = fileext, scope = scope)

  writeLines(deparse(code), con = file)
  file
}

renv_test_retrieve <- function(record) {

  renv_scope_error_handler()

  # avoid using cache
  cache <- renv_scope_tempfile()
  renv_scope_envvars(RENV_PATHS_CACHE = cache)

  # construct records
  package <- record$Package
  records <- list(record)
  names(records) <- package

  # prepare dummy library
  templib <- renv_scope_tempfile("renv-library-")
  ensure_directory(templib)
  renv_scope_libpaths(c(templib, .libPaths()))

  # attempt a restore into that library
  renv_scope_restore(
    project = getwd(),
    library = templib,
    records = records,
    packages = package,
    recursive = TRUE
  )

  records <- retrieve(record$Package)
  renv_install_impl(records)

  descpath <- file.path(templib, package)
  if (!file.exists(descpath))
    stopf("failed to retrieve package '%s'", package)

  desc <- renv_description_read(descpath)
  fields <- grep("^Remote", names(record), value = TRUE)

  testthat::expect_identical(
    as.list(desc[fields]),
    as.list(record[fields])
  )

}

renv_tests_diagnostics <- function() {

  # print library paths
  caution_bullets(
    "The following R libraries are set:",
    paste("-", .libPaths())
  )

  # print repositories
  repos <- getOption("repos")
  caution_bullets(
    "The following repositories are set:",
    paste(names(repos), repos, sep = ": ")
  )

  # print renv root
  caution_bullets(
    "The following renv root directory is being used:",
    paste("-", paths$root())
  )

  # print cache root
  caution_bullets(
    "The following renv cache directory is being used:",
    paste("-", paths$cache())
  )

  writeLines("The following packages are available in the test repositories:")

  dbs <-
    available_packages(type = "source", quiet = TRUE) %>%
    map(function(db) {
      rownames(db) <- NULL
      db[c("Package", "Version", "File")]
    })

  print(dbs)

  path <- Sys.getenv("PATH")
  splat <- strsplit(path, .Platform$path.sep, fixed = TRUE)[[1]]

  caution_bullets(
    "The following PATH is set:",
    paste("-", splat)
  )

  envvars <- c(
    grep("^_R_", names(Sys.getenv()), value = TRUE),
    "HOME",
    "R_ARCH", "R_HOME",
    "R_LIBS", "R_LIBS_SITE", "R_LIBS_USER", "R_USER",
    "R_ZIPCMD",
    "TAR", "TEMP", "TMP", "TMPDIR"
  )

  keys <- format(envvars)
  vals <- Sys.getenv(envvars, unset = "<NA>")
  vals[vals != "<NA>"] <- renv_json_quote(vals[vals != "<NA>"])

  caution_bullets(
    "The following environment variables of interest are set:",
    paste(keys, vals, sep = " : ")
  )

}

renv_tests_root <- function() {
  the$tests_root <- the$tests_root %||% {
    renv_path_normalize(testthat::test_path("."))
  }
}

renv_tests_path <- function(path = NULL) {

  # special case for NULL path
  if (is.null(path))
    return(renv_tests_root())

  # otherwise, form path from root
  file.path(renv_tests_root(), path)

}

renv_tests_supported <- function() {

  # supported when running locally + on CI
  for (envvar in c("NOT_CRAN", "CI"))
    if (renv_envvar_exists(envvar))
      return(TRUE)

  # disabled on older macOS releases (credentials fails to load)
  if (renv_platform_macos() && getRversion() < "4.0.0")
    return(FALSE)

  # disabled on Windows
  if (renv_platform_windows())
    return(FALSE)

  # true otherwise
  TRUE

}


# testthat-helpers.R ---------------------------------------------------------


expect_same_elements <- function(lhs, rhs) {

  if (!requireNamespace("testthat", quietly = TRUE))
    stop("testthat not available for testing")

  if (is.list(lhs) && is.list(rhs)) {
    lhs <- lhs[order(names(lhs))]
    rhs <- rhs[order(names(rhs))]
    return(testthat::expect_equal(!!lhs, !!rhs))
  }

  if (packageVersion("testthat") > "2.2.0")
    testthat::expect_setequal(!!lhs, !!rhs)
  else
    testthat::expect_setequal(lhs, rhs)

}


# truthy.R -------------------------------------------------------------------


truthy <- function(value, default = FALSE) {

  # https://github.com/rstudio/renv/issues/1558
  if (is.call(value)) {
    value <- tryCatch(renv_dependencies_eval(value), error = identity)
    if (inherits(value, "error"))
      return(default)
  }

  if (length(value) == 0)
    default
  else if (is.character(value))
    value %in% c("TRUE", "True", "true", "T", "1")
  else if (is.symbol(value))
    as.character(value) %in% c("TRUE", "True", "true", "T", "1")
  else if (is.na(value))
    default
  else
    as.logical(value)
}


# type.R ---------------------------------------------------------------------


renv_type_check <- function(value, type) {

  # quietly convert NAs to requested type
  if (is.null(value) || is.na(value))
    return(convert(value, type))

  # if the value already matches the expected type, return success
  if (inherits(value, type))
    return(value)

  # create error object
  fmt <- "parameter '%s' is not of expected type '%s'"
  msg <- sprintf(fmt, deparse(substitute(value)), type)
  error <- simpleError(msg, sys.call(sys.parent()))

  # report error
  stop(error)

}

renv_type_unexpected <- function(value) {
  fmt <- "parameter '%s' has unexpected type '%s'"
  msg <- sprintf(fmt, deparse(substitute(value)), typeof(value))
  error <- simpleError(msg, sys.call(sys.parent()))
  stop(error)
}


# unload.R -------------------------------------------------------------------


unload <- function(project = NULL, quiet = FALSE) {

  project <- renv_project_resolve(project)
  renv_scope_error_handler()

  if (renv_tests_running())
    return()

  if (quiet)
    renv_scope_options(renv.verbose = FALSE)

  renv_envvars_restore()

  renv_unload_shims(project)
  renv_unload_project(project)
  renv_unload_profile(project)
  renv_unload_envvars(project)
  renv_unload_sandbox(project)
  renv_unload_libpaths(project)

}

renv_unload_shims <- function(project) {
  renv_shims_deactivate()
}

renv_unload_project <- function(project) {
  renv_project_clear()
}

renv_unload_profile <- function(project) {
  Sys.unsetenv("RENV_PROFILE")
}

renv_unload_envvars <- function(project) {
  renv_envvars_restore()
}

renv_unload_sandbox <- function(project) {
  renv_sandbox_deactivate()
}

renv_unload_libpaths <- function(project) {
  renv_libpaths_restore()
}

renv_unload_finalizer <- function(libpath) {
  libpath <- dirname(renv_namespace_path(.packageName))
  .onUnload(libpath)
}


# update.R -------------------------------------------------------------------


the$update_errors <- new.env(parent = emptyenv())

renv_update_find_repos <- function(records) {

  results <- lapply(records, function(record) {
    catch(renv_update_find_repos_impl(record))
  })

  failed <- map_lgl(results, inherits, "error")
  if (any(failed))
    renv_update_errors_set("repos", results[failed])

  results[!failed]

}

renv_update_find_repos_impl <- function(record) {

  # retrieve latest-available package
  package <- record$Package
  latest <- catch(renv_available_packages_latest(package))
  if (inherits(latest, "error"))
    return(NULL)

  # validate our versions
  if (empty(latest$Version) || empty(record$Version))
    return(NULL)

  # compare the versions; return NULL if the 'latest' version
  # is older
  compare <- renv_version_compare(latest$Version, record$Version)
  if (compare != 1L)
    return(NULL)

  latest

}

renv_update_find_git <- function(records) {
  renv_parallel_exec(records, renv_update_find_git_impl)
}

renv_update_find_git_impl <- function(record) {

  sha <- renv_remotes_resolve_git_sha_ref(record)

  # if sha is empty:
  # `git remote-ls origin ref` expects ref to be a reference, not a sha
  # it is empty if ref isn't a reference on the repo
  # this may be due to record$RemoteRef actually being a sha
  # or it may be because record$RemoteRef is not a real ref
  # but we can't check, so we will try to fetch the ref & see what we get
  oldsha <- record$RemoteSha %||% ""
  if (nzchar(oldsha) && identical(sha, oldsha))
    return(NULL)

  current <- record
  current$RemoteSha <- sha

  desc <- renv_remotes_resolve_git_description(current)

  current$Version <- desc$Version
  current$Package <- desc$Package

  updated <- renv_version_ge(current$Version, record$Version)
  if (updated)
    return(current)

}

renv_update_find_github <- function(records) {

  # check for GITHUB_PAT
  if (!renv_envvar_exists("GITHUB_PAT")) {

    msg <- paste(
      "GITHUB_PAT is unset. Updates may fail due to GitHub's API rate limit.",
      "",
      "To increase your GitHub API rate limit:",
      "- Use `usethis::browse_github_pat()` to create a Personal Access Token (PAT).",
      "- Use `usethis::edit_r_environ()` and add the token as `GITHUB_PAT`.",
      sep = "\n"
    )

    warning(msg, call. = FALSE)

  }

  names(records) <- map_chr(records, `[[`, "Package")
  results <- renv_parallel_exec(records, function(record) {
    catch(renv_update_find_github_impl(record))
  })

  failed <- map_lgl(results, inherits, "error")
  if (any(failed))
    renv_update_errors_set("github", results[failed])

  results[!failed]

}

renv_update_find_github_impl <- function(record) {

  # construct and parse record entry
  host   <- record$RemoteHost %||% config$github.host()
  user   <- record$RemoteUsername
  repo   <- record$RemoteRepo
  subdir <- record$RemoteSubdir
  ref    <- record$RemoteRef

  # check for changed sha
  sha <- renv_remotes_resolve_github_sha_ref(host, user, repo, ref)
  if (sha == record$RemoteSha)
    return(NULL)

  # get updated record
  desc <- renv_remotes_resolve_github_description(host, user, repo, subdir, sha)
  current <- list(
    Package        = desc$Package,
    Version        = desc$Version,
    Source         = "GitHub",
    RemoteUsername = user,
    RemoteRepo     = repo,
    RemoteSubdir   = subdir,
    RemoteRef      = ref,
    RemoteSha      = sha,
    RemoteHost     = host
  )

  # check that the version has actually updated
  updated <-
    current$RemoteSha != record$RemoteSha &&
    numeric_version(current$Version) >= numeric_version(record$Version)

  if (updated)
    return(current)

}


renv_update_find_remote <- function(records, type) {

  update <- switch(type,
    "gitlab" = renv_remotes_resolve_gitlab,
    "bitbucket" = renv_remotes_resolve_bitbucket,
    stopf("Unsupported type %s", type)
  )

  names(records) <- map_chr(records, `[[`, "Package")
  results <- renv_parallel_exec(records, function(record) {
    catch(renv_update_find_remote_impl(record, update))
  })

  failed <- map_lgl(results, inherits, "error")
  if (any(failed))
    renv_update_errors_set(type, results[failed])

  results[!failed]

}

renv_update_find_remote_impl <- function(record, update) {

  remote <- list(
    host = record$RemoteHost,
    user = record$RemoteUsername,
    repo = record$RemoteRepo,
    ref = record$RemoteRef
  )
  current <- update(remote)

  # check that the version has actually updated
  updated <-
    current$RemoteSha != record$RemoteSha &&
    numeric_version(current$Version) >= numeric_version(record$Version)

  if (updated)
    return(current)

}


renv_update_find <- function(records) {

  sources <- extract_chr(records, "Source")
  grouped <- split(records, sources)

  # retrieve updates
  results <- enumerate(grouped, function(source, records) {
    case(
      source == "Bioconductor" ~ renv_update_find_repos(records),
      source == "Repository"   ~ renv_update_find_repos(records),
      source == "GitHub"       ~ renv_update_find_github(records),
      source == "Git"          ~ renv_update_find_git(records),
      source == "GitLab"       ~ renv_update_find_remote(records, "gitlab"),
      source == "Bitbucket"    ~ renv_update_find_remote(records, "bitbucket")
    )
  })

  # remove groupings
  ungrouped <- unlist(results, recursive = FALSE, use.names = FALSE)
  if (empty(ungrouped))
    return(list())

  # keep non-null results
  updates <- Filter(Negate(is.null), ungrouped)
  if (empty(updates))
    return(list())

  names(updates) <- extract_chr(updates, "Package")
  renv_records_sort(updates)

}



#' Update packages
#'
#' @description
#' Update packages which are currently out-of-date. Currently supports CRAN,
#' Bioconductor, other CRAN-like repositories, GitHub, GitLab, Git, and
#' BitBucket.
#'
#' Updates will only be checked from the same source -- for example,
#' if a package was installed from GitHub, but a newer version is
#' available on CRAN, that updated version will not be seen.
#'
#' @inherit renv-params
#'
#' @param packages A character vector of \R packages to update. When `NULL`
#'   (the default), all packages (apart from any listed in the `ignored.packages`
#'   project setting) will be updated.
#'
#' @param check Boolean; check for package updates without actually
#'   installing available updates? This is useful when you'd like to determine
#'   what updates are available, without actually installing those updates.
#'
#' @param exclude A set of packages to explicitly exclude from updating.
#'   Use `renv::update(exclude = <...>)` to update all packages except for
#'   a specific set of excluded packages.
#'
#' @return A named list of package records which were installed by renv.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # update the 'dplyr' package
#' renv::update("dplyr")
#'
#' }
update <- function(packages = NULL,
                   ...,
                   exclude = NULL,
                   library = NULL,
                   rebuild = FALSE,
                   check   = FALSE,
                   prompt  = interactive(),
                   project = NULL)
{
  renv_consent_check()
  renv_scope_error_handler()
  renv_dots_check(...)

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)
  renv_scope_verbose_if(prompt)

  # resolve library path
  libpaths <- renv_libpaths_resolve(library)
  library <- nth(libpaths, 1L)
  renv_scope_libpaths(libpaths)

  # resolve exclusions
  exclude <- c(exclude, settings$ignored.packages(project = project))

  # if users have requested the use of pak, delegate there
  if (config$pak.enabled() && !recursing()) {
    packages <- setdiff(packages, exclude)
    renv_pak_init()
    return(renv_pak_install(packages, libpaths, project))
  }

  # get package records
  renv_scope_binding(the, "snapshot_hash", FALSE)
  records <- renv_snapshot_libpaths(libpaths = libpaths, project = project)
  packages <- packages %||% names(records)

  # apply exclusions
  packages <- setdiff(packages, exclude)

  # check if the user has requested update for packages not installed
  missing <- renv_vector_diff(packages, names(records))
  if (!empty(missing)) {

    if (prompt || renv_verbose()) {
      caution_bullets(
        "The following package(s) are not currently installed:",
        missing,
        "The latest available versions of these packages will be installed instead."
      )
    }

    cancel_if(prompt && !proceed())

  }

  # select records
  selected <- c(
    records[renv_vector_intersect(packages, names(records))],
    named(lapply(missing, renv_available_packages_latest), missing)
  )

  # check for usage of cran, bioc
  repo <- FALSE
  bioc <- FALSE

  for (record in selected) {

    source <- renv_record_source(record, normalize = TRUE)

    if (source %in% c("repository")) {
      repo <- TRUE
      next
    }

    if (source %in% c("bioconductor")) {
      repo <- bioc <- TRUE
      next
    }

  }

  # activate bioc repositories if needed
  if (bioc)
    renv_scope_bioconductor(project = project)

  # ensure database of available packages is current
  if (repo) {
    for (type in renv_package_pkgtypes()) {
      available_packages(type = type)
    }
  }

  printf("- Checking for updated packages ... ")

  # remove records that appear to be from an R package repository,
  # but are not actually available in the current repositories
  selected <- filter(selected, function(record) {

    source <- renv_record_source(record, normalize = TRUE)
    if (!source %in% c("bioconductor", "cran", "repository"))
      return(TRUE)

    # check for available package
    package <- record$Package
    entry <- catch(renv_available_packages_latest(package))
    !inherits(entry, "error")

  })

  updates <- renv_update_find(selected)
  writef("Done!")

  renv_update_errors_emit()

  if (empty(updates)) {
    writef("- All packages appear to be up-to-date.")
    return(invisible(TRUE))
  }

  # perform a diff (for reporting to user)
  old <- selected[names(updates)]
  new <- updates
  diff <- renv_lockfile_diff_packages(old, new)

  # if we're only checking for updates, just report and exit
  if (check) {

    fmt <- case(
      length(diff) == 1 ~ "- %i package has updates available.",
      length(diff) != 1 ~ "- %i packages have updates available."
    )

    preamble <- sprintf(fmt, length(diff))
    renv_updates_report(preamble, diff, old, new)
    return(invisible(renv_updates_create(diff, old, new)))

  }

  if (prompt || renv_verbose()) {
    renv_restore_report_actions(diff, old, new)
    cancel_if(prompt && !proceed())
  }

  # perform the install
  install(
    packages = updates,
    library  = libpaths,
    rebuild  = rebuild,
    prompt   = prompt,
    project  = project
  )

}

renv_update_errors_set <- function(key, errors) {
  assign(key, errors, envir = the$update_errors)
}

renv_update_errors_clear <- function() {
  rm(
    list = ls(envir = the$update_errors, all.names = TRUE),
    envir = the$update_errors
  )
}

renv_update_errors_emit <- function() {

  # clear errors when we're done
  defer(renv_update_errors_clear())

  # if we have any errors, start by emitting a single newline
  all <- ls(envir = the$update_errors, all.names = TRUE)
  if (!empty(all))
    writef()

  # then emit errors for each class
  renv_update_errors_emit_repos()
  renv_update_errors_emit_remote("github", "GitHub")
  renv_update_errors_emit_remote("gitlab", "GitLab")
  renv_update_errors_emit_remote("bitbucket", "BitBucket")

}

renv_update_errors_emit_impl <- function(key, preamble, postamble) {

  errors <- the$update_errors[[key]]
  if (empty(errors))
    return()

  messages <- enumerate(errors, function(package, error) {
    errmsg <- paste(conditionMessage(error), collapse = "; ")
    sprintf("%s: %s", format(package), errmsg)
  })

  caution_bullets(
    preamble = preamble,
    values = messages,
    postamble = postamble
  )

}

renv_update_errors_emit_repos <- function() {

  renv_update_errors_emit_impl(
    key       = "repos",
    preamble  = "One or more errors occurred while finding updates for the following packages:",
    postamble = "Ensure that these packages are available from your active package repositories."
  )

}

renv_update_errors_emit_remote <- function(key, label) {

  renv_update_errors_emit_impl(
    key       = key,
    preamble  = sprintf("One or more errors occurred while finding updates for the following %s packages:", label),
    postamble = sprintf("Ensure that these packages were installed from an accessible %s remote.", label)
  )

}



# updates.R ------------------------------------------------------------------


renv_updates_create <- function(diff, old, new) {
  structure(
    list(diff = diff, old = old, new = new),
    class = "renv_updates"
  )
}

renv_updates_report <- function(preamble, diff, old, new) {

  lhs <- renv_lockfile_records(old)
  rhs <- renv_lockfile_records(new)
  renv_pretty_print_records_pair(
    preamble,
    lhs[names(lhs) %in% names(diff)],
    rhs[names(rhs) %in% names(diff)]
  )

}


# upgrade.R ------------------------------------------------------------------


#' Upgrade renv
#'
#' @description
#' Upgrade the version of renv associated with a project, including using
#' a development version from GitHub. Automatically snapshots the update
#' renv, updates the activate script, and restarts R.
#'
#' If you want to update all packages (including renv) to their latest CRAN
#' versions, use [renv::update()].
#'
#' @inherit renv-params
#'
#' @param version The version of renv to be installed.
#'
#'   When `NULL` (the default), the latest version of renv will be installed as
#'   available from CRAN (or whatever active package repositories are active)
#'   Alternatively, you can install the latest development version with
#'  `"main"`, or a specific commit with a SHA, e.g. `"5049cef8a"`.
#'
#' @param prompt Boolean; prompt upgrade before proceeding?
#'
#' @param reload Boolean; reload renv after install? When `NULL` (the
#'   default), renv will be re-loaded only if updating renv for the
#'   active project. Since it's not possible to guarantee a clean reload
#'   in the current session, this will attempt to restart your R session.
#'
#' @return A boolean value, indicating whether the requested version of
#'   renv was successfully installed. Note that this function is normally
#'   called for its side effects.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # upgrade to the latest version of renv
#' renv::upgrade()
#'
#' # upgrade to the latest version of renv on GitHub (development version)
#' renv::upgrade(version = "main")
#'
#' }
upgrade <- function(project = NULL,
                    version = NULL,
                    reload  = NULL,
                    prompt = interactive())
{
  renv_scope_error_handler()
  renv_scope_verbose_if(prompt)
  invisible(renv_upgrade_impl(project, version, reload, prompt))
}

renv_upgrade_impl <- function(project, version, reload, prompt) {

  project <- renv_project_resolve(project)
  renv_project_lock(project = project)

  reload <- reload %||% renv_project_loaded(project)

  lockfile <- renv_lockfile_load(project)
  old <- lockfile$Packages$renv
  new <- renv_upgrade_find_record(version)

  # check for some form of change
  if (renv_records_equal(old, new)) {
    fmt <- "- renv [%s] is already installed and active for this project."
    writef(fmt, renv_metadata_version_friendly())
    return(FALSE)
  }

  if (prompt || renv_verbose()) {
    renv_pretty_print_records_pair(
      "A new version of the renv package will be installed:",
      list(renv = old),
      list(renv = new),
      "This project will use the newly-installed version of renv."
    )
  }

  cancel_if(prompt && !proceed())

  renv_scope_restore(
    project   = project,
    library   = renv_libpaths_active(),
    records   = list(renv = new),
    packages  = "renv",
    recursive = FALSE
  )

  # retrieve and install renv
  records <- retrieve("renv")
  renv_install_impl(records)

  # update the lockfile
  lockfile <- renv_lockfile_load(project = project)
  records <- renv_lockfile_records(lockfile) %||% list()
  records$renv <- new
  renv_lockfile_records(lockfile) <- records
  renv_lockfile_save(lockfile, project = project)

  # now update the infrastructure to use this version of renv.
  # do this in a separate process to avoid issues that could arise
  # if the old version of renv is still loaded
  #
  # https://github.com/rstudio/renv/issues/1546
  writef("- Updating activate script")
  code <- substitute({
    renv <- asNamespace("renv"); renv$summon()
    version <- renv_metadata_version_create(record)
    renv_infrastructure_write(project, version = version)
  }, list(project = project, record = records[["renv"]]))

  script <- renv_scope_tempfile("renv-activate-", fileext = ".R")
  writeLines(deparse(code), con = script)

  args <- c("--vanilla", "-s", "-f", renv_shell_path(script))
  r(args, stdout = FALSE, stderr = FALSE)

  if (reload) {
    renv_restart_request(project)
  }

  invisible(TRUE)

}

renv_upgrade_find_record <- function(version) {

  if (is.null(version))
    renv_upgrade_find_record_default()
  else
    renv_upgrade_find_record_dev(version)

}

renv_upgrade_find_record_default <- function() {

  # check if the package is available on R repositories.
  # if not, prefer GitHub
  record <- catch(renv_available_packages_latest("renv"))
  if (inherits(record, "error"))
    return(renv_upgrade_find_record_dev())

  # check the version reported by R repositories.
  # if it's older than current renv, then prefer GitHub
  version <- record$Version
  if (package_version(version) < renv_package_version("renv"))
    return(renv_upgrade_find_record_dev())

  # ok -- install from repository
  record

}

renv_upgrade_find_record_dev <- function(version = NULL) {
  version <- version %||% renv_upgrade_find_record_dev_latest()
  entry <- paste("rstudio/renv", version, sep = "@")
  renv_remotes_resolve(entry)
}


renv_upgrade_find_record_dev_latest <- function() {

  # download tags
  url <- "https://api.github.com/repos/rstudio/renv/tags"
  destfile <- tempfile("renv-tags-", fileext = ".json")
  download(url, destfile = destfile, quiet = TRUE)
  json <- renv_json_read(destfile)

  # find latest version
  names <- extract_chr(json, "name")
  versions <- numeric_version(names, strict = FALSE)
  latest <- sort(versions, decreasing = TRUE)[[1]]
  names[versions %in% latest][[1L]]

}

renv_upgrade_reload <- function() {

  # we need to remove the task callbacks here, as otherwise
  # we'll run into trouble trying to remove task callbacks
  # within a task callback
  renv_task_unload()

  # now define and add a callback to reload renv; use the base namespace
  # to avoid carrying along any bits of the current renv environment
  callback <- function(...) {
    unloadNamespace("renv")
    loadNamespace("renv")
    invisible(FALSE)
  }

  environment(callback) <- baseenv()

  # add the task callback; don't name it so that the renv infrastructure
  # doesn't try to remove this callback (it'll resolve and remove itself)
  addTaskCallback(callback)

  invisible(TRUE)

}


# url.R ----------------------------------------------------------------------


renv_url_parse <- function(url) {

  pattern <- paste0(
    "^",
    "([^:]+://)?",        # protocol
    "([^/?#]+)",          # domain
    "(?:(/[^?#]*))?",     # path
    "(?:[?]([^#]+))?",    # parameters
    "(?:#(.*))?",         # fragment
    ""
  )

  matches <- regmatches(url, regexec(pattern, url, perl = TRUE))[[1L]]
  if (length(matches) != 6L)
    stopf("couldn't parse url '%s'", url)

  matches <- as.list(matches)
  names(matches) <- c("url", "protocol", "domain", "path", "parameters", "fragment")

  # parse parameters into named list
  matches$parameters <- renv_properties_read(
    text = chartr("&", "\n", matches$parameters),
    delimiter = "=",
    dequote = FALSE,
    trim = FALSE
  )

  # return parsed URL
  matches

}



# use-python.R ---------------------------------------------------------------


#' Use python
#'
#' Associate a version of Python with your project.
#'
#' When Python integration is active, renv will:
#'
#' - Save metadata about the requested version of Python in `renv.lock` -- in
#'   particular, the Python version, and the Python type ("virtualenv", "conda",
#'   "system"),
#'
#' - Capture the set of installed Python packages during `renv::snapshot()`,
#'
#' - Re-install the set of recorded Python packages during `renv::restore()`.
#'
#' In addition, when the project is loaded, the following actions will be taken:
#'
#' - The `RENV_PYTHON` environment variable will be set, indicating the version
#'   of Python currently active for this sessions,
#'
#' - The `RETICULATE_PYTHON` environment variable will be set, so that the
#'   reticulate package can automatically use the requested copy of Python
#'   as appropriate,
#'
#' - The requested version of Python will be placed on the `PATH`, so that
#'   attempts to invoke Python will resolve to the expected version of Python.
#'
#' You can override the version of Python used in a particular project by
#' setting the `RENV_PYTHON` environment variable; e.g. as part of the
#' project's `.Renviron` file. This can be useful if you find that renv
#' is unable to automatically discover a compatible version of Python to
#' be used in the project.
#'
#' @inherit renv-params
#'
#' @param ... Optional arguments; currently unused.
#'
#' @param python
#'   The path to the version of Python to be used with this project. See
#'   **Finding Python** for more details.
#'
#' @param type
#'   The type of Python environment to use. When `"auto"` (the default),
#'   virtual environments will be used.
#'
#' @param name
#'   The name or path that should be used for the associated Python environment.
#'   If `NULL` and `python` points to a Python executable living within a
#'   pre-existing virtual environment, that environment will be used. Otherwise,
#'   a project-local environment will be created instead, using a name
#'   generated from the associated version of Python.
#'
#' @details
#' # Finding Python
#'
#' In interactive sessions, when `python = NULL`, renv will prompt for an
#' appropriate version of Python. renv will search a pre-defined set of
#' locations when attempting to find Python installations on the system:
#'
#' - `getOption("renv.python.root")`,
#' - `/opt/python`,
#' - `/opt/local/python`,
#' - `~/opt/python`,
#' - `/usr/local/opt` (for macOS Homebrew-installed copies of Python),
#' - `/opt/homebrew/opt` (for M1 macOS Homebrew-installed copies of Python),
#' - `~/.pyenv/versions`,
#' - Python instances available on the `PATH`.
#'
#' In non-interactive sessions, renv will first check the `RETICULATE_PYTHON`
#' environment variable; if that is unset, renv will look for Python on the
#' `PATH`. It is recommended that the version of Python to be used is explicitly
#' supplied for non-interactive usages of `use_python()`.
#'
#'
#' # Warning
#'
#' We strongly recommend using Python virtual environments, for a few reasons:
#'
#' 1. If something goes wrong with a local virtual environment, you can safely
#'    delete that virtual environment, and then re-initialize it later, without
#'    worry that doing so might impact other software on your system.
#'
#' 2. If you choose to use a "system" installation of Python, then any packages
#'    you install or upgrade will be visible to any other application that
#'    wants to use that same Python installation. Using a virtual environment
#'    ensures that any changes made are isolated to that environment only.
#'
#' 3. Choosing to use Anaconda will likely invite extra frustration in the
#'    future, as you may be required to upgrade and manage your Anaconda
#'    installation as new versions of Anaconda are released. In addition,
#'    Anaconda installations tend to work poorly with software not specifically
#'    installed as part of that same Anaconda installation.
#'
#' In other words, we recommend selecting "system" or "conda" only if you are an
#' expert Python user who is already accustomed to managing Python / Anaconda
#' installations on your own.
#'
#'
#' @return
#'   `TRUE`, indicating that the requested version of Python has been
#'   successfully activated. Note that this function is normally called for its
#'   side effects.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # use python with a project
#' renv::use_python()
#'
#' # use python with a project; create the environment
#' # within the project directory in the '.venv' folder
#' renv::use_python(name = ".venv")
#'
#' # use python with a pre-existing virtual environment located elsewhere
#' renv::use_python(name = "~/.virtualenvs/env")
#'
#' # use virtualenv python with a project
#' renv::use_python(type = "virtualenv")
#'
#' # use conda python with a project
#' renv::use_python(type = "conda")
#'
#' }
use_python <- function(python = NULL,
                       ...,
                       type = c("auto", "virtualenv", "conda", "system"),
                       name    = NULL,
                       project = NULL)
{
  renv_scope_error_handler()
  renv_dots_check(...)
  project <- renv_project_resolve(project)

  # deactivate python integration when FALSE
  if (identical(python, FALSE))
    return(renv_python_deactivate(project))

  # handle 'auto' type
  type <- match.arg(type)
  if (identical(type, "auto"))
    type <- "virtualenv"

  case(
    type == "system"     ~ renv_use_python_system(python, name, project),
    type == "virtualenv" ~ renv_use_python_virtualenv(python, name, project),
    type == "conda"      ~ renv_use_python_condaenv(python, name, project)
  )
}

renv_use_python_system <- function(python,
                                   name,
                                   project)
{
  # retrieve python information
  python <- renv_python_resolve(python)
  version <- renv_python_version(python)
  info <- renv_python_info(python)

  # if the user ended up selecting a virtualenv or conda python, then
  # just activate those and ignore the 'system' request
  if (identical(info$type, "virtualenv"))
    return(renv_use_python_virtualenv(info$python, name, project))
  if (identical(info$type, "conda"))
    return(renv_use_python_condaenv(info$python, name, project))

  # for 'system' python usages, we just use the path to python
  # (note that this may not be portable or useful for other machines)
  renv_use_python_fini(info, python, version, project)
}

renv_use_python_virtualenv <- function(python,
                                       name,
                                       project)
{
  # if name has been set, check and see if it refers to an already-existing
  # virtual environment; if that exists, use it
  if (is.null(python) && !is.null(name)) {
    path <- renv_python_virtualenv_path(name)
    if (file.exists(path))
      python <- renv_python_exe(name)
  }

  python  <- renv_python_resolve(python)
  version <- renv_python_version(python)
  info    <- renv_python_info(python)

  # if name is unset, and 'python' doesn't already refer to an existing
  # virtual environment, then we'll use a local virtual environment
  local <- is.null(name) && identical(info$type, "virtualenv")
  if (local) {
    name <- renv_path_aliased(info$root)
    if (renv_path_same(dirname(name), renv_python_virtualenv_home()))
      name <- basename(name)
  } else {
    name <- name %||% renv_python_envpath(project, "virtualenv", version)
    if (grepl("/", name, fixed = TRUE))
      name <- renv_path_canonicalize(name)
  }

  # now, check to see if the python environment exists;
  # if it does not exist, we'll create it now
  vpython <- renv_use_python_virtualenv_impl(project, name, version, python)
  vinfo <- renv_python_info(vpython)

  # finish up now
  renv_use_python_fini(vinfo, name, version, project)

}

renv_use_python_condaenv <- function(python,
                                     name,
                                     project)
{
  # if python is set, see if it's already the path to a python interpreter
  # living within a conda environment
  while (!is.null(python)) {

    if (!is.null(name)) {
      fmt <- "ignoring value of name %s as python was already set"
      warningf(fmt, renv_path_pretty(name))
    }

    # validate that this is a conda python
    info <- renv_python_info(python)
    if (!identical(info$type, "conda")) {
      fmt <- "%s does not appear to refer to a Conda instance of Python; ignoring"
      warningf(fmt, renv_path_pretty(python))
      break
    }

    # use this edition of python without further adieu
    version <- renv_python_version(python)
    return(renv_use_python_fini(info, name, version, project))

  }

  # TODO: how do we select which version of python we want to use?
  name <- name %||% renv_python_envpath(project, "conda")
  python <- renv_use_python_condaenv_impl(project, name)
  info <- renv_python_info(python)
  version <- renv_python_version(python)

  renv_use_python_fini(info, name, version, project)

}

renv_use_python_fini <- function(info,
                                 name,
                                 version,
                                 project)
{
  # ensure project-local names are treated as such
  name    <- if (!is.null(name))    path.expand(chartr("\\", "/", name))
  project <- if (!is.null(project)) path.expand(chartr("\\", "/", project))

  if (!is.null(name) && startswith(name, project)) {
    base <- substring(name, nchar(project) + 2L)
    name <- if (grepl("^[.][^/]+$", base)) base else file.path(".", base)
  }

  # form the lockfile fields we'll want to write
  fields <- as.list(c(Version = version, Type = info$type, Name = name))

  # update the lockfile
  lockfile <- renv_lockfile_load(project)
  if (!identical(fields, lockfile$Python)) {
    lockfile$Python <- fields
    renv_lockfile_save(lockfile, project)
  }

  # re-initialize with these settings
  renv_load_python(project, fields)

  # notify user
  if (!renv_tests_running()) {
    if (is.null(info$type)) {
      fmt <- "- Activated Python %s (%s)."
      writef(fmt, version, renv_path_aliased(info$python))
    } else {
      fmt <- "- Activated Python %s [%s; %s]"
      writef(fmt, version, info$type, renv_path_aliased(name))
    }
  }

  # report to user
  setwd(project)
  activate(project = project)

  invisible(info$python)

}

# return the path to an existing python binary associated with the virtual
# environment having name 'name' and version 'version', or "" if no such
# python instance exists
renv_use_python_virtualenv_impl_existing <- function(project,
                                                     name = NULL,
                                                     version = NULL)
{
  # resolve environment path from name
  name <- name %||% renv_python_envpath(project, "virtualenv", version)
  path <- renv_python_virtualenv_path(name)
  if (!file.exists(path))
    return("")

  # check that this appears to have a valid python executable
  info <- catch(renv_python_info(path))
  if (inherits(info, "error")) {
    warning(info)
    return("")
  }

  # validate version and return
  renv_python_virtualenv_validate(path, version)
}

# Internal helper for activating a Python virtual environment
#
# @param project
#   The project directory.
#
# @param name
#   The environment name, if any. If unset, it should be constructed
#   based on the Python executable used (note: _not_ the version parameter)
#
# @param version
#   The _requested_ version of Python (which may not be the actual version!)
#   This version should be used as a hint for finding an appropriate version
#   of Python, if the environment needs to be re-created.
#
# @param python
#   The copy of Python to be used. When unset, an appropriate version of Python
#   should be discovered based on the `version` parameter.
#
# @return
#   The path to the Python binary in the associated virtual environment.
#
renv_use_python_virtualenv_impl <- function(project,
                                            name = NULL,
                                            version = NULL,
                                            python = NULL)
{
  # first, look for an already-existing python installation
  # associated with the requested version of python
  exe <- renv_use_python_virtualenv_impl_existing(project, name, version)
  if (file.exists(exe))
    return(exe)

  # couldn't resolve environment from requested version; try to find
  # a compatible version of python and re-create that environment
  python <- python %||% renv_python_find(version)
  pyversion <- renv_python_version(python)
  name <- name %||% renv_python_envpath(project, "virtualenv", pyversion)
  path <- renv_python_virtualenv_path(name)

  # if the environment already exists, but is associated with a different
  # version of Python, prompt the user to re-create that environment
  if (file.exists(path)) {
    exe <- renv_python_virtualenv_validate(path, version)
    if (file.exists(exe))
      return(exe)
  }

  printf("- Creating virtual environment '%s' ... ", basename(name))
  vpython <- renv_python_virtualenv_create(python, path)
  writef("Done!")

  printf("- Updating Python packages ... ")
  renv_python_virtualenv_update(vpython)
  writef("Done!")

  renv_python_virtualenv_validate(path, version)

}

renv_use_python_condaenv_impl <- function(project,
                                          name = NULL,
                                          version = NULL,
                                          python = NULL)
{
  # if we can't load reticulate, try installing if there is a version
  # recorded in the lockfile
  if (!requireNamespace("reticulate", quietly = TRUE)) {

    # retrieve reticulate record
    lockfile <- renv_lockfile_load(project = project)
    records <- renv_lockfile_records(lockfile)
    reticulate <- records[["reticulate"]]

    # if we have a reticulate record, then attempt to restore
    if (!is.null(reticulate)) {
      restore(packages = "reticulate",
              prompt = FALSE,
              project = project)
    } else {
      install(packages = "reticulate",
              prompt = FALSE,
              project = project)
    }

  }

  # try once more to load reticulate
  if (!requireNamespace("reticulate", quietly = TRUE))
    stopf("use of conda environments requires the 'reticulate' package")

  # TODO: how to handle things like a requested Python version here?
  name <- name %||% renv_python_envpath(project, "conda", version)
  renv_python_conda_select(name, version)
}

renv_python_deactivate <- function(project) {

  file <- renv_lockfile_path(project)
  if (!file.exists(file))
    return(TRUE)

  lockfile <- renv_lockfile_read(file)
  if (is.null(lockfile$Python))
    return(TRUE)

  lockfile$Python <- NULL
  renv_lockfile_write(lockfile, file = file)
  writef("- Deactived Python -- the lockfile has been updated.")
  TRUE

}


# use.R ----------------------------------------------------------------------


the$use_libpath <- NULL

#' @rdname embed
#'
#' @param ...
#'   The \R packages to be used with this script. Ignored if `lockfile` is
#'   non-`NULL`.
#'
#' @param lockfile
#'   The lockfile to use. When supplied, renv will use the packages as
#'   declared in the lockfile.
#'
#' @param library
#'   The library path into which the requested packages should be installed.
#'   When `NULL` (the default), a library path within the \R temporary
#'   directory will be generated and used. Note that this same library path
#'   will be re-used on future calls to `renv::use()`, allowing `renv::use()`
#'   to be used multiple times within a single script.
#'
#' @param isolate
#'   Boolean; should the active library paths be included in the set of library
#'   paths activated for this script? Set this to `TRUE` if you only want the
#'   packages provided to `renv::use()` to be visible on the library paths.
#'
#' @param sandbox
#'   Should the system library be sandboxed? See the sandbox documentation in
#'   [renv::config] for more details. You can also provide an explicit sandbox
#'   path if you want to configure where `renv::use()` generates its sandbox.
#'   By default, the sandbox is generated within the \R temporary directory.
#'
#' @param attach
#'   Boolean; should the set of requested packages be automatically attached?
#'   If `TRUE`, packages will be loaded and attached via a call
#'   to [library()] after install. Ignored if `lockfile` is non-`NULL`.
#'
#' @param verbose
#'   Boolean; be verbose while installing packages?
#'
#' @return
#'   This function is normally called for its side effects.
#'
#' @export
use <- function(...,
                lockfile = NULL,
                library  = NULL,
                isolate  = sandbox,
                sandbox  = TRUE,
                attach   = FALSE,
                verbose  = TRUE)
{

  # allow use of the cache in this context
  renv_scope_options(renv.cache.linkable = TRUE)

  # set up sandbox if requested
  renv_use_sandbox(sandbox)

  # prepare library and activate library
  library <- library %||% renv_use_libpath()
  ensure_directory(library)

  # set library paths
  libpaths <- c(library, if (!isolate) .libPaths())
  renv_libpaths_set(libpaths)

  # if we were supplied a lockfile, use it
  if (!is.null(lockfile)) {
    renv_scope_options(renv.verbose = verbose)
    records <- restore(lockfile = lockfile, clean = FALSE, prompt = FALSE)
    return(invisible(records))
  }

  dots <- list(...)
  if (empty(dots))
    return(invisible())

  # resolve the provided remotes
  remotes <- lapply(dots, renv_remotes_resolve)
  names(remotes) <- map_chr(remotes, `[[`, "Package")

  # install packages
  records <- local({
    renv_scope_options(renv.verbose = verbose)
    install(packages = remotes, library = library, prompt = FALSE)
  })

  # automatically load the requested remotes
  if (attach) {
    enumerate(remotes, function(package, remote) {
      library(package, character.only = TRUE)
    })
  }

  # return set of installed packages
  invisible(records)

}

renv_use_libpath <- function() {
  (the$use_libpath <- the$use_libpath %||% tempfile("renv-use-libpath-"))
}

renv_use_sandbox <- function(sandbox) {

  if (identical(sandbox, FALSE))
    return(FALSE)

  if (renv_sandbox_activated())
    return(TRUE)

  sandbox <- if (is.character(sandbox))
    sandbox
  else
    file.path(tempdir(), "renv-sandbox")

  renv_scope_options(renv.config.sandbox.enabled = TRUE)
  renv_sandbox_activate_impl(sandbox = sandbox)

}


# utils-connections.R --------------------------------------------------------


textfile <- function(description, open = "wt") {
  file(description, open = open, encoding = "native.enc")
}


# utils-format.R -------------------------------------------------------------


stopf <- function(fmt = "", ..., call. = FALSE) {
  stop(sprintf(fmt, ...), call. = call.)
}

warningf <- function(fmt = "", ..., call. = FALSE, immediate. = FALSE) {
  warning(sprintf(fmt, ...), call. = call., immediate. = immediate.)
}

printf <- function(fmt = "", ..., file = stdout(), sep = "") {
  if (!is.null(fmt) && renv_verbose())
    cat(sprintf(fmt, ...), file = file, sep = sep)
}

writef <- function(fmt = "", ..., con = stdout()) {
  if (!is.null(fmt) && renv_verbose())
    writeLines(sprintf(fmt, ...), con = con)
}

info_bullet <- function() {
  if (l10n_info()$`UTF-8`) "\u2139" else "i"
}


# utils-map.R ----------------------------------------------------------------


bapply <- function(x, f, ..., index = "Index") {
  result <- lapply(x, f, ...)
  bind(result, index = index)
}

enumerate <- function(x, f, ..., FUN.VALUE = NULL) {

  n <- names(x)
  idx <- named(seq_along(x), n)
  callback <- function(i) f(n[[i]], x[[i]], ...)

  if (is.environment(x))
    x <- as.list(x, all.names = TRUE)

  if (is.null(FUN.VALUE))
    lapply(idx, callback)
  else
    vapply(idx, callback, FUN.VALUE = FUN.VALUE)

}

enum_chr <- function(x, f, ...) {
  enumerate(x, f, ..., FUN.VALUE = character(1))
}

enum_int <- function(x, f, ...) {
  enumerate(x, f, ..., FUN.VALUE = integer(1))
}

enum_dbl <- function(x, f, ...) {
  enumerate(x, f, ..., FUN.VALUE = double(1))
}

enum_lgl <- function(x, f, ...) {
  enumerate(x, f, ..., FUN.VALUE = logical(1))
}


uapply <- function(x, f, ...) {
  f <- match.fun(f)
  unlist(lapply(x, f, ...), recursive = FALSE)
}

filter <- function(x, f, ...) {
  f <- match.fun(f)
  x[map_lgl(x, f, ...)]
}

reject <- function(x, f, ...) {
  f <- match.fun(f)
  x[!map_lgl(x, f, ...)]
}

map <- function(x, f, ...) {
  f <- match.fun(f)
  lapply(x, f, ...)
}

map_chr <- function(x, f, ...) {
  f <- match.fun(f)
  vapply(x, f, ..., FUN.VALUE = character(1))
}

map_dbl <- function(x, f, ...) {
  f <- match.fun(f)
  vapply(x, f, ..., FUN.VALUE = numeric(1))
}

map_int <- function(x, f, ...) {
  f <- match.fun(f)
  vapply(x, f, ..., FUN.VALUE = integer(1))
}

map_lgl <- function(x, f, ...) {
  f <- match.fun(f)
  vapply(x, f, ..., FUN.VALUE = logical(1))
}


extract <- function(x, ...) {
  lapply(x, `[[`, ...)
}

extract_chr <- function(x, ...) {
  vapply(x, `[[`, ..., FUN.VALUE = character(1))
}

extract_dbl <- function(x, ...) {
  vapply(x, `[[`, ..., FUN.VALUE = numeric(1))
}

extract_int <- function(x, ...) {
  vapply(x, `[[`, ..., FUN.VALUE = integer(1))
}

extract_lgl <- function(x, ...) {
  vapply(x, `[[`, ..., FUN.VALUE = logical(1))
}


# utils.R --------------------------------------------------------------------


`%>%` <- function(...) {

  dots <- eval(substitute(alist(...)))
  if (length(dots) != 2L)
    stopf("`%>%` called with invalid number of arguments")

  lhs <- dots[[1L]]; rhs <- dots[[2L]]
  if (!is.call(rhs))
    stopf("right-hand side of rhs is not a call")

  data <- c(rhs[[1L]], lhs, as.list(rhs[-1L]))
  call <- as.call(data)

  nm <- names(rhs)
  if (length(nm))
    names(call) <- c("", "", nm[-1L])

  eval(call, envir = parent.frame())

}

`%NA%` <- function(x, y) {
  if (length(x) && is.na(x)) y else x
}

`%&&%` <- function(x, y) {
  if (length(x)) y
}

lines <- function(...) {
  paste(..., sep = "\n")
}

is_named <- function(x) {
  nm <- names(x)
  !is.null(nm) && all(nzchar(nm))
}

named <- function(object, names = object) {
  names(object) <- names
  object
}

empty <- function(x) {
  length(x) == 0L
}

zlength <- function(x) {
  length(x) != 0L
}

trim <- function(x) {
  gsub("^\\s+|\\s+$", "", x, perl = TRUE)
}

trimws <- function(x) {
  gsub("^\\s+|\\s+$", "", x, perl = TRUE)
}

case <- function(...) {

  dots <- eval(substitute(alist(...)))
  for (i in seq_along(dots)) {

    if (identical(dots[[i]], quote(expr = )))
      next

    dot <- eval(dots[[i]], envir = parent.frame())
    if (!inherits(dot, "formula"))
      return(dot)

    # Silence R CMD check note
    expr <- NULL
    cond <- NULL

    # use delayed assignments below so we can allow return statements to
    # be handled in the lexical scope where they were defined
    if (length(dot) == 2L) {
      do.call(delayedAssign, list("expr", dot[[2L]], eval.env = environment(dot)))
      return(expr)
    }

    do.call(delayedAssign, list("cond", dot[[2L]], eval.env = environment(dot)))
    do.call(delayedAssign, list("expr", dot[[3L]], eval.env = environment(dot)))
    if (cond) return(expr)

  }

}

compose <- function(wrapper, callback) {
  function(...) wrapper(callback(...))
}

catch <- function(expr) {
  tryCatch(
    withCallingHandlers(expr, error = renv_error_capture),
    error = renv_error_tag
  )
}

catchall <- function(expr) {
  tryCatch(
    withCallingHandlers(expr, condition = renv_error_capture),
    condition = renv_error_tag
  )
}

# nocov start

ask <- function(question, default = FALSE) {

  if (renv_tests_running())
    return(TRUE)

  enabled <- getOption("renv.prompt.enabled", default = TRUE)
  if (!enabled)
    return(default)

  if (!interactive())
    return(default)

  # be verbose in this scope, as we're asking the user for input
  renv_scope_options(renv.verbose = TRUE)

  repeat {

    # solicit user's answer
    selection <- if (default) "[Y/n]" else "[y/N]"
    prompt <- sprintf("%s %s: ", question, selection)
    response <- tryCatch(
      tolower(trimws(readline(prompt))),
      interrupt = identity
    )

    # check for interrupts; treat as abort request
    cancel_if(inherits(response, "interrupt"))

    # use default when no response
    if (!nzchar(response))
      return(default)

    # check for 'yes' responses
    if (response %in% c("y", "yes")) {
      writef("")
      return(TRUE)
    }

    # check for 'no' responses
    if (response %in% c("n", "no")) {
      writef("")
      return(FALSE)
    }

    # ask the user again
    writef("- Unrecognized response: please enter 'y' or 'n', or type Ctrl + C to cancel.")

  }

}

proceed <- function(default = TRUE) {
  ask("Do you want to proceed?", default = default)
}

menu <- function(choices, title, default = 1L) {
  testing <- getOption("renv.menu.choice", integer())
  if (length(testing)) {
    selected <- testing[[1]]
    options(renv.menu.choice = testing[-1])
  } else if (is_testing()) {
    selected <- default
  } else {
    selected <- NULL
  }

  if (!is.null(selected)) {
    writef(c(
      title,
      "",
      paste0(seq_along(choices), ": ", choices),
      "",
      paste0("Selection: ", selected),
      ""
    ))
    return(names(choices)[selected])
  }

  if (!interactive()) {
    writef(c("Not interactive. Will:", choices[[default]]))
    return(default)
  }

  idx <- tryCatch(
    utils::menu(choices, paste(title, collapse = "\n"), graphics = FALSE),
    interrupt = function(cnd) 0L
  )
  if (idx == 0L) {
    "cancel"
  } else {
    names(choices)[idx]
  }
}

# nocov end

inject <- function(contents,
                   pattern,
                   replacement,
                   anchor = NULL,
                   fixed  = FALSE)
{
  # first, check to see if the pattern matches a line
  index <- grep(pattern, contents, perl = !fixed, fixed = fixed)
  if (length(index)) {
    contents[index] <- replacement
    return(contents)
  }

  # otherwise, check for the anchor, and insert after
  index <- if (!is.null(anchor))
    grep(anchor, contents, perl = !fixed, fixed = fixed)

  if (!length(index))
    return(c(contents, replacement))

  c(
    head(contents, n = index),
    replacement,
    tail(contents, n = -index)
  )
}

deparsed <- function(value, width = 60L) {
  paste(deparse(value, width.cutoff = width), collapse = "\n")
}

read <- function(file) {
  renv_scope_options(warn = -1L)
  contents <- readLines(file, warn = FALSE)
  paste(contents, collapse = "\n")
}

plural <- function(word, n) {
  if (n == 1) word else paste(word, "s", sep = "")
}

nplural <- function(word, n) {
  paste(n, plural(word, n))
}

trunc <- function(text, n = 78) {
  long <- nchar(text) > n
  text[long] <- sprintf("%s <...>", substring(text[long], 1, n - 6))
  text
}

endswith <- function(string, suffix) {
  substring(string, nchar(string) - nchar(suffix) + 1) == suffix
}

# like tools::file_ext, but includes leading '.', and preserves
# '.tar.gz', '.tar.bz' and so on
fileext <- function(path, default = "") {
  indices <- regexpr("[.]((?:tar[.])?[[:alnum:]]+)$", path, perl = TRUE)
  ifelse(indices > -1L, substring(path, indices), default)
}

visited <- function(name, envir) {
  value <- envir[[name]] %||% FALSE
  envir[[name]] <- TRUE
  value
}

rowapply <- function(X, FUN, ...) {
  lapply(seq_len(NROW(X)), function(I) {
    FUN(X[I, , drop = FALSE], ...)
  })
}

comspec <- function() {
  Sys.getenv("COMSPEC", unset = Sys.which("cmd.exe"))
}

nullfile <- function() {
  if (renv_platform_windows()) "NUL" else "/dev/null"
}

quietly <- function(expr, sink = TRUE) {

  if (sink) {
    sink(file = nullfile())
    defer(sink(NULL))
  }

  withCallingHandlers(
    expr,
    warning               = function(c) invokeRestart("muffleWarning"),
    message               = function(c) invokeRestart("muffleMessage"),
    packageStartupMessage = function(c) invokeRestart("muffleMessage")
  )

}

# NOTE: This function can be used in preference to `as.*()` if you'd like
# to preserve attributes on the incoming object 'x'.
convert <- function(x, type) {
  storage.mode(x) <- type
  x
}

remap <- function(x, map) {

  # TODO: use match?
  remapped <- x
  enumerate(map, function(key, val) {
    remapped[remapped == key] <<- val
  })
  remapped

}

keep <- function(x, keys) {
  x[intersect(keys, names(x))]
}

exclude <- function(x, keys) {
  x[setdiff(names(x), keys)]
}

invoke <- function(callback, ...) {
  callback(...)
}

dequote <- function(strings) {

  for (quote in c("'", '"')) {

    # find strings matching pattern
    pattern <- paste0(quote, "(.*)", quote)
    matches <- grep(pattern, strings, perl = TRUE)
    if (empty(matches))
      next

    # remove outer quotes
    strings[matches] <- gsub(pattern, "\\1", strings[matches], perl = TRUE)

    # un-escape inner quotes
    pattern <- paste0("\\", quote)
    strings[matches] <- gsub(pattern, quote, strings[matches], fixed = TRUE)
  }

  strings

}

nth <- function(x, i) {
  x[[i]]
}

heredoc <- function(text, leave = 0) {

  # remove leading, trailing whitespace
  trimmed <- gsub("^\\s*\\n|\\n\\s*$", "", text)

  # split into lines
  lines <- strsplit(trimmed, "\n", fixed = TRUE)[[1L]]

  # compute common indent
  indent <- regexpr("[^[:space:]]", lines)
  common <- min(setdiff(indent, -1L)) - leave
  paste(substring(lines, common), collapse = "\n")

}

find <- function(x, f, ...) {
  for (i in seq_along(x))
    if (!is.null(value <- f(x[[i]], ...)))
      return(value)
}

recursing <- function() {

  nf <- sys.nframe()
  if (nf < 2L)
    return(FALSE)

  np <- sys.parent()
  fn <- sys.function(np)
  for (i in seq_len(np - 1L))
    if (identical(fn, sys.function(i)))
      return(TRUE)

  FALSE

}

csort <- function(x, decreasing = FALSE, ...) {
  renv_scope_locale("LC_COLLATE", "C")
  sort(x, decreasing, ...)
}

fsub <- function(pattern, replacement, x, ignore.case = FALSE, useBytes = FALSE) {
  sub(pattern, replacement, x, ignore.case = ignore.case, useBytes = useBytes, fixed = TRUE)
}

rows <- function(data, indices) {

  # convert logical values
  if (is.logical(indices)) {
    if (length(indices) < nrow(data))
      indices <- rep(indices, length.out = nrow(data))
    indices <- which(indices, useNames = FALSE)
  }

  # build output list
  output <- vector("list", length(data))
  for (i in seq_along(data))
    output[[i]] <- .subset2(data, i)[indices]

  # copy relevant attributes
  attrs <- attributes(data)
  attrs[["row.names"]] <- .set_row_names(length(indices))
  attributes(output) <- attrs

  # return new data.frame
  output

}

cols <- function(data, indices) {

  # perform subset
  output <- .subset(data, indices)

  # copy relevant attributes
  attrs <- attributes(data)
  attrs[["names"]] <- attr(output, "names", exact = TRUE)
  attributes(output) <- attrs

  # return output
  output

}

stringify <- function(object, collapse = " ") {

  if (is.symbol(object))
    return(as.character(object))

  paste(
    deparse(object, width.cutoff = 500L),
    collapse = collapse
  )

}

env <- function(...) {
  list2env(list(...), envir = new.env(parent = emptyenv()))
}

env2list <- function(env) {
  as.list.environment(env, all.names = TRUE)
}

chop <- function(x, split = "\n", fixed = TRUE, perl = FALSE, useBytes = FALSE) {
  strsplit(x, split, !perl, perl, useBytes)[[1L]]
}

prof <- function(expr, ...) {

  profile <- tempfile("renv-profile-", fileext = ".Rprof")

  Rprof(profile, ...)
  result <- expr
  Rprof(NULL)
  print(summaryRprof(profile))

  invisible(result)

}

recycle <- function(data) {

  # compute number of columns
  n <- lengths(data, use.names = FALSE)
  nrow <- max(n)

  # start recycling
  for (i in seq_along(data)) {
    if (n[[i]] == 0L) {
      length(data[[i]]) <- nrow
    } else if (n[[i]] != nrow) {
      data[[i]] <- rep.int(data[[i]], nrow / n[[i]])
    }
  }

  data

}

take <- function(data, index = NULL) {
  if (is.null(index)) data else .subset2(data, index)
}

cancel <- function() {

  renv_snapshot_auto_suppress_next()
  if (is_testing())
    stop("Operation canceled", call. = FALSE)

  message("- Operation canceled.")
  invokeRestart("abort")

}

cancel_if <- function(cnd) {
  if (cnd) cancel()
}

rep_named <- function(names, x) {
  values <- rep_len(x, length(names))
  names(values) <- names
  values
}

wait_until <- function(callback, ...) {
  repeat if (callback(...)) return(TRUE)
}

timer <- function(units = "secs") {

  .time <- Sys.time()
  .units <- units

  list(

    now = function() {
      Sys.time()
    },

    elapsed = function() {
      difftime(Sys.time(), .time, units = .units)
    }
  )

}

summon <- function() {
  envir <- do.call(attach, list(what = NULL, name = "renv"))
  renv <- renv_envir_self()
  list2env(as.list(renv), envir = envir)
}

assert <- function(...) stopifnot(...)

overlay <- function(lhs, rhs) {
  modifyList(as.list(lhs), as.list(rhs))
}

# the 'top' renv function in the call stack
topfun <- function() {

  self <- renv_envir_self()
  frames <- sys.frames()

  for (i in seq_along(frames))
    if (identical(self, parent.env(frames[[i]])))
      return(sys.function(i))

}

warnify <- function(cnd) {
  class(cnd) <- c("warning", "condition")
  warning(cnd)
}


# vector.R -------------------------------------------------------------------


# these functions are like the base R equivalents, but preserve names
renv_vector_diff <- function(x, y) {
  x[match(x, y, 0L) == 0L]
}

renv_vector_intersect <- function(x, y) {
  y[match(x, y, 0L)]
}

renv_vector_unique <- function(x) {
  x[!duplicated(x)]
}


# vendor.R -------------------------------------------------------------------


#' Vendor renv in an R package
#'
#' @description
#' Calling `renv:::vendor()` will:
#'
#' - Compile a vendored copy of renv to `inst/vendor/renv.R`,
#' - Generate an renv auto-loader at `R/renv.R`.
#'
#' Using this, projects can take a dependency on renv, and use renv
#' internals, in a CRAN-compliant way. After vendoring renv, you can
#' use renv APIs in your package via the embedded renv environment;
#' for example, you could call the [renv::dependencies()] function with:
#'
#' ```
#' renv$dependencies()
#' ```
#'
#' Be aware that renv internals might change in future releases, so if you
#' need to rely on renv internal functions, we strongly recommend testing
#' your usages of these functions to avoid potential breakage.
#'
#' @param version The version of renv to vendor. `renv` sources will be pulled
#'   from GitHub, and so `version` should refer to either a commit hash or a
#'   branch name.
#'
#' @param project The project in which renv should be vendored.
#'
#' @keywords internal
#'
vendor <- function(version = "main", project = getwd()) {
  renv_scope_error_handler()

  # validate project is a package
  descpath <- file.path(project, "DESCRIPTION")
  if (!file.exists(descpath)) {
    fmt <- "%s does not contain a DESCRIPTION file; cannot proceed"
    stopf(fmt, renv_path_pretty(project))
  }

  # retrieve package sources
  sources <- renv_vendor_sources(version)

  # compute package remote
  spec <- sprintf("rstudio/renv@%s", version)
  remote <- renv_remotes_resolve(spec)

  # build script header
  header <- renv_vendor_header(remote)

  # create the renv script itself
  embed <- renv_vendor_create(
    project    = project,
    sources    = sources,
    header     = header
  )

  # create the loader
  loader <- renv_vendor_loader(project, remote, header)

  # let the user know what just happened
  template <- heredoc("
    #
    # A vendored copy of renv was created at: %s
    # The renv auto-loader was generated at:  %s
    #
    # Please add `renv$initialize()` to your package's `.onLoad()`
    # to ensure that renv is initialized on package load.
    #
  ")

  writef(template, renv_path_pretty(embed), renv_path_pretty(loader))

  invisible(TRUE)
}

renv_vendor_create <- function(project, sources, header) {

  # find all the renv R source scripts
  scripts <- list.files(file.path(sources, "R"), full.names = TRUE)

  # read into a single file
  contents <- map_chr(scripts, function(script) {
    header <- header(basename(script), n = 78L)
    contents <- readLines(script)
    parts <- c(header, "", contents, "", "")
    paste(parts, collapse = "\n")
  })

  # paste into single script
  bundle <- paste(contents, collapse = "\n")
  all <- c(header, "", bundle)

  # write to file
  target <- file.path(project, "inst/vendor/renv.R")
  ensure_parent_directory(target)
  writeLines(all, con = target)

  # return generated bundle
  invisible(target)

}

renv_vendor_loader <- function(project, remote, header) {

  source <- system.file("resources/vendor/renv.R", package = "renv")
  template <- readLines(source, warn = FALSE)

  # replace '..imports..' with the imports we use
  imports <- renv_vendor_imports()

  # create metadata for the embedded version
  version <- renv_metadata_version_create(remote)
  metadata <- renv_metadata_create(embedded = TRUE, version = version)

  # format metadata for template insertion
  lines <- enum_chr(metadata, function(key, value) {
    sprintf("    %s = %s", key, deparse(value))
  })

  inner <- paste(lines, collapse = ",\n")

  replacements <- list(
    imports  = imports,
    metadata = paste(c("list(", inner, "  )"), collapse = "\n")
  )
  contents <- renv_template_replace(template, replacements, format = "..%s..")

  all <- c("", header, "", contents)
  target <- file.path(project, "R/renv.R")
  ensure_parent_directory(target)
  writeLines(all, con = target)

  invisible(target)

}

renv_vendor_imports <- function() {

  imports <- getNamespaceImports("renv")

  # collect into sane format
  packages <- setdiff(unique(names(imports)), c("base", ""))
  names(packages) <- packages
  table <- map(packages, function(package) {
    unlist(imports[names(imports) == package], use.names = FALSE)
  })

  # format nicely
  entries <- enum_chr(table, function(package, functions) {
    lines <- sprintf("      \"%s\"", functions)
    body <- paste(lines, collapse = ",\n")
    parts <- c(sprintf("    %s = c(", package), body, "    )")
    paste(parts, collapse = "\n")
  })

  paste(c("list(", paste(entries, collapse = ",\n"), "  )"), collapse = "\n")

}

renv_vendor_sources <- function(version) {

  # retrieve renv
  tarball <- renv_bootstrap_download_github(version = version)

  # extract downloaded sources
  untarred <- tempfile("renv-vendor-")
  untar(tarball, exdir = untarred)

  # the package itself will exist as a folder within 'exdir'
  list.files(untarred, full.names = TRUE)[[1L]]

}

renv_vendor_header <- function(remote) {

  template <- heredoc("
    #
    # renv %s [rstudio/renv#%s]: A dependency management toolkit for R.
    # Generated using `renv:::vendor()` at %s.
    #
  ")

  version <- remote$Version
  hash <- substring(remote$RemoteSha, 1L, 7L)
  sprintf(template, version, hash, Sys.time())

}


# verbose.R ------------------------------------------------------------------


renv_verbose <- function() {

  verbose <- getOption("renv.verbose")
  if (!is.null(verbose))
    return(as.logical(verbose))

  verbose <- Sys.getenv("RENV_VERBOSE", unset = NA)
  if (!is.na(verbose))
    return(as.logical(verbose))

  if (is_testing())
    return(FALSE)

  interactive() || !renv_tests_running()

}

# NOTE: Prefer using 'is_testing()' to 'renv_tests_running()' for behavior
# that should apply regardless of the package currently being tested.
#
# renv_tests_running() is appropriate when running renv's own tests.
is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}


# version.R ------------------------------------------------------------------


renv_version_compare <- function(lhs, rhs, n = NULL) {

  # retrieve versions as integer vector
  lhs <- unlist(unclass(numeric_version(lhs)))
  rhs <- unlist(unclass(numeric_version(rhs)))

  # compute number of components to compare
  n <- n %||% max(length(lhs), length(rhs))

  # pad each vector with zeroes up to the requested length
  lhs <- c(lhs, rep.int(0L, max(0L, n - length(lhs))))
  rhs <- c(rhs, rep.int(0L, max(0L, n - length(rhs))))

  # iterate through each component and compare
  for (i in seq_len(n)) {
    if (lhs[[i]] < rhs[[i]])
      return(-1L)
    else if (lhs[[i]] > rhs[[i]])
      return(+1L)
  }

  # if we got here, then all components compared equal
  0L

}

renv_version_le <- function(lhs, rhs, n = NULL) {
  renv_version_compare(lhs, rhs, n) <= 0L
}

renv_version_lt <- function(lhs, rhs, n = NULL) {
  renv_version_compare(lhs, rhs, n) <  0L
}

renv_version_eq <- function(lhs, rhs, n = NULL) {
  renv_version_compare(lhs, rhs, n) == 0L
}

renv_version_gt <- function(lhs, rhs, n = NULL) {
  renv_version_compare(lhs, rhs, n) >  0L
}

renv_version_ge <- function(lhs, rhs, n = NULL) {
  renv_version_compare(lhs, rhs, n) >= 0L
}

renv_version_match <- function(versions, request) {

  nrequest <- unclass(numeric_version(request))[[1L]]
  for (i in rev(seq_along(nrequest))) {

    matches <- which(map_lgl(versions, function(version) {
      renv_version_eq(version, request, n = i)
    }))

    if (!length(matches))
      next

    # TODO: should '3.1' match the closest match (e.g. '3.2') or
    # highest match (e.g. '3.6')?
    sorted <- matches[sort(names(matches), decreasing = TRUE)]
    return(names(sorted)[[1L]])

  }

  versions[[1L]]

}

renv_version_parts <- function(version, n) {

  # split version into parts
  parts <- unclass(as.numeric_version(version))[[1L]]

  # extend parts to size of n
  diff <- max(n) - length(parts)
  if (diff > 0)
    parts <- c(parts, rep.int(0L, diff))

  # retrieve possibly-extended parts
  parts[1:n]

}

renv_version_maj_min <- function(version) {
  parts <- renv_version_parts(version, 2L)
  paste(parts, collapse = ".")
}

renv_version_length <- function(version) {
  nv <- as.numeric_version(version)
  length(unclass(nv)[[1L]])
}


# virtualization.R -----------------------------------------------------------


the$virtualization_type <- NULL

renv_virtualization_init <- function() {

  type <- tryCatch(
    renv_virtualization_type_impl(),
    error = function(e) "unknown"
  )

  the$virtualization_type <- type

}

renv_virtualization_type <- function() {
  the$virtualization_type
}

renv_virtualization_type_impl <- function() {

  # only done on linux for now
  if (!renv_platform_linux())
    return("native")

  # check for cgroup
  if (file.exists("/proc/1/cgroup")) {
    contents <- readLines("/proc/1/cgroup")
    if (any(grepl("/docker/", contents)))
      return("docker")
  }

  # assume native otherwise
  "native"

}


# warnings.R -----------------------------------------------------------------


renv_warnings_unknown_sources <- function(records) {

  if (empty(records))
    return(FALSE)

  # TODO: Should this be documented?
  enabled <- renv_config_get(
    name    = "unknown.sources",
    scope   = "warnings",
    type    = "logical[1]",
    default = TRUE
  )

  if (!enabled)
    return(FALSE)

  renv_scope_options(renv.verbose = TRUE)
  renv_pretty_print_records(
    "The following package(s) were installed from an unknown source:",
    records,
    c(
      "renv may be unable to restore these packages in the future.",
      "Consider reinstalling these packages from a known source (e.g. CRAN)."
    )
  )

  return(TRUE)

}


# watchdog-server.R ----------------------------------------------------------


renv_watchdog_server_start <- function(client) {

  # initialize logging
  renv_log_init()

  # create socket server
  server <- renv_socket_server()
  dlog("watchdog-server", "Listening on port %i.", server$port)

  # communicate information back to client
  dlog("watchdog-server", "Waiting for client...")
  metadata <- list(port = server$port, pid = server$pid)
  conn <- renv_socket_connect(port = client$port, open = "wb")
  serialize(metadata, connection = conn)
  close(conn)
  dlog("watchdog-server", "Synchronized with client.")

  # initialize locks
  lockenv <- new.env(parent = emptyenv())

  # start listening for connections
  repeat tryCatch(
    renv_watchdog_server_run(server, client, lockenv),
    error = function(e) {
      dlog("watchdog-server", "Error: %s", conditionMessage(e))
    }
  )

}

renv_watchdog_server_run <- function(server, client, lockenv) {

  # check for parent exit
  if (!renv_process_exists(client$pid)) {
    dlog("watchdog-server", "Client process has exited; shutting down.")
    renv_watchdog_server_exit(server, client, lockenv)
  }

  # set file time on owned locks, so we can see they're not orphaned
  dlog("watchdog-server", "Refreshing lock times.")
  locks <- ls(envir = lockenv, all.names = TRUE)
  renv_lock_refresh(locks)

  # wait for connection
  dlog("watchdog-server", "Waiting for connection...")
  conn <- renv_socket_accept(server$socket, open = "rb", timeout = 1)
  defer(close(conn))

  # read the request
  dlog("watchdog-server", "Received connection; reading data.")
  request <- unserialize(conn)

  dlog("watchdog-server", "Received request.")
  str(request)

  # handle the request
  switch(

    request$method %||% "<missing>",

    ListLocks = {
      dlog("watchdog-server", "Executing 'ListLocks' request.")
      conn <- renv_socket_connect(port = request$port, open = "watchdog-server", "b")
      defer(close(conn))
      locks <- ls(envir = lockenv, all.names = TRUE)
      serialize(locks, connection = conn)
    },

    LockAcquired = {
      dlog("watchdog-server", "Acquired lock on path '%s'.", request$data$path)
      assign(request$data$path, TRUE, envir = lockenv)
    },

    LockReleased = {
      dlog("watchdog-server", "Released lock on path '%s'.", request$data$path)
      rm(list = request$data$path, envir = lockenv)
    },

    Shutdown = {
      dlog("watchdog-server", "Received shutdown request; shutting down.")
      renv_watchdog_server_exit(server, client, lockenv)
    },

    "<missing>" = {
      dlog("watchdog-server", "Received request with no method field available.")
    },

    {
      dlog("watchdog-server", "Unknown method '%s'", request$method)
    }

  )

}

renv_watchdog_server_exit <- function(server, client, lockenv) {

  # remove any existing locks
  locks <- ls(envir = lockenv, all.names = TRUE)
  unlink(locks, recursive = TRUE, force = TRUE)

  # shut down the socket server
  close(server$socket)

  # quit
  quit(status = 0)

}


# watchdog.R -----------------------------------------------------------------


# whether or not the user has enabled the renv watchdog in this session
the$watchdog_enabled <- FALSE

# metadata related to the running watchdog process, if any
the$watchdog_process <- NULL

renv_watchdog_init <- function() {
  the$watchdog_enabled <- renv_watchdog_enabled_impl()
}

renv_watchdog_enabled <- function() {
  the$watchdog_enabled
}

renv_watchdog_check <- function() {

  if (!renv_watchdog_enabled())
    return(FALSE)

  if (renv_watchdog_running())
    return(TRUE)

  renv_watchdog_start()

}

renv_watchdog_enabled_impl <- function() {

  # skip in older versions of R; we require newer APIs
  if (getRversion() < "4.0.0")
    return(FALSE)

  # skip if explicitly disabled via envvar
  enabled <- Sys.getenv("RENV_WATCHDOG_ENABLED", unset = NA)
  if (!is.na(enabled))
    return(truthy(enabled))

  # disable on Windows; need to understand CI test failures
  # https://github.com/rstudio/renv/actions/runs/5273668333/jobs/9537353788#step:6:242
  if (renv_platform_windows())
    return(FALSE)

  # skip during R CMD check (but not when running tests)
  checking <- renv_envvar_exists("_R_CHECK_PACKAGE_NAME_")
  if (checking && !is_testing())
    return(FALSE)

  # skip during R CMD build or R CMD INSTALL
  # ... unless we are running tests on CI
  building <-
    renv_envvar_exists("R_PACKAGE_NAME") ||
    renv_envvar_exists("R_PACKAGE_DIR")

  if (building) {
    ci <- Sys.getenv("CI", unset = "FALSE")
    if (!truthy(ci))
      return(FALSE)
  }

  # ok, we're enabled
  TRUE

}

renv_watchdog_start <- function() {

  the$watchdog_enabled <- tryCatch(
    renv_watchdog_start_impl(),
    error = function(e) {
      warning(conditionMessage(e))
      FALSE
    }
  )

}

renv_watchdog_start_impl <- function() {

  # create a socket server -- this is used so the watchdog process
  # can communicate what port it'll be listening on for messages
  dlog("watchdog", "launching watchdog")
  server <- renv_socket_server()
  socket <- server$socket; port <- server$port
  defer(close(socket))

  # generate script to invoke watchdog
  script <- renv_scope_tempfile("renv-watchdog-", fileext = ".R")

  # figure out library path -- need to dodge devtools::load_all()
  library <- dirname(renv_namespace_path(.packageName))
  if (!file.exists(file.path(library, "Meta/package.rds")))
    library <- renv_libpaths_default()

  # for R CMD check
  name <- .packageName
  pid <- Sys.getpid()

  env <- list(
    name    = name,
    library = library,
    pid     = pid,
    port    = port
  )

  code <- substitute(env = env, {
    client <- list(pid = pid, port = port)
    host <- loadNamespace(name, lib.loc = library)
    renv <- if (!is.null(host$renv)) host$renv else host
    renv$renv_watchdog_server_start(client)
  })

  writeLines(deparse(code), con = script)

  # debug logging
  debugging <- Sys.getenv("RENV_WATCHDOG_DEBUG", unset = "FALSE")
  stdout <- stderr <- if (truthy(debugging)) "" else FALSE

  # launch the watchdog
  system2(
    command = R(),
    args = c("--vanilla", "-s", "-f", renv_shell_path(script)),
    stdout = stdout,
    stderr = stderr,
    wait = FALSE
  )

  # wait for connection from watchdog server
  dlog("watchdog", "watchdog process launched; waiting for message")
  conn <- catch(renv_socket_accept(socket, open = "rb", timeout = 10))
  if (inherits(conn, "error")) {
    dlog("watchdog", paste("error connecting to watchdog:", conditionMessage(conn)))
    return(FALSE)
  }

  # store information about the running process
  the$watchdog_process <- unserialize(conn)
  close(conn)

  # return TRUE to indicate process was started
  dlog("watchdog", "watchdog message received [pid == %i]", the$watchdog_process$pid)
  TRUE

}

renv_watchdog_notify <- function(method, data = list()) {

  tryCatch(
    renv_watchdog_notify_impl(method, data),
    error = warnify
  )

}

renv_watchdog_notify_impl <- function(method, data = list()) {

  # make sure the watchdog is running
  if (!renv_watchdog_check())
    return(FALSE)

  # connect to the running server
  port <- renv_watchdog_port()
  conn <- renv_socket_connect(port, open = "wb")

  # close the connection on exit
  defer(close(conn))

  # write message
  message <- list(method = method, data = data)
  serialize(message, connection = conn)

  # TRUE indicates message was written
  TRUE

}

renv_watchdog_request <- function(method, data = list()) {
  tryCatch(
    renv_watchdog_request_impl(method, data),
    error = warnify
  )
}

renv_watchdog_request_impl <- function(method, data = list()) {

  # make sure the watchdog is running
  if (!renv_watchdog_check())
    return(FALSE)

  # connect to the running server
  port <- renv_watchdog_port()
  outgoing <- renv_socket_connect(port, open = "wb")
  defer(close(outgoing))

  # create our own socket server
  server <- renv_socket_server()
  defer(close(server$socket))

  # write message
  message <- list(method = method, data = data, port = server$port)
  serialize(message, connection = outgoing)

  # now, open a new connection to get the response
  incoming <- renv_socket_accept(server$socket, open = "rb")
  defer(close(incoming))

  # read the response
  unserialize(connection = incoming)

}

renv_watchdog_pid <- function() {
  the$watchdog_process$pid
}

renv_watchdog_port <- function() {
  the$watchdog_process$port
}

renv_watchdog_running <- function() {
  pid <- renv_watchdog_pid()
  !is.null(pid) && renv_process_exists(pid)
}

renv_watchdog_unload <- function() {
  renv_watchdog_shutdown()
}

renv_watchdog_terminate <- function() {
  pid <- renv_watchdog_pid()
  renv_process_kill(pid)
}

renv_watchdog_shutdown <- function() {

  # nothing to do if watchdog isn't running
  if (!renv_watchdog_running())
    return(TRUE)

  # tell watchdog to shutdown
  renv_watchdog_notify("Shutdown")

  # wait for process to exit (avoid RStudio bomb)
  clock <- timer()
  wait_until(function() {
    !renv_watchdog_running() || clock$elapsed() > 1
  })

  if (!renv_watchdog_running())
    return(TRUE)

  # if it's still running, explicitly terminate it
  renv_watchdog_terminate()

  # wait for process to exit (avoid RStudio bomb)
  clock <- timer()
  wait_until(function() {
    !renv_watchdog_running() || clock$elapsed() > 1
  })

}


# xcode.R --------------------------------------------------------------------


renv_xcode_available <- function() {

  # allow bypass if required
  check <- getOption("renv.xcode.available", default = NULL)
  if (!is.null(check))
    return(check)

  # otherwise, check via xcode-select
  status <- suppressWarnings(
    system2("/usr/bin/xcode-select", "-p", stdout = FALSE, stderr = FALSE)
  )

  identical(status, 0L)

}

renv_xcode_check <- function() {

  # allow bypass of xcode check if required
  check <- getOption("renv.xcode.check", default = TRUE)
  if (identical(check, FALSE))
    return()

  # only run on macOS
  if (!renv_platform_macos())
    return()

  # only run check once per session
  if (once())
    return()

  cmd <- "/usr/bin/xcrun --find --show-sdk-path"
  status <- system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  if (identical(status, 0L))
    return()

  if (identical(status, 69L)) {

    msg <- "
macOS is reporting that you have not yet agreed to the Xcode license.
You must accept the Xcode license before R packages can be installed from source.
Please run:

    sudo xcodebuild -license accept

in the Terminal to accept the Xcode license.
Set options(renv.xcode.check = FALSE) to disable this warning.
"
    warning(msg)

  }

  fmt <- "%s returned exit code %i"
  warningf(fmt, cmd, status)

}


# yaml.R ---------------------------------------------------------------------


renv_yaml_load <- function(text) {

  yaml::yaml.load(
    string = text,
    eval.expr = FALSE,
    handlers = list(
      r = function(yaml) {
        attr(yaml, "type") <- "r"
        yaml
      }
    )
  )

}


# zzz.R ----------------------------------------------------------------------


.onLoad <- function(libname, pkgname) {
  renv_zzz_load()
}

.onAttach <- function(libname, pkgname) {
  renv_zzz_attach()
}

.onUnload <- function(libpath) {

  renv_lock_unload()
  renv_task_unload()
  renv_watchdog_unload()

  # flush the help db to avoid errors on reload
  # https://github.com/rstudio/renv/issues/1294
  helpdb <- system.file(package = "renv", "help/renv.rdb")
  .Internal <- .Internal
  lazyLoadDBflush <- function(...) {}

  tryCatch(
    .Internal(lazyLoadDBflush(helpdb)),
    error = function(e) NULL
  )

}

# NOTE: required for devtools::load_all()
.onDetach <- function(libpath) {
  package <- Sys.getenv("DEVTOOLS_LOAD", unset = NA)
  if (identical(package, .packageName))
    .onUnload(libpath)
}

renv_zzz_load <- function() {

  # NOTE: needs to be visible to embedded instances of renv as well
  the$envir_self <<- renv_envir_self()

  renv_metadata_init()
  renv_platform_init()
  renv_virtualization_init()
  renv_envvars_init()
  renv_log_init()
  renv_methods_init()
  renv_libpaths_init()
  renv_patch_init()
  renv_sandbox_init()
  renv_sdkroot_init()
  renv_watchdog_init()

  if (!renv_metadata_embedded()) {

    # TODO: It's not clear if these callbacks are safe to use when renv is
    # embedded, but it's unlikely that clients would want them anyhow.
    renv_task_create(renv_sandbox_task)
    renv_task_create(renv_snapshot_task)
  }

  # if an renv project already appears to be loaded, then re-activate
  # the sandbox now -- this is primarily done to support suspend and
  # resume with RStudio where the user profile might not be run
  if (renv_rstudio_available()) {
    project <- getOption("renv.project.path")
    if (!is.null(project))
      renv_sandbox_activate(project = project)
  }

  # make sure renv is unloaded on exit, so locks etc. are released
  # we previously tried to orchestrate this via unloadNamespace(),
  # but this fails when a package importing renv is already loaded
  # https://github.com/rstudio/renv/issues/1621
  reg.finalizer(renv_envir_self(), renv_unload_finalizer, onexit = TRUE)

}

renv_zzz_attach <- function() {
  renv_rstudio_fixup()
}

renv_zzz_run <- function() {

  # check if we're in pkgload::load_all()
  # if so, then create some files
  if (renv_envvar_exists("DEVTOOLS_LOAD")) {
    renv_zzz_bootstrap_activate()
    renv_zzz_bootstrap_config()
  }

  # check if we're running as part of R CMD build
  # if so, build our local repository with a copy of ourselves
  if (building())
    renv_zzz_repos()

}

renv_zzz_bootstrap_activate <- function() {

  source <- "templates/template-activate.R"
  target <- "inst/resources/activate.R"
  scripts <- c("R/bootstrap.R", "R/json-read.R")

  # Do we need an update
  source_mtime <- max(renv_file_info(c(source, scripts))$mtime)
  target_mtime <- renv_file_info(target)$mtime

  if (!is.na(target_mtime) && target_mtime > source_mtime)
    return()

  # read the necessary bootstrap scripts
  contents <- map(scripts, readLines)
  bootstrap <- unlist(contents)

  # format nicely for insertion
  bootstrap <- paste(" ", bootstrap)
  bootstrap <- paste(bootstrap, collapse = "\n")

  # replace template with bootstrap code
  template <- renv_file_read(source)
  replaced <- renv_template_replace(template, list(BOOTSTRAP = bootstrap))

  # write to resources
  printf("- Generating 'inst/resources/activate.R' ... ")
  writeLines(replaced, con = target)
  writef("Done!")

}

renv_zzz_bootstrap_config <- function() {

  source <- "inst/config.yml"
  target <- "R/config-defaults.R"

  source_mtime <- renv_file_info(source)$mtime
  target_mtime <- renv_file_info(target)$mtime

  if (target_mtime > source_mtime)
    return()

  template <- renv_template_create(heredoc(leave = 2, '
    ${NAME} = function(..., default = ${DEFAULT}) {
      renv_config_get(
        name    = "${NAME}",
        type    = "${TYPE}",
        default = default,
        args    = list(...)
      )
    }
  '))

  template <- gsub("^\\n+|\\n+$", "", template)

  generate <- function(entry) {

    name    <- entry$name
    type    <- entry$type
    default <- entry$default
    code    <- entry$code

    default <- if (length(code)) trimws(code) else deparse(default)

    replacements <- list(
      NAME     = name,
      TYPE     = type,
      DEFAULT  = default
    )

    renv_template_replace(template, replacements)

  }

  config <- yaml::read_yaml("inst/config.yml")
  code <- map_chr(config, generate)
  all <- c(
    "",
    "# Auto-generated by renv_zzz_bootstrap_config()",
    "",
    "#' @rdname config",
    "#' @export",
    "#' @format NULL",
    "config <- list(",
    "",
    paste(code, collapse = ",\n\n"),
    "",
    ")"
  )

  printf("- Generating 'R/config-defaults.R' ... ")
  writeLines(all, con = target)
  writef("Done!")

}

renv_zzz_repos <- function() {

  # don't run if we're running tests
  if (renv_package_checking())
    return()

  # prevent recursion
  installing <- Sys.getenv("RENV_INSTALLING_REPOS", unset = NA)
  if (!is.na(installing))
    return()

  renv_scope_envvars(RENV_INSTALLING_REPOS = "TRUE")
  writeLines("** installing renv to package-local repository")

  # get package directory
  pkgdir <- getwd()

  # move to build directory
  tdir <- tempfile("renv-build-")
  ensure_directory(tdir)
  renv_scope_wd(tdir)

  # build renv again
  r_cmd_build("renv", path = pkgdir, "--no-build-vignettes")

  # copy built tarball to inst folder
  src <- list.files(tdir, full.names = TRUE)
  tgt <- file.path(pkgdir, "inst/repos/src/contrib")

  ensure_directory(tgt)
  file.copy(src, tgt)

  # write PACKAGES
  renv_scope_envvars(R_DEFAULT_SERIALIZE_VERSION = "2")
  write_PACKAGES(tgt, type = "source")

}

if (identical(.packageName, "renv")) {
  renv_zzz_run()
}


