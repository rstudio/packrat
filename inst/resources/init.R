local({

  libDir <- file.path('packrat', 'lib', R.version$platform, getRversion())

  if (suppressWarnings(requireNamespace("packrat", quietly = TRUE, lib.loc = libDir))) {
    # Check 'print.banner.on.startup' -- when NA and RStudio, don't print
    print.banner <- packrat::get_opts("print.banner.on.startup")
    if (print.banner == "auto" && is.na(Sys.getenv("RSTUDIO", unset = NA))) {
      print.banner <- TRUE
    } else {
      print.banner <- FALSE
    }
    return(packrat::on(print.banner = print.banner))
  }

  message("Packrat is not installed in the local library -- ",
    "attempting to bootstrap an installation...")

  ## We need utils for the following to succeed -- there are calls to functions
  ## in 'restore' that are contained within utils. utils gets loaded at the
  ## end of start-up anyhow, so this should be fine
  library("utils", character.only = TRUE)

  ## Install packrat into local project library
  packratSrcPath <- list.files(full.names = TRUE,
    file.path("packrat", "src", "packrat")
  )[1]

  if (!length(packratSrcPath)) {
    stop("Could not find a local packrat source tarball to install; cannot bootstrap packrat")
  }

  lib <- file.path("packrat", "lib", R.version$platform, getRversion())
  if (!file.exists(lib)) {
    dir.create(lib, recursive = TRUE)
  }
  lib <- normalizePath(lib, winslash = "/")

  message("> Installing packrat into project private library:")
  message("- ", shQuote(lib))

  ## The following is performed because a regular install.packages call can fail
  peq <- function(x, y) paste(x, y, sep = " = ")
  installArgs <- c(
    peq("pkgs", shQuote(packratSrcPath)),
    peq("lib", shQuote(lib)),
    peq("repos", "NULL"),
    peq("type", shQuote("source"))
  )
  installCmd <- paste(sep = "",
                      "utils::install.packages(",
                      paste(installArgs, collapse = ", "),
                      ")")

  fullCmd <- paste(
    shQuote(file.path(R.home("bin"), "R")),
    "--vanilla",
    "--slave",
    "-e",
    shQuote(installCmd)
  )
  system(fullCmd)

  ## Tag the installed packrat so we know it's managed by packrat

  ## -- InstallAgent -- ##
  installAgent <- 'InstallAgent: packrat 0.2.0.123'

  ## -- InstallSource -- ##
  installSource <- 'InstallSource: source'

  packratDescPath <- file.path(lib, "packrat", "DESCRIPTION")
  DESCRIPTION <- readLines(packratDescPath)
  DESCRIPTION <- c(DESCRIPTION, installAgent, installSource)
  cat(DESCRIPTION, file = packratDescPath, sep = "\n")

  # If the environment allows us to restart, do so with a call to restore
  restart <- getOption("restart")
  if (!is.null(restart)) {
    restart("packrat::restore(restart = FALSE)")
  }

  # Otherwise, continue on as normal
  message("> Attaching packrat")
  library("packrat", character.only = TRUE, lib.loc = lib)

  message("> Restoring library")
  restore(restart = FALSE)

  # Callers (source-erers) can define this hidden variable to make sure we don't enter packrat mode
  # Primarily useful for testing
  if (!exists(".__DONT_ENTER_PACKRAT_MODE__.")) {
    message("> Packrat bootstrap successfully completed. Entering packrat mode...")
    packrat::on()
  }

  Sys.unsetenv("R_PACKRAT_NEEDS_BOOTSTRAP")

})
