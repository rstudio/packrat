userPkgsInSystemLibrary <- function() {

  systemPkgs <- as.data.frame(
    stringsAsFactors = FALSE,
    utils::installed.packages(utils::tail(.libPaths(), 1))
  )
  userLibs <- with(systemPkgs, {
    Package[is.na(Priority)]
  })

  ## Translations is a system package installed with CRAN R
  ## that we should exclude -- it is not given a priority,
  ## strangely enough
  userLibs <- setdiff(userLibs, "translations")

  userLibs
}

checkDirtySystemLibrary <- function() {
  userPkgs <- userPkgsInSystemLibrary()
  length(userPkgs) > 0
}

checkNeedsLibraryMigration <- function() {

  ## We'll silently create the user library if necessary
  userLib <- userLib()
  if (!file.exists(userLib))
    dir.create(userLib, recursive = TRUE)

  shouldRun <- checkDirtySystemLibrary() && is.windows()
  if (!shouldRun) {
    return(invisible(character()))
  } else {
    pkgsToMigrate <- userPkgsInSystemLibrary()
    message(paste0(
      "WARNING: It appears you have non-system packages installed in your system library.\n",
      "Packrat relies on only 'base' and 'recommended' R packages being installed ",
      "in the system library.\n\nPlease delete and reinstall the following packages:\n- ",
      paste(pkgsToMigrate, collapse = ", ")
    ))

    if (!file.exists(userLib)) {
      dir.create(userLib, recursive = TRUE)
    }

    # message("You can perform this action by calling 'packrat::migrate_windows()'.")
    pkgsToMigrate
  }

}

migrate_packages <- function() {

  ## Exit packrat mode if it's on
  if (isPackratModeOn())
    packrat::off(print.banner = FALSE)

  ## Clean up the search path and unload everything possible
  cleanSearchPath()
  pkgsToMigrate <- userPkgsInSystemLibrary()

  if (!length(pkgsToMigrate)) {
    message("No migration is necessary.")
    return(invisible(NULL))
  } else {
    if (interactive()) {
      wrapped <- paste(strwrap(paste(shQuote(pkgsToMigrate), collapse = ", "), 78), collapse = "\n")
      message("The following packages will be migrated:\n- ",
              wrapped)
      response <- readline("Do you want to continue? [Y/n]: ")
      if (tolower(substring(response, 1, 1)) != "y") {
        message("Migration aborted.")
        return(invisible(pkgsToMigrate))
      }
    }
  }

  ## Set up the library
  systemLib <- normalizePath(.Library, winslash = "/", mustWork = TRUE)
  userLib <- userLib()
  if (!file.exists(userLib))
    dir.create(userLib, recursive = TRUE)
  .libPaths(userLib)

  descFiles <- file.path(systemLib, pkgsToMigrate, "DESCRIPTION")
  descContent <- lapply(descFiles, readDcf)
  names(descContent) <- pkgsToMigrate
  githubPkgs <- pkgsToMigrate[sapply(descContent, function(x) {
    "GithubRepo" %in% colnames(x)
  })]
  cranPkgs <- pkgsToMigrate[sapply(descContent, function(x) {
    "Repository" %in% colnames(x) && x[, "Repository"] == "CRAN"
  })]
  biocPkgs <- pkgsToMigrate[sapply(descContent, function(x) {
    "biocViews" %in% colnames(x)
  })]

  unknownPkgs <- setdiff(pkgsToMigrate, c(githubPkgs, cranPkgs, biocPkgs))

  # Ignore RStudio packages
  unknownPkgs <- setdiff(unknownPkgs, c("manipulate", "rstudio"))

  ## Install packages from GitHub
  if (length(githubPkgs)) {
    message("> Installing GitHub packages")
    if (!requireNamespace("devtools")) {
      install.packages("devtools", lib = userLib())
    }
    for (pkg in githubPkgs) {
      desc <- as.data.frame(descContent[[pkg]], stringsAsFactors = FALSE)
      ref <- desc$GithubSHA1 %||% desc$GithubRef %||% "master"
      devtools::install_github(repo = desc$GithubRepo,
                               username = desc$GithubUsername,
                               ref = ref,
                               quick = TRUE
      )
    }
  }

  ## Install packages from BioC
  if (length(biocPkgs)) {
    message("> Installing BioC packages")
    try(unloadNamespace("BiocInstaller"), silent = TRUE)
    source("http://bioconductor.org/biocLite.R")
    BiocInstaller::biocLite(biocPkgs, lib.loc = userLib)
  }

  ## Install packages from CRAN
  if (length(cranPkgs)) {
    message("> Installing CRAN packages")
    ## Check that we're not reinstalling packages that might have been added as dependencies
    alreadyInstalled <- list.files(userLib())
    needsInstall <- cranPkgs[!(cranPkgs %in% alreadyInstalled)]

    ## Unload namespaces so we can install
    try(lapply(needsInstall, forceUnload), silent = TRUE)

    ## Vectorized so install.packages can infer an installation order
    install.packages(needsInstall, lib = userLib)
  }

  ## Let the user know about what's been migrated, what's not
  successes <- list.files(userLib())
  failures <- setdiff(pkgsToMigrate, successes)

  result <- list(
    cran = cranPkgs,
    bioc = biocPkgs,
    github = githubPkgs,
    missing = setdiff(failures, c("rstudio", "manipulate"))
  )

  if (length(successes)) {
    message("The following packages were successfully migrated:\n- ",
            paste(shQuote(successes), collapse = ", "),
            "\n\n")
  }

  if (length(failures)) {
    message("The following packages were NOT successfully migrated and will ",
            "need to be reinstalled manually:\n- ",
            paste(shQuote(failures), collapse = ", "),
            "\n\n")
  }

  ## Clean out the system library
  if (interactive()) {
    prettyPkgs <- paste(collapse = "\n",
      strwrap(paste(shQuote(pkgsToMigrate), collapse = ", "), 78)
    )
    message("The following packages will be removed from the system library:")
    message(prettyPkgs)
    response <- readline("Do you want to continue? [Y/n]: ")
    if (tolower(substring(response, 1, 1)) == "y") {
      remove.packages(pkgsToMigrate, lib = .Library)
      message("System library successfully cleaned of user packages.")
    } else {
      message("System library cleanup aborted.")
      return(result)
    }
  } else {
    remove.packages(pkgsToMigrate, lib = .Library)
  }

  message("Library migration complete!")
  result

}

migrate_windows <- migrate_packages

checkNeedsPackratMigration <- function(project = NULL) {

  # These files should no longer exist in the base project directory
  if (file.exists("packrat.sources") ||
      file.exists("packrat.lock")) {

    message("WARNING: This project needs to be migrated to the latest ",
            "version of packrat.")
    response <- readline("Would you like to migrate? [Y/n]: ")
    if (tolower(substring(response, 1, 1)) == "y") {
      migrate()
    } else {
      message("Migration process aborted -- please restore an old version of ",
              "packrat to continue.")
    }

  }

}

checkNeedsMigration <- function() {
  checkNeedsLibraryMigration()
  checkNeedsPackratMigration()
}
