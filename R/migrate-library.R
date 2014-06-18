userPkgsInSystemLibrary <- function() {

  systemPkgs <- as.data.frame(
    stringsAsFactors = FALSE,
    utils::installed.packages(utils::tail(.libPaths(), 1))
  )
  userLibs <- with(systemPkgs, {
    Package[is.na(Priority)]
  })
  userLibs
}

checkDirtySystemLibrary <- function() {
  userPkgs <- userPkgsInSystemLibrary()
  length(userPkgs) > 0
}

checkNeedsLibraryMigration <- function() {

  ## We'll silently create the user library if necessary
  if (!file.exists(userLib()))
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
    userLib <- unlist(strsplit(Sys.getenv("R_LIBS_USER"),
                               .Platform$path.sep))[1L]

    if (!file.exists(userLib)) {
      dir.create(userLib, recursive = TRUE)
    }

    message("You can perform this action by calling 'packrat::migrate_windows()'.")
    pkgsToMigrate
  }

}

migrate_windows <- function() {

  pkgsToMigrate <- userPkgsInSystemLibrary()

  if (!length(pkgsToMigrate)) {
    message("No migration is necessary.")
    return(invisible(NULL))
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
  if (length(unknownPkgs)) {
    warning("unable to determine source for the following packages; they will need to be reinstalled manually:\n- ",
            paste(shQuote(unknownPkgs), collapse = ", "))
  }

  ## Install packages from GitHub
  if (length(githubPkgs)) {
    if (!require("devtools")) {
      install.packages("devtools", lib = userLib())
    }
    for (pkg in githubPkgs) {
      desc <- as.data.frame(descContent[[pkg]], stringsAsFactors = FALSE)
      ref <- desc$GithubSHA1 %||% desc$GithubRef %||% "master"
      devtools::install_github(repo = desc$GithubRepo,
                               username = desc$GithubUsername,
                               ref = ref,
      )
    }
  }

  ## Install packages from BioC
  if (length(biocPkgs)) {
    if (!require("BiocInstaller")) {
      source("http://bioconductor.org/biocLite.R\")' to install BiocLite")
    }
    biocLite(biocPkgs, lib.loc = userLib)
  }

  ## Install packages from CRAN
  if (length(cranPkgs)) {
    ## Check that we're not reinstalling packages that might have been added as dependencies
    alreadyInstalled <- list.files(userLib())
    needsInstall <- cranPkgs[!(cranPkgs %in% alreadyInstalled)]
    install.packages(needsInstall, lib = userLib)
  }

  ## Clean out the library
  remove.packages(pkgsToMigrate, lib = .Library)

  message("Library migration complete.")
  notInstalled <- setdiff(pkgsToMigrate, list.files(userLib()))
  if (length(notInstalled)) {
    message("The following packages could not be installed (try restarting your R session and reinstalling them):\n- ",
            paste(shQuote(notInstalled, collapse = ", ")))
  }

  invisible(list(
    cran = cranPkgs,
    bioc = biocPkgs,
    github = githubPkgs,
    missing = notInstalled
  ))
}
