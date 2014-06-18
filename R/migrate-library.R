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

  shouldRun <- checkDirtySystemLibrary() && is.windows()
  if (!shouldRun) {
    return(invisible(FALSE))
  } else {
    message(paste0(
      "WARNING: It appears you have non-system packages installed in your system library.\n",
      "Packrat relies on only 'base' and 'recommended' R packages being installed ",
      "in the system library."
    ))
    userLib <- unlist(strsplit(Sys.getenv("R_LIBS_USER"),
                               .Platform$path.sep))[1L]
    ans <- readline(
      paste0("Would you like to create a personal library at ", shQuote(userLib), "? [Y/n]: ")
    )

    if (tolower(substring(ans, 1, 1)) == "y") {
      dir.create(userLib, recursive = TRUE)
      .libPaths(c(userLib, .libPaths()))
    }

  }

}
