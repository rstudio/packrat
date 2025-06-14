updateInit <- function() {

  # Update init.R (the file sourced from within the .Rprofile)
  init.R <- readLines(file.path("inst", "resources", "init.R"))
  packrat.version <- read.dcf("DESCRIPTION")[1, "Version"]

  installAgentLine <- grep("## -- InstallAgent -- ##", init.R)
  init.R[installAgentLine + 1] <- paste("    installAgent <-", shQuote(paste("InstallAgent:", "packrat", packrat.version), type = "cmd"))

  installSourceLine <- grep("## -- InstallSource -- ##", init.R)
  init.R[installSourceLine + 1] <- paste("    installSource <-", shQuote(paste("InstallSource:", "source"), type = "cmd"))

  cat(init.R, file = file.path("inst", "resources", "init.R"), sep = "\n")
}

# This function is used to update project settings, typically called after
# a call to packrat::set_opts
updateSettings <- function(project = NULL, options = NULL) {

  project <- getProjectDir(project)

  if (is.null(options)) {
    options <- get_opts(project = project)
  }

  # Make sure the packrat directory is ignored if we're in a package
  if (file.exists(file.path(project, "DESCRIPTION"))) {
    updateRBuildIgnore(project)
  }

  if (isGitProject(project)) {
    updateGitIgnore(project, options)
  }

  if (isSvnProject(project)) {
    updateSvnIgnore(project, options)
  }

  # Set the repositories
  if (file.exists(lockFilePath(project))) {
    lockFile <- readLockFile(file = lockFilePath(project))
    options('repos' = lockFile$repos)
  }

  # Update the external packages library
  symlinkExternalPackages(project = project)

  invisible(TRUE)

}
