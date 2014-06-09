updateBootstrap <- function() {
  bootstrap.R <- readLines(file.path("inst", "resources", "bootstrap.R"))
  packrat.version <- read.dcf("DESCRIPTION")[1, "Version"]

  ## Sync the packrat path, messages
  source("R/aaa-globals.R")

  installAgentLine <- grep("## -- InstallAgent -- ##", bootstrap.R)
  bootstrap.R[installAgentLine + 1] <- paste("  installAgent <-", shQuote(paste("InstallAgent:", "packrat", packrat.version)))

  installSourceLine <- grep("## -- InstallSource -- ##", bootstrap.R)
  bootstrap.R[installSourceLine + 1] <- paste("  installSource <-", shQuote(paste("InstallSource:", "source")))

  cat(bootstrap.R, file = file.path("inst", "resources", "bootstrap.R"), sep = "\n")
}

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
  lockFile <- readLockFile(file = lockFilePath(project))
  repos <- character()
  repos["CRAN"] <- lockFile$repos
  options('repos' = repos)

  invisible(TRUE)

}
