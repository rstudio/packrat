updateInit <- function() {
  init.R <- readLines(file.path("inst", "resources", "init.R"))
  packrat.version <- read.dcf("DESCRIPTION")[1, "Version"]

  ## Sync the packrat path, messages
  source("R/aaa-globals.R")

  installAgentLine <- grep("## -- InstallAgent -- ##", init.R)
  init.R[installAgentLine + 1] <- paste("  installAgent <-", shQuote(paste("InstallAgent:", "packrat", packrat.version)))

  installSourceLine <- grep("## -- InstallSource -- ##", init.R)
  init.R[installSourceLine + 1] <- paste("  installSource <-", shQuote(paste("InstallSource:", "source")))

  cat(init.R, file = file.path("inst", "resources", "init.R"), sep = "\n")
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

  ## TODO -- this will change soon, but for now we just set the first repo (should be CRAN anyhow)
  repos["CRAN"] <- lockFile$repos[[1]]
  options('repos' = repos)

  invisible(TRUE)

}
