# Hooks for library modifying functions that can be used to auto.snapshot
# and also maintain library state consistency when within packrat mode
snapshotHook <- function(expr, value, ok, visible) {

  tryCatch(

    expr = {
      snapshotHookImpl()
    },

    # Cases where an automatic snapshot can fail:
    #
    # 1. A library is deleted, e.g. with remove.packages.
    # TODO: How should we handle an automatic snapshot fail?
    error = function(e) {

      project <- .packrat_mutables$get("project")

      if (is.null(project)) {
        file = "" ## to stdout
      } else {
        file = file.path(project, .packrat$packratFolderName, "packrat.log")
      }

      if (inherits(e, "simpleError")) {
        msg <- e$message
      } else {
        msg <- e
      }

      if (identical(file, ""))
        message(paste("Error on automatic snapshot:", msg))
      else
        cat(msg, file = file, append = TRUE)
    }

  )

}

## Builds a call that can be executed asynchronously -- returned as a character
## vector that can be pasted with e.g. paste(call, collapse = "; ")
buildSnapshotHookCall <- function(project) {

  project <- getProjectDir()
  packratDir <- getPackratDir(project)
  snapshotLockPath <- file.path(packratDir, "snapshot.lock")

  ## utility paster
  peq <- function(x, y) paste(x, y, sep = " = ")

  snapshotArgs <- paste(sep = ", ",
                        peq("project", shQuote(project)),
                        peq("auto.snapshot", "TRUE"),
                        peq("verbose", "FALSE")
  )

  repos <- paste(deparse(getOption('repos'), width.cutoff = 500), collapse = ' ')

  setwdCmd <- paste0("setwd(", shQuote(project), ")")
  reposCmd <- paste0("options('repos' = ", repos, ")")
  setLibsCmd <- paste0(".libPaths(c(", paste(shQuote(getLibPaths()), collapse = ", "), "))")
  snapshotCmd <- paste0("try(suppressMessages(packrat:::snapshotImpl(", snapshotArgs, ")), silent = TRUE)")
  cleanupCmd <- paste0("file.remove(", shQuote(snapshotLockPath), ")")

  c(
    setwdCmd,
    reposCmd,
    setLibsCmd,
    snapshotCmd,
    cleanupCmd,
    "invisible()"
  )
}

snapshotHookImpl <- function() {
  project <- getProjectDir()
  packratDir <- getPackratDir(project)

  ## A snapshot lock file that we should check to ensure we don't try to
  ## snapshot multiple times
  snapshotLockPath <- file.path(packratDir, "snapshot.lock")

  ## This file needs to be checked, and deleted, by the async process
  if (file.exists(snapshotLockPath)) {
    ## we assume another process is currently performing an async snapshot
    return(FALSE)
  }

  fullCmd <- paste(buildSnapshotHookCall(project), collapse = "; ")
  file.create(snapshotLockPath)
  r_path <- file.path(R.home("bin"), "R")

  cmd <- paste(shQuote(r_path), "--vanilla", "--slave", "-e", shQuote(fullCmd))
  res <- system(cmd, wait = FALSE, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
  invisible(TRUE)
}
