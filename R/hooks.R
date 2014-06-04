# Hooks for library modifying functions that can be used to auto.snapshot
# and also maintain library state consistency when within packrat mode
snapshotHook <- function(expr, value, ok, visible) {

  tryCatch(
    expr = {

      project <- getProjectDir()
      packratDir <- getPackratDir(project)

      ## A snapshot lock file that we should check to ensure we don't try to
      ## snapshot multiple times
      snapshotLockPath <- file.path(packratDir, "snapshot.lock")

      ## This file needs to be checked, and deleted, by the async process
      if (file.exists(snapshotLockPath)) {
        return(FALSE)
      }
      file.create(snapshotLockPath)

      peq <- function(x, y) paste(x, y, sep = " = ")
      snapshotArgs <- paste(sep = ", ",
                            peq("project", shQuote(project)),
                            peq("orphan.check", "FALSE"),
                            peq("auto.snapshot", "TRUE"),
                            peq("verbose", "FALSE")
      )
      setwdCmd <- paste("setwd(", shQuote(project), ")")
      snapshotCmd <- paste("try(suppressMessages(packrat:::snapshotImpl(", snapshotArgs, ")))")
      cleanupCmd <- paste("file.remove(", shQuote(snapshotLockPath), ")")
      setLibsCmd <- paste(".libPaths( c(", paste(shQuote(getLibPaths()), collapse = ", "), ") )")
      fullCmd <- paste(sep = "; ",
                       setwdCmd,
                       setLibsCmd,
                       snapshotCmd,
                       cleanupCmd,
                       "invisible()"
      )

      r_path <- file.path(R.home("bin"), "R")

      cmd <- paste(shQuote(r_path), "--vanilla", "--slave", "-e", shQuote(fullCmd))
      res <- system(cmd, wait = FALSE, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
      invisible(TRUE)

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
