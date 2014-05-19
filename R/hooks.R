# Hooks for library modifying functions that can be used to auto-snapshot
# and also maintain library state consistency when within packrat mode
snapshotHook <- function(expr, value, ok, visible) {
  tryCatch(

    expr = {
      silent(
        snapshot(projDir = .packrat$projectDir, orphan.check = FALSE)
      )
    },

    # Cases where an automatic snapshot can fail:
    #
    # 1. A library is deleted, e.g. with remove.packages.
    # TODO: How should we handle an automatic snapshot fail?
    error = function(e) {
      if (is.null(.packrat$projectDir)) {
        file = "" ## to stdout
      } else {
        file = file.path(.packrat$projectDir, .packrat$packratFolderName, "packrat.log")
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
  invisible(TRUE)
}
