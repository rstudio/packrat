# Hooks for library modifying functions that can be used to auto-snapshot
# and also maintain library state consistency when within packrat mode
snapshotHook <- function(expr, value, ok, visible) {
  tryCatch(

    expr = {
      suppressMessages(
        snapshot(projDir = .packrat$projectDir, orphan.check = FALSE)
      )
    },

    error = function(e) {
      message("ERROR: Automatic snapshotting failed!")
      stop(e)
    }

  )
  invisible(TRUE)
}
