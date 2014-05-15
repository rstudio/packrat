# Hooks for library modifying functions that can be used to auto-snapshot
# and also maintain library state consistency when within packrat mode
snapshotHook <- function(expr, value, ok, visible) {
  tryCatch(

    expr = {
      silent(
        snapshot(projDir = .packrat$projectDir, orphan.check = FALSE)
      )
    },

    error = function(e) {
      message("NOTE: Automatic snapshotting failed. Automatic snapshotting is now disabled. Reason:\n")
      message(e)
      message("\nConsider submitting a bug report at 'https://github.com/rstudio/packrat/issues'.\n")
    }

  )
  invisible(TRUE)
}
