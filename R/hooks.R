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
      cat(e, file = file.path(.packrat$projectDir, "packrat.log"), append = TRUE)
    }

  )
  invisible(TRUE)
}
