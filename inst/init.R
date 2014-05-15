if (suppressWarnings(require("packrat", quietly = TRUE))) {
  packrat::packrat_mode()
} else {
  message("error: packrat not installed; cannot enter packrat mode")
  if (file.exists(".packrat/bootstrap.R")) {
    message("Run 'source(\".packrat/bootstrap.R\")' to bootstrap a packrat installation.")
  }
}
