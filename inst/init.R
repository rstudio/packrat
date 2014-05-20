#### -- Packrat Autoloader (version 0.1.0.99) -- ####
if (suppressWarnings(require("packrat", quietly = TRUE))) {
  packrat:::checkPackified()
  packrat::packrat_mode()
} else {
  message("error: packrat not installed; cannot enter packrat mode")
  if (file.exists('packrat/bootstrap.R')) {
    message("Run 'source(\"packrat/bootstrap.R\")' to bootstrap a packrat installation.")
  }
}
#### -- End Packrat Autoloader -- ####
