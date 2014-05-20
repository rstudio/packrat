#### -- Packrat Autoloader (version 0.1.0.99) -- ####
if (suppressWarnings(require("packrat", quietly = TRUE))) {
  packrat:::checkPackified()
  if (!packrat:::packratModeOn()) {
    packrat::packrat_mode()
  }
} else {
  message("error: packrat not installed; cannot enter packrat mode")
  if (file.exists('packrat/bootstrap.R')) ## -- packrat::bootstrap_path -- ##
    message('Run \'source("packrat/bootstrap.R")\' to bootstrap a packrat installation.') ## -- packrat::bootstrap_message -- ##
}
#### -- End Packrat Autoloader -- ####
