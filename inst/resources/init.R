#### -- Packrat Autoloader (version 0.2.0.99) -- ####
local({

  ## -- packrat::libDir -- ##
  libDir <- file.path('packrat', 'lib', R.version$platform, getRversion())

  if (suppressWarnings(require("packrat", quietly = TRUE, lib.loc = libDir))) {
    packrat:::checkPackified()
    packrat:::setPackratModeOn()
  } else {
    message("Error: packrat not installed; cannot enter packrat mode")

    ## -- packrat::bootstrapPath -- ##
    bootstrapPath <- 'packrat/bootstrap.R'

    if (file.exists(bootstrapPath)) {

      ## -- packrat::bootstrapMessage -- ##
      message('Run \'source("packrat/bootstrap.R")\' to bootstrap a packrat installation.')
    }
  }
})
#### -- End Packrat Autoloader -- ####
