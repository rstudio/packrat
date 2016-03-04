bundle_test <- function(bundler, checker, ...) {

  # set and restore directory
  owd <- setwd(tempdir())
  on.exit(setwd(owd), add = TRUE)

  # create temporary directory
  dir <- file.path(tempdir(), "packrat-test-bundle")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  # enter, bundle and untar
  setwd("packrat-test-bundle")
  suppressWarnings(packrat::init(enter = FALSE))
  bundler(file = "test-bundle.tar.gz", ...)
  untar("test-bundle.tar.gz", exdir = "untarred", tar = "internal")

  # run checker
  checker()

}

