updateInit <- function() {
  init.R <- readLines(file.path("inst", "init.R"))
  packrat.version <- read.dcf("DESCRIPTION")[1, "Version"]
  init.R[1] <- paste(sep = "",
                     "#### -- Packrat Autoloader (version ", packrat.version, ") -- ####")
  init.R[length(init.R)] <- "#### -- End Packrat Autoloader -- ####"
  cat(init.R, file=file.path("inst", "init.R"), sep = "\n")
}
