updateInit <- function() {
  init.R <- readLines("inst/init.R")
  packrat.version <- readDcf("DESCRIPTION")[1, "Version"]
  init.R[1] <- paste(sep = "",
                     "#### -- Packrat Autoloader (version ", packrat.version, ") -- ####")
  init.R[length(init.R)] <- "#### -- End Packrat Autoloader -- ####"
  cat(init.R, file="inst/init.R", sep = "\n")
}
