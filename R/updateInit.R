updateInit <- function() {
  init.R <- readLines(file.path("inst", "init.R"))
  packrat.version <- read.dcf("DESCRIPTION")[1, "Version"]
  init.R[1] <- paste(sep = "",
                     "#### -- Packrat Autoloader (version ", packrat.version, ") -- ####")
  init.R[length(init.R)] <- "#### -- End Packrat Autoloader -- ####"

  ## Sync the packrat path, messages
  source("R/aaa-globals.R")

  ## Update library path
  libraryPathMarker <- "## -- packrat::library_path -- ##"
  filePathArgs <- c(
    shQuote(.packrat$packratFolderName),
    shQuote("lib"),
    "R.version$platform",
    "getRversion()"
  )
  filePathCmd <- paste(sep = "",
                       "file.path(", paste(filePathArgs, collapse = ", "), ")")
  libPathLine <- grep(paste(sep = "", libraryPathMarker, "$"),
                      init.R)
  libPathCmd <- paste(sep = "",
                      "libDir <- ", filePathCmd, " ", libraryPathMarker)
  init.R[libPathLine] <- libPathCmd

  ## Update bootstrap path
  bootstrapPathMarker <- "## -- packrat::bootstrap_path -- ##"
  bootstrapPath <- file.path(.packrat$packratFolderName, "bootstrap.R")
  fileCheckLine <- grep( paste(sep = "", bootstrapPathMarker, "$"),
                         init.R)
  checkCmd <- paste(sep = "",
                    "  if (file.exists(", shQuote(bootstrapPath), ")) ",
                    bootstrapPathMarker)
  init.R[fileCheckLine] <- checkCmd

  ## Update bootstrap message
  bootstrapMsgMarker <- "## -- packrat::bootstrap_message -- ##"
  msgLine <- grep(paste(sep = "", bootstrapMsgMarker, "$"),
                  init.R)
  init.R[msgLine] <- paste(sep = "",
                           "    message('Run \\\'source(\"",
                           bootstrapPath,
                           "\")\\\' to bootstrap a packrat installation.\') ",
                           bootstrapMsgMarker)

  cat(init.R, file=file.path("inst", "init.R"), sep = "\n")
}
