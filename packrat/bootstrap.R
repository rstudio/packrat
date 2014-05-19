## This file is used to bootstrap a packrat package from an empty R installation
packratSrcPath <- list.files(".packrat/src/packrat", full.names = TRUE)[1]
tmpLib <- file.path(tempdir(), "packrat", "library")
dir.create(tmpLib, recursive = TRUE)

message("> Installing packrat to temporary library in directory:")
message("> ", tmpLib)
install.packages(packratSrcPath, lib = tmpLib, repos = NULL)

message("> Attaching packrat")
library("packrat", lib.loc = tmpLib)

message("Packrat successfully installed. Run 'packrat_mode()' to enter packrat mode.")
