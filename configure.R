#!/usr/bin/env Rscript

library(utils)

DESCRIPTION <- readLines("DESCRIPTION")
idx <- grep("^Version:", DESCRIPTION)
if (!grepl(".*-", DESCRIPTION[[idx]])) {
  DESCRIPTION[[idx]] <- paste(DESCRIPTION[[idx]], "-1", sep = "")
} else {
  minorVersion <- as.numeric(gsub(".*-", "", DESCRIPTION[[idx]]))
  minorVersion <- minorVersion + 1
  DESCRIPTION[[idx]] <- gsub("-.*", paste("-", minorVersion, sep = ""), DESCRIPTION[[idx]])
}
cat(DESCRIPTION, file = "DESCRIPTION", sep = "\n")

source("R/update.R")
updateInit()
