#!/usr/bin/env Rscript

library(utils)

DESCRIPTION <- readLines("DESCRIPTION")
idx <- grep("^Version:", DESCRIPTION)
minorVersion <- as.numeric(gsub(".*-", "", DESCRIPTION[[idx]]))
minorVersion <- minorVersion + 1
DESCRIPTION[[idx]] <- gsub("-.*", paste("-", minorVersion, sep = ""), DESCRIPTION[[idx]])
cat(DESCRIPTION, file = "DESCRIPTION", sep = "\n")

source("R/update.R")
updateInit()
