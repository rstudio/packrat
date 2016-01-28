#!/usr/bin/env Rscript

library(utils)

args <- commandArgs(TRUE)

# Extract the Version field from the DESCRIPTION.
DESCRIPTION <- readLines("DESCRIPTION")
idx <- grep("^Version:", DESCRIPTION)
field <- DESCRIPTION[[idx]]

# Figure out what the minor-patch bumped version is.
bumpedDescVersion <- if (!grepl("-", DESCRIPTION[[idx]])) {
  paste(field, "-1", sep = "")
} else {
  minorVersion <- as.numeric(gsub(".*-", "", DESCRIPTION[[idx]]))
  minorVersion <- minorVersion + 1
  gsub("-.*", paste("-", minorVersion, sep = ""), DESCRIPTION[[idx]])
}

# Set the version field (use command line args if appropriate)
version <- if (length(args)) {
  paste("Version:", args[[1]])
} else {
  bumpedDescVersion
}

DESCRIPTION[[idx]] <- version
cat(DESCRIPTION, file = "DESCRIPTION", sep = "\n")

source("R/update.R")
updateInit()
