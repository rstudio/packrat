
# ensure that a file ends with a single newline
normalizeDcf <- function(path) {
  n <- file.info(path)$size
  contents <- readChar(path, n, TRUE)
  replaced <- gsub("\n*$", "", contents)
  if (!identical(contents, replaced))
    cat(replaced, file = path, sep = "\n")
  path
}

appendToDcf <- function(path, records) {
  normalizeDcf(path)
  write_dcf(records, path, append = TRUE)
}

# Combines one or more comma-delimited fields from a data frame read from a
# DCF.
combineDcfFields <- function(dcfFrame, fields) {
  unique(unlist(lapply(fields, function(field) {
    gsub("\\s.*", "", unlist(
      strsplit(
        gsub("^\\s*", "", as.character(dcfFrame[[field]])), "\\s*,\\s*")))
  })))
}
