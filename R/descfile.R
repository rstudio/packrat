
# check whether the specified file ends with newline
ends_with_newline <- function(path) {
  conn <- file(path, open = "rb", raw = TRUE)
  on.exit(close(conn))
  seek(conn, where = -1, origin = "end")
  lastByte <- readBin(conn, "raw", n = 1)
  lastByte == 0x0a
}

appendToDcf <- function(path, records) {
  if (!ends_with_newline(path))
    cat('\n', sep = '', file = path, append = TRUE)
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
