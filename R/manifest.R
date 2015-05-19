readManifest <- function(project = NULL) {
  path <- manifestFilePath(project)
  if (!file.exists(path))
    return(list())

  content <- read(path)
  splat <- strsplit(content, "\n{2,}", perl = TRUE)
  lapply(splat, readManifestEntry)
}

# Manifest entries are just DESCRIPTION files
readManifestEntry <- function(entry) {
  read.dcf(textConnection(entry))
}

writeManifest <- function(manifestPath, pkgRecords) {

  ip <- installed.packages()
  missingPkgs <- c()

  content <- vapply(pkgRecords, FUN.VALUE = character(1), USE.NAMES = FALSE, function(record) {
    name <- record$name

    if (!(name %in% rownames(ip))) {
      missingPkgs <- c(missingPkgs, name)
      next
    }

    libPath <- ip[name, "LibPath"]
    descPath <- file.path(libPath, name, "DESCRIPTION")
    if (!file.exists(descPath))
      stop("No DESCRIPTION file for package '", name, "'")

    read(descPath)
  })

  cat(content, file = manifestPath, sep = "\n")
}
