readManifest <- function(project = NULL) {
  path <- manifestFilePath(project)
  if (!file.exists(path))
    return(list())

  content <- read(path)
  splat <- strsplit(content, "\n{2,}", perl = TRUE)[[1]]
  lapply(splat, readManifestEntry)
}

# Manifest entries are just DESCRIPTION files
readManifestEntry <- function(entry) {
  readDcf(textConnection(entry), all = TRUE)
}

writeManifest <- function(project, pkgRecords) {
  project <- getProjectDir(NULL)
  manifestPath <- manifestFilePath(project)
  ip <- installed.packages(lib.loc = libDir(project))

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

  if (length(missingPkgs)) {
    stop("The DESCRIPTION files associated with the following packages ",
         "could not be found:\n- ",
         paste(surround(missingPkgs, with = '"'), collapse = ", "))
  }

  cat(content, file = manifestPath, sep = "\n")
}
