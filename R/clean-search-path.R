## Clean up the search path -- unload all packages in the user library
## Primarily used when entering packrat mode
cleanSearchPath <- function(verbose = TRUE, lib.loc = getUserLibPaths()) {

  searchPath <- search_path()

  ## Don't remove anything in a packrat private library
  toCheck <- grep("packrat", searchPath$lib.dir, invert = TRUE)
  if (!length(toCheck)) return(NULL)

  searchPath <- searchPath[toCheck, ]

  searchPath$path <- paste("package", searchPath$package, sep = ":")
  ip <- utils::installed.packages(lib.loc = lib.loc)
  searchPath <- searchPath[searchPath$package %in% rownames(ip), ]

  ## Don't unload base, recommended packages
  userPkgs <- searchPath$package[is.na(ip[searchPath$package, "Priority"])]
  searchPathToUnload <- searchPath[searchPath$package %in% userPkgs, ]

  if (verbose && nrow(searchPathToUnload)) {
    message("Unloading packages in user library:\n- ", paste(searchPathToUnload$package, collapse = ", "))
  }

  for (path in searchPathToUnload$path) {
    forceUnload(path)
  }

  searchPathToUnload
}
