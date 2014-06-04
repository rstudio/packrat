userPkgsInSystemLibrary <- function() {

  systemPkgs <- as.data.frame( stringsAsFactors = FALSE,
                               utils::installed.packages(utils::tail(getLibPaths(), 1))
  )
  userLibs <- with(systemPkgs, {
    Package[is.na(Priority)]
  })
  userLibs
}

checkDirtySystemLibrary <- function() {
  userPkgs <- userPkgsInSystemLibrary()
  length(userPkgs) > 0
}

checkNeedsLibraryMigration <- function() {

  shouldRun <- checkDirtySystemLibrary()
  if (!shouldRun) {
    return(invisible(FALSE))
  } else {
    message(paste( sep = "",
                   "WARNING: It appears you have user libraries installed in your system library.\n",
                   "Packrat relies on only 'base' and 'recommended' R packages being installed ",
                   "in the system library."))
    switch (Sys.info()["sysname"],
            "Darwin"=macLibraryMigration(),
            "Windows"=windowsLibraryMigration(), ## TODO: windows-specific migration?
            defaultLibraryMigration()
    )
  }

}

## TODO -- how should we instruct the user to migrate in the default case?
defaultLibraryMigration <- function() {
  message(paste(sep = "",
                "Please migrate the following packages to a user library:\n- ",
                paste(userPkgsInSystemLibrary(), collapse = ", ")
  ))
}

## TODO -- migrate on Windows?
windowsLibraryMigration <- function() {
  defaultLibraryMigration()
}

macLibraryMigration <- function() {
  scriptCall <- paste("bash", instMacRUserlibFilePath())
  message(paste(sep = "",
                "Please run the following command at a terminal to migrate your user libraries to a user library folder:\n",
                "    ", scriptCall))
}
