## Write a .Renviron -- this ensures that packages in the site-library do not get attached to the .libPath()
augmentRenviron <- function(projDir = NULL) {

  projDir <- getProjectDir(projDir)

  path <- file.path(projDir, ".Renviron")
  if (!file.exists(path)) {
    file.copy(
      system.file(package="packrat", "Renviron"),
      path
    )
  } else {
    ## Read the .Renviron in and see if it's been packified
    .Renviron <- readLines(path)
    packifyStart <- grep("#### -- Packrat Environment Variables", .Renviron, fixed = TRUE)
    packifyEnd <- grep("#### -- End Packrat Environment Variables -- ####", .Renviron, fixed = TRUE)

    if (length(packifyStart) && length(packifyEnd)) {
      .Renviron <- .Renviron[-c(packifyStart:packifyEnd)]
    }

    ## Append init.R to the .Renviron
    .Renviron <- c(.Renviron, readLines(system.file(package="packrat", "Renviron")))
    cat(.Renviron, file = path, sep = "\n")
  }
}
