## Augment the .Rprofile for a project -- if it doesn't exist, just copy
## from packrat; if it does, check it and add if necessary
augmentRprofile <- function(projDir = NULL) {
  projDir <- getProjectDir(projDir)

  path <- file.path(projDir, ".Rprofile")
  if (!file.exists(path)) {
    file.copy(
      system.file(package="packrat", "init.R"),
      path
    )
  } else {

    ## Read the .Rprofile in and see if it's been packified
    .Rprofile <- readLines(path)
    packifyStart <- grep("#### -- packrat::packify -- ####", .Rprofile, fixed = TRUE)
    packifyEnd <- grep("#### -- end packrat::packify -- ####", .Rprofile, fixed = TRUE)

    if (length(packifyStart) && length(packifyEnd)) {
      .Rprofile <- .Rprofile[-c(packifyStart:packifyEnd)]
    }

    ## Append init.R to the .Rprofile
    .Rprofile <- c(.Rprofile, readLines(system.file(package="packrat", "init.R")))
    cat(.Rprofile, file = path, sep = "\n")

  }
}
