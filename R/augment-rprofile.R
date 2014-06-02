## Augment the .Rprofile for a project -- if it doesn't exist, just copy
## from packrat; if it does, check it and add if necessary
augmentRprofile <- function(project = NULL) {
  project <- getProjectDir(project)

  path <- file.path(project, ".Rprofile")
  if (!file.exists(path)) {
    file.copy(
      system.file(package="packrat", "init.R"),
      path
    )
  } else {

    ## Read the .Rprofile in and see if it's been packified
    .Rprofile <- readLines(path)
    packifyStart <- grep("#### -- Packrat Autoloader", .Rprofile, fixed = TRUE)
    packifyEnd <- grep("#### -- End Packrat Autoloader -- ####", .Rprofile, fixed = TRUE)

    if (length(packifyStart) && length(packifyEnd)) {
      .Rprofile <- .Rprofile[-c(packifyStart:packifyEnd)]
    }

    ## Append init.R to the .Rprofile
    .Rprofile <- c(.Rprofile, readLines(system.file(package="packrat", "init.R")))
    cat(.Rprofile, file = path, sep = "\n")

  }
}
