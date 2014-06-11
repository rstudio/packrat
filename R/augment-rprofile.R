## Augment the .Rprofile for a project -- if it doesn't exist, just copy
## from packrat; if it does, check it and add if necessary
augmentRprofile <- function(project = NULL) {

  project <- getProjectDir(project)
  path <- file.path(project, ".Rprofile")

  if (!file.exists(path)) {
    file.copy(
      instInitRprofileFilePath(),
      path
    )
  } else {
    editRprofileAutoloader(project, "update")
  }
}

# edit the .Rprofile for this project
editRprofileAutoloader <- function(project, action = c("update", "remove")) {

  # resolve action argument
  action <- match.arg(action)

  ## Read the user part of the .Rprofile
  path <- file.path(project, ".Rprofile")
  .Rprofile <- readRprofile(path)$user

  ## Append init.R to the .Rprofile if needed
  if (identical(action, "update"))
    .Rprofile <- c(.Rprofile, readLines(instInitRprofileFilePath()))

  ## Write the upated .Rprofile
  cat(.Rprofile, file = path, sep = "\n")
}

## Read an .Rprofile, separating the contents of the file into the packrat
## autoloader chunk and user content
readRprofile <- function(path) {

  contents <- list()

  if (file.exists(path)) {
    .Rprofile <- readLines(path)
    packifyStart <- grep("#### -- Packrat Autoloader", .Rprofile, fixed = TRUE)
    packifyEnd <- grep("#### -- End Packrat Autoloader -- ####", .Rprofile, fixed = TRUE)

    if (length(packifyStart) && length(packifyEnd)) {
      contents$autoloader <- .Rprofile[c(packifyStart:packifyEnd)]
      contents$user <- .Rprofile[-c(packifyStart:packifyEnd)]
    } else {
      contents$user <- .Rprofile
    }
  }

  contents
}

