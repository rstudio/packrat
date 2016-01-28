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

  # if the .Rprofile doesn't exist, create it
  if (!file.exists(file.path(project, ".Rprofile")))
    file.create(file.path(project, ".Rprofile"))

  ## Read the .Rprofile in and see if it's been packified
  path <- file.path(project, ".Rprofile")
  .Rprofile <- readLines(path)
  packifyStart <- grep("#### -- Packrat Autoloader", .Rprofile, fixed = TRUE)
  packifyEnd <- grep("#### -- End Packrat Autoloader -- ####", .Rprofile, fixed = TRUE)

  if (length(packifyStart) && length(packifyEnd))
    .Rprofile <- .Rprofile[-c(packifyStart:packifyEnd)]

  ## Append init.R to the .Rprofile if needed
  if (identical(action, "update"))
    .Rprofile <- c(.Rprofile, readLines(instInitRprofileFilePath()))

  ## if the .Rprofile is now empty, delete it
  if (identical(gsub("[[:space:]]", "", unique(.Rprofile)), "") ||
      !length(.Rprofile))
    file.remove(file.path(project, ".Rprofile"))
  else
    cat(.Rprofile, file = path, sep = "\n")

  invisible()
}

