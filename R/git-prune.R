git_files <- function(project = NULL) {
  project <- getProjectDir(project)
  owd <- getwd()
  setwd(project)
  on.exit(setwd(owd))
  objects <- system("git rev-list --all --objects", intern = TRUE)
  splat <- strsplit(objects, " ", fixed = TRUE)
  sort(unique(unlist(sapply(splat, function(x) {
    if (length(x) == 2) x[[2]] else NULL
  }))))
}

git_prune <- function(project = NULL, prune.lib = TRUE, prune.src = FALSE) {
  # See: http://stevelorek.com/how-to-shrink-a-git-repository.html for notes
  project <- getProjectDir(project)
  owd <- getwd()
  setwd(project)
  on.exit(setwd(owd))
  if (!isGitProject(project))
    stop("Not a git project (no .git/ directory found)", call. = FALSE)

  localBranches <- system("git branch", intern = TRUE)
  localBranches <- gsub("^[[:blank:]]*", "", localBranches)
  allBranches <- system("git branch -a | grep remotes | grep -v HEAD | grep -v master",
                        intern = TRUE)
  allBranches <- gsub("^[[:blank:]]*", "", allBranches)
  needsClone <- allBranches[!(gsub(".*/", "", allBranches) %in% localBranches)]

  if (length(needsClone)) {
    message("Deep-cloning Git repository...")
    for (branch in needsClone)
      system(paste("git branch --track",
                   gsub(".*/", "", branch),
                   branch), ignore.stdout = TRUE, ignore.stderr = TRUE)
    message("Done!")
  }

  allFiles <- git_files()
  toRemove <- character()
  if (prune.lib)
    toRemove <- c(toRemove, grep("^packrat/lib*/", allFiles, value = TRUE))
  if (prune.src)
    toRemove <- c(toRemove, grep("^packrat/src/", allFiles, value = TRUE))

  n <- length(toRemove)
  if (!n) {
    message("Nothing to prune. Exiting...")
    return(invisible(character()))
  }

  for (i in seq_along(toRemove)) {
    file <- toRemove[[i]]
    message("Removing file ", i, " of ", n, "...")
    cmd <- paste("git filter-branch --tag-name-filter cat --index-filter",
                 "'git rm -r --cached --ignore-unmatch",
                 file,
                 "' --prune-empty -f -- --all")
    system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  }

  unlink(file.path(project, ".git", "refs", "original"), recursive = TRUE)
  system("git reflog expire --expire=now --all")
  system("git gc --prune=now")
  system("git gc --aggressive --prune=now")
  message("Done!")
  return(invisible(toRemove))

}
