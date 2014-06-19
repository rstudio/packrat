# Make an asynchronous call to available.packages in order to cache the current
# set of available packages for the session
cacheAvailablePackages <- function() {
  repos <- getOption("repos")
  reposCmd <- paste0("options('repos' = ", deparse(repos, width.cutoff = 500), ")")
  fileCopyCmd <- paste0("file.copy(file, file.path(", shQuote(tempdir()), ", basename(file)))")
  fullCmd <- paste(reposCmd,
                   "ap <- available.packages()",
                   "file <- list.files(path = tempdir(), pattern = '^repos_http', full.names = TRUE)",
                   fileCopyCmd,
                   sep = "; ")
  cmd <- paste("R --vanilla --slave -e", shQuote(fullCmd))
  system(cmd, wait = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)
}

