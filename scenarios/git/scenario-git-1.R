library(packrat)
packratHome <- "~/git/packrat"
setwd(packratHome)
try(setwd("scenarios/git/"), silent = TRUE)
source("../helper-scenarios.R")
if (!grepl("scenarios/git", getwd(), fixed = TRUE)) {
  stop("Enter the 'scenarios/git/' directory before running this script")
}
projDir <- path.expand(file.path(packratHome, "scenarios", "git", "packrat-scenario-git-1"))
mkdir(projDir)
adamDir <- file.path(projDir, "adam")
bettyDir <- file.path(projDir, "betty")
repoDir <- file.path(projDir, "repo")
repoGitDir <- file.path(repoDir, ".git")
for (dir in c(adamDir, bettyDir, repoDir)) {
  unlink(dir, recursive = TRUE)
  mkdir(dir)
}

## Initialize the repo dir and put it in a 'receiving' state
setwd(repoDir)
git("init")
git("checkout -b empty")

## Initialize adam's packrat project
setwd(adamDir)
cat("library(digest)", file = "foo.R")
git("init")
bootstrap(enter = FALSE, source.packages = packratHome)

## Add everything and push to a remote
git("add -A")
git("commit -m 'initial commit'")
git("remote add origin", repoGitDir)
git("push --set-upstream origin master")

## Betty pulls to initialize her project
setwd(bettyDir)
git("clone", repoGitDir, ".")
git("checkout master")
packrat::on(bootstrap = TRUE)
packrat::status()
# The following packages are used in your code, tracked by packrat, but no longer present in your library:
#   from   to
# digest      0.6.4.1   NA
# packrat   0.2.0.104   NA
#
# Use packrat::restore() to restore these libraries.
packrat::restore()
# Installing digest (0.6.4.1) ... OK (built source)
# Installing packrat (0.2.0.104) ... OK (built source)
# You must restart R to finish applying these changes.
