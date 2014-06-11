options(warn = 2)
options(error = recover)

## Make sure you are in the scenarios directory before executing this script!
library(packrat)
packratHome <- "~/git/packrat"
setwd(packratHome)
try(setwd("scenarios/git/"), silent = TRUE)
source("../helper-scenarios.R")
if (!grepl("scenarios/git", getwd(), fixed = TRUE)) {
  stop("Enter the 'scenarios/git/' directory before running this script")
}

projDir <- file.path(tempdir(), "packrat-scenario-git-1")
mkdir(projDir)
adamDir <- file.path(projDir, "adam")
bettyDir <- file.path(projDir, "betty")
repoDir <- file.path(projDir, "repo")
repoGitDir <- file.path(repoDir, ".git")
for (dir in c(adamDir, bettyDir, repoDir)) {
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
packrat::on()
packrat::status()
