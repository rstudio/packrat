---
layout: home
---

<iframe id="screencast" src="//player.vimeo.com/video/78556317" width="600" height="337" frameborder="0" webkitallowfullscreen="webkitallowfullscreen" mozallowfullscreen="mozallowfullscreen" allowfullscreen="allowfullscreen"> </iframe>

## Packrat is a dependency management system for R.

Use packrat to make your R projects more:

* **Isolated:** Installing a new or updated package for one project won't break your other projects, and vice versa. That's because packrat gives each project its own private package library.
* **Portable:** Easily transport your projects from one computer to another, even across different platforms. Packrat makes it easy to install the packages your project depends on.
* **Reproducible:** Packrat records the exact package versions you depend on, and ensures those exact versions are the ones that get installed wherever you go.

## Installing packrat

Until packrat is ready for CRAN, you can install it directly from GitHub using [devtools](https://github.com/hadley/devtools):

    install.packages("devtools")
    devtools::install_github("rstudio/packrat")

## Basic concepts

If you're like the vast majority of R users, when you start working on a new R project you create a new directory for all of your R scripts and data files.

Packrat enhances your project directory by storing your package dependencies inside it, rather than relying on your personal R library that is shared across all of your other R sessions. We call this directory your **private package library** (or just **private library**). When you start an R session in a packrat project directory, R will only look for packages in your private library; and anytime you install or remove a package, those changes will be made to your private library.

Unfortunately, private libraries don't travel well; like all R libraries, their contents are compiled for your specific machine architecture, operating system, and R version. Packrat lets you **snapshot** the state of your private library, which saves to your project directory whatever information packrat needs to be able to recreate that same private library on another machine. The process of installing packages to a private library from a snapshot is called **restoring**.

## Commands

Use `bootstrap()` to create a new packrat project, `snapshot()` to record changes to your project's library, and `restore()` to recreate your library the way it was the last time you (or anyone!) took a snapshot. 

Using these simple functions and sharing packrat's files lets you collaborate in a shared, consistent environment with others as your project grows and changes, and provides an easy way to share your results when you're done.

    bootstrap(projDir = ".")

Initializes a regular R project directory as a packrat project. This creates the supporting files and directories listed below in "Anatomy of a packrat project", including a private library and snapshot. **You must restart your R session after running `bootstrap()` in order to use packrat.**

    status(projDir = '.')

Shows the differences between the project's packrat snapshot, its private package library, and its R scripts.

These differences are created when you use the normal R package management commands like `install.packages()`, `update.packages()`, and `remove.packages()`. To bring these differences into packrat, you can use `snapshot()`.

Differences can also arise if one of your collaborators adds or removes packages from the packrat snapshot. In this case, you simply need to tell packrat to update your private package library using `restore()`.

    snapshot(projDir = ".")

Stores the state of the private library (each package and its exact version) in packrat. 

You'll need to call this after making changes to the private library as described above. Snapshotting your library makes it possible to restore to the snapshot later, and if you're sharing a project with someone else using a version control system, packrat can use the snapshot to mirror your library changes on your collaborator's library. 

    restore(projDir = ".")

Adds, removes, and changes packages installed in the private library so that they match the state of the most recent snapshot.

You'll need to call this after copying a project onto a new machine, or if you're using version control (see section below) and someone else added a package that you don't have installed yet.

    clean(projDir = ".")

Removes any packages in the private library that aren't being referenced from .R files in the project.

## Anatomy of a packrat project

A packrat project contains a few extra files and directories. The `bootstrap()` function creates these files for you, if they don't already exist.

* `library/`: Private package library for this project.
* `packrat.lock`: Lists the precise package versions that were used to satisfy dependencies, including dependencies of dependencies. (This file should never be edited by hand!)
* `packrat.sources/`: Source packages of all the dependencies that packrat has been made aware of.
* `.Rprofile` and `.Renviron`: Directs R to use the private package library (when it is started from the project directory).

## Using packrat with version control

Packrat is designed to work hand in hand with Git, Subversion, or any other version control system. Be sure to check in the `.Rprofile`, `.Renviron`, and `packrat.lock` files, as well as everything under `packrat.sources/`. You can tell your VCS to ignore `library` (or feel free to check it in if you don't mind taking up some extra space in your repository).
