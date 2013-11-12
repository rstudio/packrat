---
layout: home
title: Packrat commands
---
<style type="text/css">
pre {
  font-weight: bold;
  margin-top: 42px !important;
}
</style>

The following is a list of the R functions in the packrat package that you'll use most often. You can find more detailed documentation by typing `help(package="packrat")` at the R console.

For more background on how these commands fit into the packrat workflow, see [the walkthrough](walkthrough.html).

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
