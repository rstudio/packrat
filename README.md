# packrat

Packrat is a dependency management system for R.

Use packrat to make your R projects more:

* **Isolated:** Installing a new or updated package for one project won't break your other projects, and vice versa. That's because packrat gives each project its own private package library.
* **Portable:** Easily transport your projects from one computer to another, even across different platforms. Packrat makes it easy to install the packages your project depends on.
* **Reproducible:** Packrat records the exact package versions you depend on, and ensures those exact versions are the ones that get installed wherever you go.

## Commands

#### `bootstrap(appDir = ".")`

> Initializes a regular R project directory as a packrat project. This creates the supporting files and directories listed below in "Anatomy of a packrat project". **You must restart your R session after running `bootstrap()` in order to use packrat.**

#### `status(appDir = '.', lib.loc = NULL, quiet = FALSE)`

> Shows the differences between the project's packrat dependencies, its private package library, and its R scripts.

> These differences are created when you use the normal R package management commands like `install.packages()`, `update.packages()`, and `remove.packages()`. To bring these differences into packrat, you can use `snapshot()`.

> Differences can also arise if one of your collaborators adds or removes packages from the packrat dependencies. In this case, you simply need to tell packrat to update your private package library using `restore()`.

#### `snapshot(appDir = ".")`

> Stores the state of the private library (each package and its exact version) in packrat. 

> You'll need to call this after making changes to the private library as described above. Snapshotting your library makes it possible to restore to the snapshot later, and if you're sharing a project with someone else using a version control system, packrat can use the snapshot to mirror your library changes on your collaborator's library. 

#### `restore(appDir = ".")`

> Adds, removes, and changes packages installed in the private library so that they match the state of the most recent snapshot.

> You'll need to call this after copying a project onto a new machine, or if you're using version control (see section below) and someone else added a package that you don't have installed yet. 

## Anatomy of a packrat project

A packrat project contains a few extra files and directories. The `bootstrap()` function creates these files for you, if they don't already exist.

* `library/`: Private package library for this project.
* `packrat.lock`: Lists the precise package versions that were used to satisfy dependencies, including dependencies of dependencies. (This file should never be edited by hand!)
* `packrat.sources/`: Source packages of all the dependencies that packrat has been made aware of.
* `.Rprofile` and `.Renviron`: Directs R to use the private package library (when it is started from the project directory).

## Using packrat with version control

Packrat is designed to work hand in hand with Git, Subversion, or any other version control system. Be sure to check in the `.Rprofile`, `.Renviron`, and `packrat.lock` files, as well as everything under `packrat.sources/`. You can tell your VCS to ignore `library` (or feel free to check it in if you don't mind taking up some extra space in your repository).
