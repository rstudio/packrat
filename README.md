# packrat

Packrat is a dependency management system for R.

Use packrat to make your R projects more:

* **Isolated:** Installing a new or updated package for one project won't affect your other projects, and vice versa. That's because packrat gives each project its own private package library.
* **Portable:** Easily transport your projects from one computer to another, even across different platforms. Packrat makes it easy to install the packages your project depends on.
* **Reproducible:** Packrat records the exact package versions you depend on, and ensures those exact versions get installed in the future.

## Getting started

To turn an R project directory into a packrat project, start R from that directory, then run:

```
packrat::bootstrap()
```

After the bootstrap process succeeds, you'll need to exit and restart R (again, ensuring that you start it from the project directory).

## Anatomy of a packrat project

A packrat project comes with a few extra files and directories. The bootstrap process will create these files for you, if they don't already exist.

* `library/`: Private package library for this project. The packages your project depends on will be stored here, in both source and binary form.
* `DESCRIPTION`: Lists the packages that the project directly depends on. During bootstrap, Packrat scans your R code for `library` and `require` calls and populates this file automatically, but you can also edit this file by hand.
* `packrat.lock`: Lists the precise package versions that were used to satisfy the dependencies, including dependencies of dependencies. (This file should never be edited by hand!)
* `.Rprofile` and `.Renviron`: Directs R to use the private package library (when it is started from the project directory).

## Using packrat with version control

Packrat is designed to work hand in hand with Git, Subversion, or any other version control system. Be sure to check in the `.Rprofile`, `.Renviron`, `DESCRIPTION`, and `packrat.lock` files, as well as everything under `library/src`. You can tell your VCS to ignore `library/bin` (or feel free to check it in if you don't mind taking up some extra space in your repository).
