---
layout: home
title: Packrat
subtitle: Reproducible package management for R
---

## Packrat is a dependency management system for R.

Use packrat to make your R projects more:

* **Isolated:** Installing a new or updated package for one project won't break your other projects, and vice versa. That's because packrat gives each project its own private package library.
* **Portable:** Easily transport your projects from one computer to another, even across different platforms. Packrat makes it easy to install the packages your project depends on.
* **Reproducible:** Packrat records the exact package versions you depend on, and ensures those exact versions are the ones that get installed wherever you go.

<iframe id="screencast" src="//player.vimeo.com/video/78556317" width="700" height="393" frameborder="0" webkitallowfullscreen="webkitallowfullscreen" mozallowfullscreen="mozallowfullscreen" allowfullscreen="allowfullscreen"> </iframe>

## Basic concepts

If you're like the vast majority of R users, when you start working on a new R project you create a new directory for all of your R scripts and data files.

Packrat enhances your project directory by storing your package dependencies inside it, rather than relying on your personal R library that is shared across all of your other R sessions. We call this directory your **private package library** (or just **private library**). When you start an R session in a packrat project directory, R will only look for packages in your private library; and anytime you install or remove a package, those changes will be made to your private library.

Unfortunately, private libraries don't travel well; like all R libraries, their contents are compiled for your specific machine architecture, operating system, and R version. Packrat lets you **snapshot** the state of your private library, which saves to your project directory whatever information packrat needs to be able to recreate that same private library on another machine. The process of installing packages to a private library from a snapshot is called **restoring**.

## Installing packrat

Until packrat is ready for CRAN, you can install it directly from GitHub using [devtools](https://github.com/hadley/devtools). **It's very important that you use devtools 1.4 or later to install packrat, otherwise you will get errors when you try to use it.** As of this writing, devtools 1.4 is not yet on CRAN. So these instructions must be followed for now.

    > install.packages("devtools")
    > devtools::install_github("devtools")
    > detach("package:devtools", unload=TRUE)
    > devtools::install_github("rstudio/packrat")

## Next steps

* If you skipped the screencast above, it's a good idea to go back and watch it.
* We highly recommend following our **[walkthrough guide](walkthrough.html)**.
* Then check out some of the **[most common commands](commands.html)**.

## Need help?

Drop by [packrat-discuss](https://groups.google.com/group/packrat-discuss) and let us know if you have any questions or comments.

## Conflict resolution

## Caveats

### Package types

Packrat can only work with three types of packages: 

1. Packages installed from a CRAN repository (or a CRAN-like repository such as Bioconductor)
2. Packages installed by `devtools::install_github`, version 1.4 or later.
3. Local source packages (via the `sourcePackagePaths` parameter)

If you depend on a package that doesn't fall into any of these types, you'll need to coerce it into one of these types. The easiest way to do this is to put the package's source into a `.tar.gz` file, and use the `sourcePackagePaths` parameter. Note that if you use this mechanism, you will need to update the version of the package every time you make a change to the source code; Packrat relies on version numbers for local source packages to know when they've been updated.

### Building packages

Packrat prefers to install binary versions of packages from CRAN-like repositories when available. However, many packages don't have binaries, and even those that do now may not have them in the near future (CRAN does not archive binaries, only sources). 

Packrat stores the sources for each package locally, so that your project never depends on having platform binaries available from a mirror.

However, this means it is almost certain that you'll need to be able to build source packages locally in order to use Packrat. See [Package Development Prerequisites](http://www.rstudio.com/ide/docs/packages/prerequisites) to learn more about the tools to install for your operating system. It's recommended that you and your collaborators prepare your machines for package development before using Packrat.