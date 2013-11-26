---
layout: home
title: Packrat
subtitle: Reproducible package management for R
---

## Packrat is a dependency management system for R.

R package dependencies can be frustrating. Have you ever had to use trial-and-error to figure out what R packages you need to install to make someone else's code work--and then been left with those packages globally installed forever, because now you're not sure whether you need them? Have you ever updated a package to get code in one of your projects to work, only to find that the updated package makes code in another project *stop* working? 

We built packrat to solve these problems. Use packrat to make your R projects more:

* **Isolated:** Installing a new or updated package for one project won't break your other projects, and vice versa. That's because packrat gives each project its own private package library.
* **Portable:** Easily transport your projects from one computer to another, even across different platforms. Packrat makes it easy to install the packages your project depends on.
* **Reproducible:** Packrat records the exact package versions you depend on, and ensures those exact versions are the ones that get installed wherever you go.

<iframe id="screencast" src="//player.vimeo.com/video/79537844?title=0&amp;byline=0&amp;portrait=0" width="700" height="393" frameborder="0" webkitallowfullscreen="webkitallowfullscreen" mozallowfullscreen="mozallowfullscreen" allowfullscreen="allowfullscreen"> </iframe>

## Basic concepts

If you're like the vast majority of R users, when you start working on a new R project you create a new directory for all of your R scripts and data files.

Packrat enhances your project directory by storing your package dependencies inside it, rather than relying on your personal R library that is shared across all of your other R sessions. We call this directory your **private package library** (or just **private library**). When you start an R session in a packrat project directory, R will only look for packages in your private library; and anytime you install or remove a package, those changes will be made to your private library.

Unfortunately, private libraries don't travel well; like all R libraries, their contents are compiled for your specific machine architecture, operating system, and R version. Packrat lets you **snapshot** the state of your private library, which saves to your project directory whatever information packrat needs to be able to recreate that same private library on another machine. The process of installing packages to a private library from a snapshot is called **restoring**.

## Installing packrat

Until packrat is ready for CRAN, you can install it directly from GitHub using [devtools](https://github.com/hadley/devtools). **It's very important that you use devtools 1.4 or later to install packrat, otherwise you will get errors when you try to use it.** As of this writing, devtools 1.4 is not yet on CRAN. So these instructions must be followed precisely.

    > install.packages("devtools")
    > devtools::install_github("rstudio/packrat")

You'll also need to make sure your machine is able to build packages from source. See [Package Development Prerequisites](http://www.rstudio.com/ide/docs/packages/prerequisites) for the tools needed for your operating system.

## Next steps

* If you skipped the screencast above, it's a good idea to go back and watch it.
* We highly recommend following our **[walkthrough guide](walkthrough.html)**.
* Then check out some of the **[most common commands](commands.html)**.
* We also have a short list of **[limitations and caveats](limitations.html)** you should be aware of.

## Need help?

Drop by [packrat-discuss](https://groups.google.com/group/packrat-discuss) and let us know if you have any questions or comments.
