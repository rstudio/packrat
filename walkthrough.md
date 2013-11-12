---
layout: home
title: Packrat by example
---

This tutorial will walk you through some of the most common tasks you'll want to do with packrat, and explain the fundamental concepts behind the package on the way.

## First things first

You're getting ready to start a new project, so you create a new directory that will eventually contain all the .R scripts, CSV data, and other files that are needed for this particular project.

You know you're going to need to make use of several R packages over the course of this project. So before you write your first line of code, set up the project directory to use packrat (an operation we call **bootstrap**):

    > packrat::bootstrap("~/projects/babynames")
    Adding these packages to packrat:
                _      
        packrat   0.1.0

    Fetching sources for packrat (0.1.0) ... OK (Github)
    Snapshot written to /home/jmcphers/projects/babynames/packrat.lock 
    Installing packrat (0.1.0) ... OK (copied local binary)
    Packrat startup directives installed. Please quit and restart your R session.

(Tip: If the current working directory is the project directory, you can omit the path.)

If `bootstrap` completes successfully, you'll see a message telling you to restart R. Do that, making sure that you're starting in the project directory ([see instructions](starting.html)); in fact, you're going to need to start in the project directory every time from now on for packrat to work.

Now that you've restarted, you're no longer in an ordinary R project; you're in a packrat project. The main difference is that **a packrat project has its own private package library**. Any packages you install from inside a packrat project are only available to that project; and packages you install outside of the project are not available to the project. (except...)

This is what we mean by "isolation" and it's Very Good Thing, as it means that upgrading a package for one project won't break a totally different project that just happens to reside on the same machine, even if that package contained incompatible changes.

A packrat project contains a few extra files and directories. The `bootstrap()` function creates these files for you, if they don't already exist.

* **`library/`:** Private package library for this project.
* **`packrat.lock`:** Lists the precise package versions that were used to satisfy dependencies, including dependencies of dependencies. (This file should never be edited by hand!)
* **`packrat.sources/`:** Source packages of all the dependencies that packrat has been made aware of.
* **`.Rprofile` and `.Renviron`:** Directs R to use the private package library (when it is started from the project directory).

## Adding, removing, and updating packages

Adding a package in a packrat project is easy. The first step is to start R inside your packrat project, and install the package however you normally do; usually that means either the `install.packages()` function or the "Install Packages" button in your favorite R IDE. Let's do this now, with the `reshape2` package.

    > install.packages("reshape2")

If you completed the previous steps correctly, you just installed the `reshape2` package from CRAN into your project's private package library. Let's see if packrat notices the new package we installed.

    > packrat::status()
    The following packages are installed but not needed:
                 _      
        plyr       1.8  
        reshape2   1.2.2
        stringr    0.6.2
    Use packrat::clean() to remove them. Or, if they are actually needed
    by your project, add `library(packagename)` calls to a .R file
    somewhere in your project.

Not only does it detect `reshape2`, but the packages that `reshape2` itself depends on as well.

But notice these packages are "installed but not needed". That's because packrat only considers packages that appear in `library()` or `require()` calls in your \*.R script files to be used by the project. Let's satisfy packrat by creating a `babynames.R` file, and put this single line in it:

    library(reshape2)

Now let's check the status again.

    > packrat::status()

    The following packages have been updated in your library, but have not
    been recorded in packrat:
                   library   packrat
        plyr           1.8        NA
        reshape2     1.2.2        NA
        stringr      0.6.2        NA
    Use packrat::snapshot() to record these packages in packrat.

That's better. Let's do what it says and call `snapshot()`.

    > packrat::snapshot()

    Adding these packages to packrat:
                 _        
        lattice    0.20-24
        plyr       1.8    
        reshape2   1.2.2  
        stringr    0.6.2  

    Fetching sources for lattice (0.20-24) ... OK (CRAN current)
    Fetching sources for plyr (1.8) ... OK (CRAN current)
    Fetching sources for stringr (0.6.2) ... OK (CRAN current)
    Fetching sources for reshape2 (1.2.2) ... OK (CRAN current)
    Snapshot written to /home/jmcphers/projects/babynames/packrat.lock 
    Installing lattice... OK (built source)

When packrat takes a snapshot, it looks in the project's private package library for packages that have been added, modified, or removed since the last time `snapshot` was called. For packages that were added or modified, packrat attempts to go find the uncompiled _source package_ from CRAN, Bioconductor, or GitHub (caveat: only for packages that were installed using `devtools` version 1.4 or later), and save them in the `packrat.sources` project subdirectory. It also records metadata about each package in a `packrat.lock` file.

Because we save source packages for all of your dependencies, packrat makes your project more **reproducible**. When someone else wants to run your project--even if that someone else is you, years in the future, dusting off some old backups--they won't need to try to figure out what versions of what packages you were running, and where you got them.

## Restoring snapshots

Once your project has a snapshot, you can easily install the packages from that snapshot into your private library at any time.

You'll need to do this, for example, when copying the project to a new computer, especially to one with a different operating system. Let's simulate this by exiting R and then deleting the `library` subdirectory in your project. Then launch R from your project directory again.

As R starts, you should be greeted with a message like this:

    Creating private package library at
    /home/jmcphers/projects/babynames/library/x86_64-pc-linux-gnu/3.0.2
    Packrat needs to install the packages this project depends on. Run
    initPackrat() to get started.

Go ahead and run `initPackrat()` as it directs. `initPackrat` is responsible for installing the packrat package itself into the private package library, and then calling `packrat::restore()` to install the rest of the snapshotted packages.

    > initPackrat()
    Initializing packrat... OK
    Installing lattice (0.20-24) ... OK (built source)
    Installing plyr (1.8) ... OK (built source)
    Installing stringr (0.6.2) ... OK (built source)
    Installing reshape2 (1.2.2) ... OK (built source)

All of the packages in the snapshot have now been installed in your project's newly created private package library.

    > packrat::status()
    Up to date.

Another reason to restore from the packrat snapshot is if you remove a package that you later realize you still needed, or if one of your collaborators makes their own changes to the snapshot. In these cases, you can call `packrat::restore()` (as `initPackrat()` is only designed for cases where even packrat itself is not installed).

Let's remove the plyr package, and use `packrat::restore()` to bring it back.

    > remove.packages("plyr")
    Removing package from
    ‘/home/jmcphers/projects/babynames/library/x86_64-pc-linux-gnu/3.0.2’
    (as ‘lib’ is unspecified)
    Changes made to this project's private library may need to be snapshotted.
    Run packrat::status() to see differences since the last snapshot.

    > packrat::status()

    The following packages are missing from your library, or are out of date:
               packrat   library
        plyr       1.8        NA
    Use packrat::restore() to install/remove the appropriate packages.

    > packrat::restore()
    Installing plyr (1.8) ... OK (built source)

## Collaboration (Git, SVN)

Packrat is designed to work with your favorite source control system. When you use Packrat with source control, you can be sure you and your collaborators are all working with the same set of packages at the same versions. For this example we'll use Git, but the same principles apply to other source control systems.

When you add a Packrat project directory to source control, you'll add its support files too. These store the packages and snapshots needed to recreate the private package library. If your project is already under source control, you just need to add the new support files:


    $ git add packrat.lock
    $ git add packrat.sources/
    $ git add .Rprofile
    $ git add .Renviron
    $ git commit -m "Add Packrat"

You can also commit the library itself, but it isn't necessary; as you saw above, Packrat can recreate the library. 

Whenever you run `packrat::snapshot()`, Packrat will make changes to the `packrat.lock` file and sources in the `packrat.sources/` directory. You'll want to include these changes with your commit. The snapshot that you take can then be applied by your collaborators so that their private libraries match yours.

Let's say you've just added another dependency to your project. As before, you run `packrat::snapshot()` to store this change:

    > packrat::snapshot()

    Adding these packages to packrat:
                 _        
        chartreuse    1.07

    Fetching sources for chartreuse (1.07) ... OK (CRAN current)
    Snapshot written to /home/jmcphers/projects/babynames/packrat.lock 

Next, you check Git to see what's changed:

    $ git status
    # On branch master
    # Changes not staged for commit:
    #   (use "git add <file>..." to update what will be committed)
    #   (use "git checkout -- <file>..." to discard changes in working directory)
    #
    #   modified:   packrat.lock
    #   modified:   packrat.sources/chartreuse/chartreuse-1.07.tar.gz
    #   modified:   colors.R

Packrat's snapshot includes changes to the lockfile, and sources for the new dependency. You commit these changes with your code. 

    $ git add colors.R
    $ git add packrat.sources/
    $ git add packrat.lock
    $ git commit -m "Add support for chartreuse" 
    $ git push
    ...
    To https://github.com/jmcphers/babynames
       d28a993..e0be144  master -> master

Later, your collaborator picks up your changes, and notices that you've made some changes in Packrat.

    $ git pull
    Updating d28a993..e0be144
    Fast-forward
     colors.R | 3 ++-
     packrat.lock | 5 ++++++
     packrat.sources/chartreuse/chartreuse-1.07.tar.gz 
     3 files changed, 7 insertions(+), 1 deletion(-)

Your collaborator applies your snapshot with `restore()`: 

    > packrat::restore()
    Installing chartreuse (1.07) ... OK (built source)

Now you're both working with the `chartreuse` package.

Of course, in addition to adding new dependencies, Packrat's controlled environment can help your team safely update and remove dependencies together without compromising your results.
