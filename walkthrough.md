---
layout: home
title: Packrat by example
---

This tutorial will walk you through some of the most common tasks you'll want
to do with packrat, and explain the fundamental concepts behind the package on
the way.

## First things first

You're getting ready to start a new project, so you create a new directory that
will eventually contain all the .R scripts, CSV data, and other files that are
needed for this particular project.

You know you're going to need to make use of several R packages over the course
of this project. So before you write your first line of code, set up the
project directory to use Packrat with `packrat::init`:

    > packrat::init("~/projects/babynames")
    Adding these packages to packrat:
                _         
        packrat   0.2.0.123

    Fetching sources for packrat (0.2.0.123) ... OK (GitHub)
    Snapshot written to '/Users/kevin/projects/babynames/packrat/packrat.lock'
    Installing packrat (0.2.0.123) ... OK (built source)
    init complete!
    Packrat mode on. Using library in directory:
    - "~/projects/babynames/packrat/lib" 

(Tip: If the current working directory is the project directory, you can omit
the path.)

After initializing the project, you will be placed into packrat mode in the
project directory. You're ready to go!

You're no longer in an ordinary R project; you're in a Packrat project. The
main difference is that **a packrat project has its own private package
library**. Any packages you install from inside a packrat project are only
available to that project; and packages you install outside of the project are
not available to the project.

This is what we mean by "isolation" and it's Very Good Thing, as it means that
upgrading a package for one project won't break a totally different project
that just happens to reside on the same machine, even if that package contained
incompatible changes.

A packrat project contains a few extra files and directories. The `init()`
function creates these files for you, if they don't already exist.

* **`packrat/packrat.lock`:** Lists the precise package versions that were used
  to satisfy dependencies, including dependencies of dependencies. (This file
  should never be edited by hand!)
* **`packrat/packrat.opts`:** Project-specific packrat options. These can be
  queried and set with `get_opts` and `set_opts`; see `?"packrat-options"` for
  more information.
* **`packrat/lib/`:** Private package library for this project.
* **`packrat/src/`:** Source packages of all the dependencies that packrat has
  been made aware of.
* **`.Rprofile`:** Directs R to use the private package library (when it is
  started from the project directory).

## Adding, removing, and updating packages

Adding a package in a Packrat project is easy. The first step is to start R
inside your Packrat project, and install the package however you normally do;
usually that means either the `install.packages()` function or the "Install
Packages" button in your favorite R IDE. Let's do this now, with the `reshape2`
package.

    > install.packages("reshape2")

If you completed the previous steps correctly, you just installed the
`reshape2` package from CRAN into your project's private package library. Let's
see if packrat notices the new package we installed.

    > packrat::status()
    The following packages are installed but not needed:
                 _       
        plyr       1.8.1 
        Rcpp       0.11.2
        reshape2   1.4   
        stringr    0.6.2 

    Use packrat::clean() to remove them. Or, if they are actually needed
    by your project, add `library(packagename)` calls to a .R file
    somewhere in your project.

Not only does it detect `reshape2`, but the packages that `reshape2` itself
depends on as well.

But notice these packages are "installed but not needed". That's because
packrat only considers packages that appear in `library()` or `require()` calls
in your \*.R script files to be used by the project. Let's satisfy packrat by
creating a `babynames.R` file, and put this single line in it:

    library(reshape2)

Now let's check the status again.

    > packrat::status()

    The following packages have been updated in your library, but have not been recorded in packrat:
                   library   packrat
        plyr         1.8.1        NA
        Rcpp        0.11.2        NA
        reshape2       1.4        NA
        stringr      0.6.2        NA

    Use packrat::snapshot() to record these packages in packrat.

That's better. Let's do what it says and call `snapshot()`.

    > packrat::snapshot()

    Adding these packages to packrat:
                 _       
        plyr       1.8.1 
        Rcpp       0.11.2
        reshape2   1.4   
        stringr    0.6.2 

    Fetching sources for plyr (1.8.1) ... OK (CRAN current)
    Fetching sources for Rcpp (0.11.2) ... OK (CRAN current)
    Fetching sources for reshape2 (1.4) ... OK (CRAN current)
    Fetching sources for stringr (0.6.2) ... OK (CRAN current)
    Snapshot written to '/Users/kevin/projects/babynames/packrat/packrat.lock'

When packrat takes a snapshot, it looks in the project's private package
library for packages that have been added, modified, or removed since the last
time `snapshot` was called. For packages that were added or modified, packrat
attempts to go find the uncompiled _source package_ from CRAN, BioConductor, or
GitHub (caveat: only for packages that were installed using `devtools` version
1.4 or later), and save them in the `packrat/src` project subdirectory. It
also records metadata about each package in the `packrat.lock` file.

Because we save source packages for all of your dependencies, packrat makes
your project more **reproducible**. When someone else wants to run your
project--even if that someone else is you, years in the future, dusting off
some old backups--they won't need to try to figure out what versions of what
packages you were running, and where you got them.

## Restoring snapshots

Once your project has a snapshot, you can easily install the packages from that
snapshot into your private library at any time.

You'll need to do this, for example, when copying the project to a new
computer, especially to one with a different operating system. Let's simulate
this by exiting R and then deleting the `library` subdirectory in your project.
Then launch R from your project directory again.

Packrat automates the whole process for you -- upon restarting R in this
directory, you should see the following output:

    Packrat is not installed in the local library -- attempting to bootstrap an installation...
    > Installing packrat into project private library:
    - '/Users/kevin/projects/babynames/packrat/lib/x86_64-apple-darwin13.1.0/3.2.0'
    * installing *source* package ‘packrat’ ...
    ** R
    ** inst
    ** preparing package for lazy loading
    ** help
    *** installing help indices
    ** building package indices
    ** testing if installed package can be loaded
    * DONE (packrat)
    > Attaching packrat
    > Restoring library
    Installing plyr (1.8.1) ... OK (built source)
    Installing Rcpp (0.11.2) ... OK (built source)
    Installing reshape2 (1.4) ... OK (built source)
    Installing stringr (0.6.2) ... OK (built source)
    > Packrat bootstrap successfully completed. Entering packrat mode...
    Packrat mode on. Using library in directory:
    - "~/projects/babynames/packrat/lib"


All of the packages in the snapshot have now been installed in your project's
newly created private package library.

    > packrat::status()
    Up to date.

Another reason to restore from the packrat snapshot is if you remove a package
that you later realize you still needed, or if one of your collaborators makes
their own changes to the snapshot. In these cases, you can call
`packrat::restore()`.

Let's remove the plyr package, and use `packrat::restore()` to bring it back.

    > remove.packages("plyr")
    Removing package from ‘/Users/kevin/projects/babynames/packrat/lib/x86_64-apple-darwin13.1.0/3.2.0’
    (as ‘lib’ is unspecified)

    > packrat::status()

    The following packages are used in your code, tracked by packrat, but no longer present in your library:
                from   to
        plyr   1.8.1   NA

    Use packrat::restore() to restore these libraries.

    > packrat::restore()
    Installing plyr (1.8.1) ... OK (built source)

<!--

## Collaboration (Git, SVN)

Packrat is designed to work with your favorite source control system. When you
use Packrat with source control, you can be sure you and your collaborators are
all working with the same set of packages at the same versions. For this
example we'll use Git, but the same principles apply to other source control
systems.

When using Git, Packrat will automatically update the project's local `.gitignore` file to ignore the project local library; if you want to assert more fine-grained control, you can control whether your project library and / or sources are ignored by Git with the options:

    > packrat::set_opts(vcs.ignore.lib = TRUE)
    > packrat::set_opts(vcs.ignore.src = FALSE)

Whenever you run `packrat::snapshot()`, Packrat will make changes to the
`packrat.lock` file and sources in the `packrat/src/` directory. You'll
want to include these changes with your commit. The snapshot that you take can
then be applied by your collaborators so that their private libraries match
yours.

Let's say you've just added another dependency to your project. As before, you
run `packrat::snapshot()` to store this change:

    > packrat::snapshot()

    Adding these packages to packrat:
               _      
        digest   0.6.4

    Fetching sources for digest (0.6.4) ... OK (CRAN current)
    Snapshot written to '/Users/kevin/projects/babynames/packrat/packrat.lock'

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
     packrat.sources/chartreuse/chartreuse-1.07.tar.gz | Bin 0 -> 20783 bytes
     3 files changed, 7 insertions(+), 1 deletion(-)

Your collaborator applies your snapshot with `restore()`: 

    > packrat::restore()
    Installing chartreuse (1.07) ... OK (built source)

Now you're both working with the `chartreuse` package.

Of course, in addition to adding new dependencies, Packrat's controlled environment can help your team safely update and remove dependencies together without compromising your results.

## Conflict resolution

So far we've talked mostly about `packrat::status()` reporting single packages that need to be added or removed. However, if you don't snapshot frequently or forget to apply a snapshot from a collaborator for a while, the output can get complex.

Let's say that your collaborator added a dependency called `fuchsia`. Meanwhile, you removed your dependency on `chartreuse` (but didn't remove the library) and you added one called `sienna`.  Now you want to snapshot your changes, so you call `packrat::status()` to see what will be changed. 

    > packrat::status()
    
    The following packages are missing from your library, or are out of date:
                    packrat   library
        fuchsia         1.1        NA
    Use packrat::restore() to install/remove the appropriate packages.
    
    The following packages have been updated in your library, but have not
    been recorded in packrat:
                 library   packrat
        sienna   1.1.7-2        NA
    Use packrat::snapshot() to record these packages in packrat.
    
    The following packages are installed but not needed:
             _      
        chartreuse   1.07
    Use packrat::clean() to remove them. Or, if they are actually needed
    by your project, add `library(packagename)` calls to a .R file
    somewhere in your project.

The best way to deal with this output is to *do the actions in the order suggested*. In other words:

1. Use `packrat::restore()` to apply your collaborator's change. Now your private library has your changes *and* your collaborator's change.
2. Use `packrat::snapshot()` to take a snapshot of the new library. Now Packrat is up to date with both changes.
3. Use `packrat::clean()` to clean up libraries no longer in use.

If you'd swapped the order of the first steps--that is, forced a snapshot as your first operation--you'd overwrite the snapshot your collaborator took, losing their changes.

-->
