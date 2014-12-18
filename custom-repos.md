---
layout: home
title: How to Set Up a Custom CRAN-like Repository
---

This document will walk you through how to create a custom, CRAN-like repository.
We will create a CRAN-like repository on-disk, add a package to it, and then use
`install.packages()` to install it.

First, we create the directory itself -- we'll put it at `~/local-cran`:

    localCRAN <- path.expand("~/local-cran")
    dir.create(localCRAN)

Next, we create the `src/contrib` directory -- this is where the source tarballs
for uploaded packages will live.

    contribDir <- file.path(localCRAN, "src", "contrib")
    dir.create(contribDir, recursive = TRUE)

Now, we'll create the 'binary' directories, where build versions of these packages
(for the various architectures supported) live. Note that these folders should
exist, but it is not necessary to populate them.

    binPaths <- list(
      win.binary = "bin/windows/contrib",
      mac.binary = "bin/macosx/contrib",
      mac.binary.mavericks = "bin/macosx/mavericks/contrib",
      mac.binary.leopard = "bin/macosx/leopard/contrib"
    )

    binPaths <- lapply(binPaths, function(x) file.path(localCRAN, x))
    lapply(binPaths, function(path) {
      dir.create(path, recursive = TRUE)
    })

Now, let's generate a simple package and 'upload' it to our CRAN-like
repository. We would _like_ to just use 'package.skeleton()', but it creates
packages that cannot actually be built and installed -- so we use 'pkgKitten'
instead, which generates buildable package skeletons.

    if (!require("pkgKitten")) {
      install.packages("pkgKitten")
      require("pkgKitten")
    }
    kitten("sashimi", path = tempdir())
    pkgDir <- file.path(tempdir(), "sashimi")

Now, the key step -- we give this package a custom repository name. The idea
is that we will now tie this name to a particular URL on disk for installation.
We will call this custom repository 'sushi'; to enable this, we will annotate
the DESCRIPTION file generated with 'Repository: sushi'. The idea is that
all packages that will 'live' on this custom repository should get the
'Repository: sushi' field.

    sashimiDescPath <- file.path(tempdir(), "sashimi", "DESCRIPTION")
    cat("Repository: sushi", file = sashimiDescPath, append = TRUE, sep = "\n")

Now that we've set up the package source to name it's 'home' repository,
we can build it and 'upload' it to CRAN!

    owd <- getwd()
    setwd(tempdir())
    system("R CMD build sashimi")
    setwd(owd)
    
    file.copy(
      file.path(tempdir(), "sashimi_1.0.tar.gz"),
      file.path(contribDir, "sashimi_1.0.tar.gz")
    )

We're almost done. The last step we need to do (and, for a real repository,
should be run whenever the repository is updated), is to write the PACKAGES
file for each sub-directory. This file is used by R's functions like
`install.packages()` for querying information about the repository, and what
packages are actually available.

    tools::write_PACKAGES(contribDir, type = "source")
    lapply(binPaths, function(path) {
      tools::write_PACKAGES(path)
    })

Great! Your custom CRAN repository tree should now look like:

    ~/local-cran
    ├── bin
    │   ├── macosx
    │   │   ├── contrib
    │   │   │   ├── PACKAGES
    │   │   │   └── PACKAGES.gz
    │   │   ├── leopard
    │   │   │   └── contrib
    │   │   │       ├── PACKAGES
    │   │   │       └── PACKAGES.gz
    │   │   └── mavericks
    │   │       └── contrib
    │   │           ├── PACKAGES
    │   │           └── PACKAGES.gz
    │   └── windows
    │       └── contrib
    │           ├── PACKAGES
    │           └── PACKAGES.gz
    └── src
        └── contrib
            ├── PACKAGES
            ├── PACKAGES.gz
            └── sashimi_1.0.tar.gz

    11 directories, 13 files

Now, we just need to teach R about this repository, and where it exists. This
is as simple as setting the 'repos' option, giving mapping the repository name
(sushi) to a URI (in this case, the path on disk).

    oldRepos <- getOption('repos')
    cranURI <- paste("file://", normalizePath(localCRAN, winslash = "/"), sep = "")
    options(repos = c(oldRepos, sushi = cranURI))

If we've done this correctly, `install.packages` will just 'know' how to
install this package. Note that binaries won't be available for it (since
you would have to generate your own binary build machines, to generate binaries from
uploaded sources) -- but we can install the source packages just fine.

    install.packages("sashimi", type = "source")

Bam! You just installed a package from your own custom-made CRAN-like repository!

Wait... where does Packrat fit in all of this? Well, Packrat already
understands how to communicate with CRAN-like repositories -- it's what it does
by default. So, all you need to do in your Packrat project is:

1. Update the `repos` in your project, analogous to the command above `options(repos = c(oldRepos, sushi = cranURI))`, and
2. Call `packrat::snapshot()` to update the repositories associated with your project,

and you're good to go!
