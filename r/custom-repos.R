## Create a custom CRAN-like repository locally, at
## '~/local-cran'. We will simulate building and installing
## a source package within a Packrat project.

## This CRAN-like repository will _not_ serve binary packages,
## so we need to ensure that we only install from source there.
options(pkgType = "source")

localCRAN <- path.expand("~/local-cran")
dir.create(localCRAN)

contribDir <- file.path(localCRAN, "src", "contrib")
dir.create(contribDir, recursive = TRUE)

# Binary paths are versioned based on R -- we'll create a path
# for our current version of R, but leave it empty.
rVersion <- paste(unlist(getRversion())[1:2], collapse = ".")
binPaths <- list(
  win.binary = file.path("bin/windows/contrib", rVersion),
  mac.binary = file.path("bin/macosx/contrib", rVersion),
  mac.binary.mavericks = file.path("bin/macosx/mavericks/contrib", rVersion),
  mac.binary.leopard = file.path("bin/macosx/leopard/contrib", rVersion)
)

binPaths <- lapply(binPaths, function(x) file.path(localCRAN, x))
lapply(binPaths, function(path) {
  dir.create(path, recursive = TRUE)
})

if (!require("pkgKitten")) {
  install.packages("pkgKitten")
  require("pkgKitten")
}
kitten("sashimi", path = tempdir())
pkgDir <- file.path(tempdir(), "sashimi")

sashimiDescPath <- file.path(tempdir(), "sashimi", "DESCRIPTION")
cat("Repository: sushi", file = sashimiDescPath, append = TRUE, sep = "\n")

# Go to the temporary directory and build 'sashimi'
owd <- getwd()
setwd(tempdir())
system("R CMD build sashimi")
setwd(owd)

# Copy it to the 'src/contrib' sub-directory
file.copy(
  file.path(tempdir(), "sashimi_1.0.tar.gz"),
  file.path(contribDir, "sashimi_1.0.tar.gz")
)

tools::write_PACKAGES(contribDir, type = "source")
lapply(binPaths, function(path) {
  tools::write_PACKAGES(path)
})

oldRepos <- getOption("repos")
cranURI <- paste("file://", normalizePath(localCRAN, winslash = "/"), sep = "")
options(repos = c(oldRepos, sushi = cranURI))

dir.create("~/packrat-custom-repos-project")
setwd("~/packrat-custom-repos-project")
packrat::init()
install.packages("sashimi", type = "source")
packrat::snapshot()
packrat::restore()