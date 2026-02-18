#
# renv 1.1.7 [rstudio/renv#53d868d]: A dependency management toolkit for R.
# Generated using `renv:::vendor()` at 2026-02-12 12:06:56.796845.
#

renv <- new.env(parent = new.env())

renv$initialize <- function(libname, pkgname) {
  # set up renv + imports environments
  attr(renv, "name") <- "embedded:renv"
  attr(parent.env(renv), "name") <- "imports:renv"

  # get imports
  imports <- list(
    tools = c(
      "file_ext",
      "md5sum",
      "package_dependencies",
      "pskill",
      "psnice",
      "write_PACKAGES"
    ),
    utils = c(
      "Rprof",
      "URLencode",
      "adist",
      "available.packages",
      "browseURL",
      "citation",
      "contrib.url",
      "download.file",
      "download.packages",
      "file.edit",
      "getCRANmirrors",
      "head",
      "help",
      "install.packages",
      "installed.packages",
      "modifyList",
      "old.packages",
      "packageDescription",
      "packageVersion",
      "read.table",
      "remove.packages",
      "sessionInfo",
      "str",
      "summaryRprof",
      "tail",
      "tar",
      "toBibtex",
      "untar",
      "unzip",
      "update.packages",
      "zip"
    )
  )

  # load the imports required by renv
  for (package in names(imports)) {
    namespace <- asNamespace(package)
    functions <- imports[[package]]
    list2env(mget(functions, envir = namespace), envir = parent.env(renv))
  }

  # source renv into the aforementioned environment
  script <- system.file("vendor/renv.R", package = .packageName)
  sys.source(script, envir = renv)

  # initialize metadata
  renv$the$metadata <- list(
    embedded = TRUE,
    version = structure(
      "1.1.7",
      md5 = "5c2a82def4966cf44b900fddbeb62fab",
      sha = "53d868dd20396f31df39ef8ed2a2a403c2ff31a7"
    )
  )

  # run our load / attach hooks so internal state is initialized
  renv$.onLoad(libname, pkgname)

  # remove our initialize method when we're done
  rm(list = "initialize", envir = renv)
}
