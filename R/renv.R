
#
# renv 0.17.3-84: A dependency management toolkit for R.
# Generated using `renv:::vendor()` at 2023-06-12 09:09:27.
#


renv <- new.env(parent = new.env())

renv$initialize <- function() {

  # set up renv + imports environments
  attr(renv, "name") <- "embedded:renv"
  attr(parent.env(renv), "name") <- "imports:renv"

  # set up metadata environment
  metadata <- list(
    embedded = TRUE,
    version  = "1a0dd23ff61f7e0d2a3dc7a6e7b7e36a4b6c5de0"
  )

  # assign into renv
  assign("_renv_metadata", as.environment(metadata), envir = renv)

  # get imports
  imports <- list(
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

  # remove our initialize method when we're done
  rm(list = "initialize", envir = renv)

}
