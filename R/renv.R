
#
# renv 1.0.1 [rstudio/renv#5dc2fc9]: A dependency management toolkit for R.
# Generated using `renv:::vendor()` at 2023-08-11 10:13:20.914354.
#


renv <- new.env(parent = new.env())

renv$initialize <- function() {

  # set up renv + imports environments
  attr(renv, "name") <- "embedded:renv"
  attr(parent.env(renv), "name") <- "imports:renv"

  # get imports
  imports <- list(
    tools = c(
      "file_ext",
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
    version = structure("1.0.1", sha = "5dc2fc934867a1b34ec9d1c594dc57d3c9698650")
  )

  # run our load / attach hooks so internal state is initialized
  renv$renv_zzz_load()

  # remove our initialize method when we're done
  rm(list = "initialize", envir = renv)

}
