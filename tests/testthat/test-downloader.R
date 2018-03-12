context("Downloader")

getAvailableDownloadMethods <- function() {

  if (is.windows()) {
    methods <- "internal"
    if (getRversion() >= "3.2")
      methods <- c(methods, "wininet")
    return(methods)
  }

  has <- function(program) {
    nzchar(Sys.which(program)[[1]])
  }

  methods <- "internal"

  if (has("wget"))
    methods <- c(methods, "wget")

  if (has("curl"))
    methods <- c(methods, "curl")

  if (canUseLibCurlDownloadMethod())
    methods <- c(methods, "libcurl")

  methods
}

test_that("404s are errors", {

  skip_on_cran()

  URL <- "https://cran.rstudio.com/no/such/file/here.txt"
  methods <- getAvailableDownloadMethods()

  destfile <- tempfile()
  on.exit(try(unlink(file), silent = TRUE), add = TRUE)

  for (method in methods) {
    expect_error(
      downloadFile(URL, destfile = destfile, method = method, quiet = TRUE),
      info = sprintf("(method = '%s')", method)
    )
  }

})

test_that("The same content is returned regardless of download method", {
  skip_on_cran()

  URL <- "https://cran.rstudio.org/src/base/AUTHORS"
  methods <- getAvailableDownloadMethods()
  methods <- setdiff(methods, "internal")

  contents <- lapply(methods, function(method) {

    path <- tempfile()
    on.exit(try(unlink(path), silent = TRUE), add = TRUE)

    downloadFile(URL, destfile = path, method = method, quiet = TRUE)
    readChar(path, file.info(path)$size, TRUE)

  })

  expect_true(
    length(Reduce(unique, contents)) == 1,
    info = "various download methods retrieve exact same content"
  )

})
