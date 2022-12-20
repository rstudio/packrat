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

test_that("renvDownload calls renv$download, passing in the values it received", {
  url <- "https://github.com/my-great-org/cool-repo.tar.gz"
  destfile <- nullfile()
  type <- "github"

  renv_download_mock <- mockery::mock(destfile)
  mockery::stub(renvDownload, "renv$download", renv_download_mock)

  renvDownload(url, destfile, type = type)

  mockery::expect_called(renv_download_mock, 1)
  mockery::expect_args(renv_download_mock, 1, url, destfile, type)
})

test_that("authDownloadAdvice offers sound advice", {

  # Using renv for downloads
  expect_true(grepl("Packrat is configured to use internal renv for authenticated downloads.", authDownloadAdvice("github", TRUE, "renv")(), fixed = TRUE))

  # Using httr for downloads
  expect_true(grepl("Packrat will use the httr package for authenticated downloads.", authDownloadAdvice("gitlab", TRUE, "httr")(), fixed = TRUE))

  # With no available auth methods
  expect_true(grepl("Packrat is not configured to use an auth-capable download method. Try setting the option packrat.authenticated.downloads.use.renv to TRUE, or installing the httr package.", authDownloadAdvice("bitbucket", TRUE, "internal")(), fixed = TRUE))

  # Expected auth token (GitHub) present
  expect_true(grepl("GITHUB_PAT found; check that it is correct.", authDownloadAdvice("github", TRUE, "renv")(), fixed = TRUE))

  # Expected token not found
  expect_true(grepl("BITBUCKET_USERNAME and BITBUCKET_PASSWORD environment variables not found.", authDownloadAdvice("bitbucket", FALSE, "httr")(), fixed = TRUE))
})
