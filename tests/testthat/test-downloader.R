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
  destfile <- "/dev/null"
  type <- "github"

  renv_download_mock <- mockery::mock(destfile)
  mockery::stub(renvDownload, "renv$download", renv_download_mock)

  renvDownload(url, destfile, type = type)

  mockery::expect_called(renv_download_mock, 1)
  mockery::expect_args(renv_download_mock, 1, url, destfile, type)
})

test_that("authDownloadAdvice offers sound advice", {
  # Testing in the context of GITHUB_PAT being set. All providers share the same flow.
  github_pat <- Sys.getenv("GITHUB_PAT", unset = NA)
  Sys.setenv(GITHUB_PAT = "foo")
  if (is.na(github_pat)) {
    on.exit(Sys.unsetenv("GITHUB_PAT"), add = TRUE, after = TRUE)
  } else {
    on.exit(Sys.setenv(GITHUB_PAT = github_pat), add = TRUE, after = TRUE)
  }

  advice_github <- authDownloadAdvice(type = "github")

  # Using renv for downloads
  mockery::stub(advice_github, "canUseRenvDownload", TRUE)
  expect_equal(
    advice_github(),
    "If you are trying to restore a package from a private Git repo, you must have credentials available in your environment, and Packrat must be configured to use an auth-capable download method. GITHUB_PAT found; check that it is correct. Packrat is configured to use internal renv for authenticated downloads."
  )

  # Using httr for downloads
  mockery::stub(advice_github, "canUseRenvDownload", FALSE)
  mockery::stub(advice_github, "canUseHttr", TRUE)
  expect_equal(
    advice_github(),
    "If you are trying to restore a package from a private Git repo, you must have credentials available in your environment, and Packrat must be configured to use an auth-capable download method. GITHUB_PAT found; check that it is correct. Packrat will use the httr package for authenticated downloads."
  )

  # With no available auth methods
  mockery::stub(advice_github, "canUseRenvDownload", FALSE)
  mockery::stub(advice_github, "canUseHttr", FALSE)
  expect_equal(
    advice_github(),
    "If you are trying to restore a package from a private Git repo, you must have credentials available in your environment, and Packrat must be configured to use an auth-capable download method. GITHUB_PAT found; check that it is correct. Packrat is not configured to use an auth-capable download method. Try setting the option packrat.authenticated.downloads.use.renv to TRUE, or installing the httr package."
  )

  # With no token
  Sys.unsetenv("GITHUB_PAT")
  expect_equal(
    advice_github(),
    "If you are trying to restore a package from a private Git repo, you must have credentials available in your environment, and Packrat must be configured to use an auth-capable download method. GITHUB_PAT environment variable not found. Packrat is not configured to use an auth-capable download method. Try setting the option packrat.authenticated.downloads.use.renv to TRUE, or installing the httr package."
  )
})
