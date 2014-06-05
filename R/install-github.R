#' Attempts to install a package directly from github.
#'
#' This function is vectorised on \code{repo} so you can install multiple
#' packages in a single command.
#'
#' @param repo Repository address in the format
#'   \code{[username/]repo[/subdir][@@ref|#pull]}.
#' @param auth_user Your account username if you're attempting to install
#'   a package hosted in a private repository (and your username is different
#'   to \code{username}). Currently disabled.
#' @param auth_token To install from a private repo, generate a personal
#'   access token (PAT) in \url{https://github.com/settings/applications} and
#'   supply to this argument. This is safer than using a password because
#'   you can easily delete a PAT without affecting any others. Defaults to
#'   the \code{GITHUB_PAT} environment variable. Currently disabled.
#' @param ... Other arguments passed on to \code{install}.
#' @param dependencies By default, installs all dependencies so that you can
#'   build vignettes and use all functionality of the package.
#' @export
#' @family package installation
#' @examples
#' \dontrun{
#' install_github("roxygen")
#' install_github("wch/ggplot2")
#' install_github(c("rstudio/httpuv", "rstudio/shiny"))
#' install_github(c("devtools@@devtools-1.4", "klutometis/roxygen#142", "mfrasca/r-logging/pkg))
#'
#' # Update devtools to the latest version, on Linux and Mac
#' # On Windows, this won't work - see ?build_github_devtools
#' install_github("hadley/devtools")
#'
#' # To install from a private repo, use auth_token with a token
#' # from https://github.com/settings/applications. You only need the
#' # repo scope. Best practice is to save your PAT in env var called
#' # GITHUB_PAT.
#' install_github("hadley/private", auth_token = "abc")
#'
#' }
install_github <- function(repo,
                           auth_user = NULL,
                           auth_token = github_pat(), ...,
                           dependencies = TRUE) {

  invisible(vapply(repo, install_github_single, FUN.VALUE = logical(1),
    auth_user, auth_token, ...,
    dependencies = dependencies))
}

github_get_conn <- function(repo,
                            auth_user = NULL,
                            auth_token = NULL, ...) {

  params <- github_parse_path(repo)
  username <- params$username
  if (is.null(username)) username <- "hadley"
  repo <- params$repo
  ref <- params$ref
  if (is.null(ref)) ref <- "master"
  pull <- params$pull
  subdir <- params$subdir

  if (!is.null(pull)) {
    pullinfo <- github_pull_info(repo, username, pull)
    username <- pullinfo$username
    ref <- pullinfo$ref
  }

  if (!is.null(auth_token)) {
    auth <- httr::authenticate(
      user = auth_token,
      password = "x-oauth-basic",
      type = "basic"
    )
  } else {
    auth <- list()
  }

  msg <- paste0("Installing github repo ",
    paste(repo, ref, sep = "/", collapse = ", "),
    " from ",
    paste(username, collapse = ", "))

  url <- paste("https://github.com/", username, "/", repo,
    "/archive/", ref, ".zip", sep = "")

  list(
    url = url, auth = auth, msg = msg, repo = repo, username = username,
    ref = ref, pull = pull, subdir = subdir,
    auth_user = auth_user
  )
}

install_github_single <- function(repo,
                                  auth_user = NULL,
                                  auth_token = NULL, ...) {

  conn <- github_get_conn(repo, auth_user, auth_token, ...)
  message(conn$msg)

  # define before_install function that captures the arguments to
  # install_github and appends the to the description file
  github_before_install <- function(bundle, pkg_path) {

    desc <- file.path(pkg_path, "DESCRIPTION")

    # Remove any blank lines from DESCRIPTION -- this protects users from
    # 'Error: contains a blank line' errors thrown by R CMD INSTALL
    DESCRIPTION <- readLines(desc, warn = FALSE)
    if (any(DESCRIPTION == "")) {
      DESCRIPTION <- DESCRIPTION[DESCRIPTION != ""]
    }
    cat(DESCRIPTION, file = desc, sep = "\n")

    # Function to append a field to the DESCRIPTION if it's not null
    append_field <- function(name, value) {
      if (!is.null(value)) {
        cat("Github", name, ":", value, "\n", sep = "", file = desc, append = TRUE)
      }
    }

    # Append fields
    append_field("Repo", conn$repo)
    append_field("Username", conn$username)
    append_field("Ref", conn$ref)
    append_field("SHA1", github_extract_sha1(bundle))
    append_field("Pull", conn$pull)
    append_field("Subdir", conn$subdir)
    append_field("AuthUser", conn$auth_user)

  }

  # If there are slashes in the ref, the URL will have extra slashes, but the
  # downloaded file shouldn't have them.
  # install_github("shiny", "rstudio", "v/0/2/1")
  #  URL: https://github.com/rstudio/shiny/archive/v/0/2/1.zip
  #  Output file: shiny.zip
  install_url(conn$url, subdir = conn$subdir,
    config = conn$auth, before_install = github_before_install, ...)
}

# Retrieve the username and ref for a pull request
## E.g., given repo = 'Kmisc', username = 'kevinushey', and
## pull = '7', we should get
## repo == 'Kmisc'
## username = 'kevinushey'
## ref = 'test-pull-request'
github_pull_info <- function(repo, username, pull, method = "wget") {

  ## Strip out the ref name by downloading the URL and 'parsing' the
  ## HTML with regular expressions. (CAUTION: may summon Cthulhu)
  URL <- file.path("https://github.com", username, repo, "pull", pull, fsep = "/")
  tempfile <- tempfile()
  download.file(URL, destfile = tempfile, method = method)
  content <- readLines(tempfile)
  unlink(tempfile)

  rx <- paste(repo, "branch", sep = ":")
  line <- grep(rx, content, value = TRUE)[[1]]
  ref <- gsub("\".*", "", gsub(paste0(".*", rx, ":"), "", line))
  list(repo = repo, username = username, ref = ref)

}

# Extract the commit hash from a github bundle and append it to the
# package DESCRIPTION file. Git archives include the SHA1 hash as the
# comment field of the zip central directory record
# (see https://www.kernel.org/pub/software/scm/git/docs/git-archive.html)
# Since we know it's 40 characters long we seek that many bytes minus 2
# (to confirm the comment is exactly 40 bytes long)
github_extract_sha1 <- function(bundle) {

  # open the bundle for reading
  conn <- file(bundle, open = "rb", raw = TRUE)
  on.exit(close(conn))

  # seek to where the comment length field should be recorded
  seek(conn, where = -0x2a, origin = "end")

  # verify the comment is length 0x28
  len <- readBin(conn, "raw", n = 2)
  if (len[1] == 0x28 && len[2] == 0x00) {
    # read and return the SHA1
    rawToChar(readBin(conn, "raw", n = 0x28))
  } else {
    NULL
  }
}

# Parse a GitHub path of the form [username/]repo[/subdir][#pull|@ref]
github_parse_path <- function(path) {
  username_rx <- "(?:([^/]+)/)?"
  repo_rx <- "([^/@#]+)"
  subdir_rx <- "(?:/([^@#]*[^@#/]))?"
  ref_rx <- "(?:@(.+))"
  pull_rx <- "(?:#([0-9]+))"
  ref_or_pull_rx <- sprintf("(?:%s|%s)?", ref_rx, pull_rx)
  github_rx <- sprintf("^(?:%s%s%s%s|(.*))$",
    username_rx, repo_rx, subdir_rx, ref_or_pull_rx)

  params <- c("username", "repo", "subdir", "ref", "pull", "invalid")
  replace <- setNames(sprintf("\\%d", seq_along(params)), params)
  ret <- lapply(replace, function(r) gsub(github_rx, r, path, perl = TRUE))
  if (ret$invalid != "")
    stop(sprintf("Invalid GitHub path: %s", path))
  ret[sapply(ret, nchar) > 0]
}
