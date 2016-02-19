#' Install a local development package.
#'
#' Uses \code{R CMD INSTALL} to install the package. Will also try to install
#' dependencies of the package from CRAN, if they're not already installed.
#'
#' By default, installation takes place using the current package directory.
#' If you have compiled code, this means that artefacts of compilation will be
#' created in the \code{src/} directory. If you want to avoid this, you can
#' use \code{local = FALSE} to first build a package bundle and then install
#' it from a temporary directory. This is slower, but keeps the source
#' directory pristine.
#'
#' If the package is loaded, it will be reloaded after installation.
#'
#' @param pkg package description, can be path or package name.
#' @param reload if \code{TRUE} (the default), will automatically reload the
#'   package after installing.
#' @param quick if \code{TRUE} skips docs, multiple-architectures,
#'   demos, and vignettes, to make installation as fast as possible.
#' @param local if \code{FALSE} \code{\link{build}}s the package first:
#'   this ensures that the installation is completely clean, and prevents any
#'   binary artefacts (like \file{.o}, \code{.so}) from appearing in your local
#'   package directory, but is considerably slower, because every compile has
#'   to start from scratch.
#' @param args An optional character vector of additional command line
#'   arguments to be passed to \code{R CMD install}. This defaults to the
#'   value of the option \code{"devtools.install.args"}.
#' @param quiet if \code{TRUE} suppresses output from this function.
#' @param dependencies \code{logical} indicating to also install uninstalled
#'   packages which this \code{pkg} depends on/links to/suggests. See
#'   argument \code{dependencies} of \code{\link{install.packages}}.
#' @param build_vignettes if \code{TRUE}, will build vignettes. Normally it is
#'   \code{build} that's responsible for creating vignettes; this argument makes
#'   sure vignettes are built even if a build never happens (i.e. because
#'   \code{local = TRUE}.
#' @param keep_source If \code{TRUE} will keep the srcrefs from an installed
#'   package. This is useful for debugging (especially inside of RStudio).
#'   It defaults to the option \code{"keep.source.pkgs"}.
#' @export
#' @importFrom tools pkgVignettes
install <- function(pkg = ".", reload = TRUE, quick = FALSE, local = TRUE,
                    args = getOption("devtools.install.args"), quiet = FALSE,
                    dependencies = NA, build_vignettes = !quick,
                    keep_source = getOption("keep.source.pkgs")) {

  pkg <- as.package(pkg)

  if (!quiet) message("Installing ", pkg$package)

  # Build the package. Only build locally if it doesn't have vignettes
  has_vignettes <- length(pkgVignettes(dir = pkg$path)$doc > 0)
  if (local && !(has_vignettes && build_vignettes)) {
    built_path <- pkg$path
  } else {
    built_path <- build(pkg, tempdir(), vignettes = build_vignettes, quiet = quiet)
    on.exit(unlink(built_path))
  }

  opts <- c(
    paste("--library=", shQuote(getLibPaths()[1]), sep = ""),
    if (keep_source) "--with-keep.source",
    "--install-tests"
  )
  if (quick) {
    opts <- c(opts, "--no-docs", "--no-multiarch", "--no-demo")
  }
  opts <- paste(paste(opts, collapse = " "), paste(args, collapse = " "))

  R(paste("CMD INSTALL ", shQuote(built_path), " ", opts, sep = ""),
    quiet = quiet)

  if (reload) reload(pkg$package, quiet = quiet)

  invisible(TRUE)
}

build <- function(pkg = ".", path = NULL, binary = FALSE, vignettes = TRUE,
                  args = NULL, quiet = FALSE) {
  pkg <- as.package(pkg)
  if (is.null(path)) {
    path <- dirname(pkg$path)
  }

  if (getRversion() < "3.1.0") {
    noBuildVignettes <- "--no-vignettes"
  } else {
    noBuildVignettes <- "--no-build-vignettes"
  }

  if (binary) {
    args <- c("--build", args)
    cmd <- paste0("CMD INSTALL ", shQuote(pkg$path), " ",
                  paste0(args, collapse = " "))
    ext <- if (.Platform$OS.type == "windows") "zip" else "tgz"
  } else {
    args <- c(args, "--no-manual", "--no-resave-data")

    if (!vignettes) {
      args <- c(args, noBuildVignettes)

    } else if (!nzchar(Sys.which("pdflatex"))) {
      message("pdflatex not found. Not building PDF vignettes.")
      args <- c(args, noBuildVignettes)
    }

    cmd <- paste0("CMD build ", shQuote(pkg$path), " ",
                  paste0(args, collapse = " "))

    ext <- "tar.gz"
  }
  with_libpaths(c(tempdir(), getLibPaths()), R(cmd, path, quiet = quiet))
  targz <- paste0(pkg$package, "_", pkg$version, ".", ext)

  file.path(path, targz)
}

R.path <- function() file.path(R.home("bin"), "R")

R <- function(options, path = tempdir(), env_vars = NULL, ...) {
  options <- paste("--vanilla", options)
  r_path <- file.path(R.home("bin"), "R")

  # If rtools has been detected, add it to the path only when running R...
  if (!is.null(get_rtools_path())) {
    old <- add_path(get_rtools_path(), 0)
    on.exit(set_path(old))
  }

  in_dir(path, system_check(r_path, options, c(r_env_vars(), env_vars), ...))
}

r_env_vars <- function() {
  c("LC_ALL" = "C",
    "R_LIBS" = paste(getLibPaths(), collapse = .Platform$path.sep),
    "CYGWIN" = "nodosfilewarning",
    # When R CMD check runs tests, it sets R_TESTS. When the tests
    # themeselves run R CMD xxxx, as is the case with the tests in
    # devtools, having R_TESTS set causes errors because it confuses
    # the R subprocesses. Unsetting it here avoids those problems.
    "R_TESTS" = "",
    "NOT_CRAN" = "true",
    "TAR" = auto_tar())
}

auto_tar <- function() {
  tar <- Sys.getenv("TAR", unset = NA)
  if (!is.na(tar)) return(tar)

  windows <- .Platform$OS.type == "windows"
  no_rtools <- is.null(get_rtools_path())
  if (windows && no_rtools) "internal" else ""
}

with_something <- function(set) {
  function(new, code) {
    old <- set(new)
    on.exit(set(old))
    force(code)
  }
}

in_dir <- with_something(setwd)

set_libpaths <- function(paths) {
  old <- getLibPaths()
  setLibPaths(paths)
  invisible(old)
}

with_libpaths <- with_something(set_libpaths)

with_envvar <- function(new, code, action = "replace") {
  old <- set_envvar(new, action)
  on.exit(set_envvar(old, "replace"))
  force(code)
}

install_local_path <- function(path, subdir = NULL, ...) {
  invisible(lapply(path, install_local_path_single, subdir = subdir, ...))
}

install_local_path_single <- function(path, subdir = NULL, before_install = NULL, ..., quiet = FALSE) {
  stopifnot(file.exists(path))
  if (!quiet) {
    message("Installing package from '", path, "'")
  }

  if (!file.info(path)$isdir) {
    bundle <- path
    path <- decompress(path)
    on.exit(unlink(path), add = TRUE)
  } else {
    bundle <- NULL
  }

  pkg_path <- if (is.null(subdir)) path else file.path(path, subdir)

  # Check it's an R package
  if (!file.exists(file.path(pkg_path, "DESCRIPTION"))) {
    stop("Does not appear to be an R package (no DESCRIPTION)", call. = FALSE)
  }

  # Check configure is executable if present
  config_path <- file.path(pkg_path, "configure")
  if (file.exists(config_path)) {
    Sys.chmod(config_path, "777")
  }

  # Call before_install for bundles (if provided)
  if (!is.null(bundle) && !is.null(before_install))
    before_install(bundle, pkg_path)

  # Finally, run install
  with_build_tools({
    install(pkg_path, quiet = quiet, ...)
  })
}

with_build_tools <- function(code) {
  check <- getOption("buildtools.check", NULL)
  if (!is.null(check)) {
    if (check("Installing R packages from source")) {
      with <- getOption("buildtools.with", NULL)
      if (!is.null(with))
        with(code)
      else
        force(code)
    }
  }
  else {
    force(code)
  }
}

decompress <- function(src, target = tempdir()) {
  stopifnot(file.exists(src))

  if (grepl("\\.zip$", src)) {
    unzip(src, exdir = target, unzip = getOption("unzip"))
    outdir <- getrootdir(as.vector(unzip(src, list = TRUE)$Name))

  } else if (grepl("\\.tar$", src)) {
    untar(src, exdir = target)
    outdir <- getrootdir(untar(src, list = TRUE))

  } else if (grepl("\\.(tar\\.gz|tgz)$", src)) {
    untar(src, exdir = target, compressed = "gzip")
    outdir <- getrootdir(untar(src, compressed = "gzip", list = TRUE))

  } else if (grepl("\\.(tar\\.bz2|tbz)$", src)) {
    untar(src, exdir = target, compressed = "bzip2")
    outdir <- getrootdir(untar(src, compressed = "bzip2", list = TRUE))

  } else {
    ext <- gsub("^[^.]*\\.", "", src)
    stop("Don't know how to decompress files with extension ", ext,
         call. = FALSE)
  }

  file.path(target, outdir)
}

getdir <- function(path)  sub("/[^/]*$", "", path)

getrootdir <- function(file_list) {
  getdir(file_list[which.min(nchar(gsub("[^/]", "", file_list)))])
}

as.package <- function(x = NULL) {
  if (is.package(x)) return(x)

  x <- check_dir(x)
  load_pkg_description(x)
}

check_dir <- function(x) {
  if (is.null(x)) {
    stop("Path is null", call. = FALSE)
  }

  # Normalise path and strip trailing slashes
  x <- gsub("\\\\", "/", x, fixed = TRUE)
  x <- sub("/*$", "", x)

  if (!file.exists(x)) {
    stop("Can't find directory ", x, call. = FALSE)
  }
  if (!file.info(x)$isdir) {
    stop(x, " is not a directory", call. = FALSE)
  }

  x
}

load_pkg_description <- function(path) {
  path <- normalizePath(path)
  path_desc <- file.path(path, "DESCRIPTION")

  if (!file.exists(path_desc)) {
    stop("No description at ", path_desc, call. = FALSE)
  }

  desc <- as.list(readDcf(path_desc)[1, ])
  names(desc) <- tolower(names(desc))
  desc$path <- path

  structure(desc, class = "package")
}

is.package <- function(x) inherits(x, "package")

if (!exists("set_rtools_path")) {
  set_rtools_path <- NULL
  get_rtools_path <- NULL
  local({
    rtools_paths <- NULL
    set_rtools_path <<- function(rtools) {
      stopifnot(is.rtools(rtools))
      path <- file.path(rtools$path, version_info[[rtools$version]]$path)

      rtools_paths <<- path
    }
    get_rtools_path <<- function() {
      rtools_paths
    }
  })
}

find_rtools <- function(debug = FALSE) {
  # Non-windows users don't need rtools
  if (.Platform$OS.type != "windows") return(TRUE)

  # First try the path
  from_path <- scan_path_for_rtools(debug)
  if (is_compatible(from_path)) {
    set_rtools_path(from_path)
    return(TRUE)
  }

  if (!is.null(from_path)) {
    # Installed
    if (is.null(from_path$version)) {
      # but not from rtools
      if (debug) "gcc and ls on path, assuming set up is correct\n"
      return(TRUE)
    } else {
      # Installed, but not compatible
      message("WARNING: Rtools ", from_path$version, " found on the path",
              " at ", from_path$path, " is not compatible with R ", getRversion(), ".\n\n",
              "Please download and install ", rtools_needed(), " from ", rtools_url,
              ", remove the incompatible version from your PATH, then run find_rtools().")
      return(invisible(FALSE))
    }
  }

  # Not on path, so try registry
  registry_candidates <- scan_registry_for_rtools(debug)

  if (length(registry_candidates) == 0) {
    # Not on path or in registry, so not installled
    message("WARNING: Rtools is required to build R packages, but is not ",
            "currently installed.\n\n",
            "Please download and install ", rtools_needed(), " from ", rtools_url,
            " and then run find_rtools().")
    return(invisible(FALSE))
  }

  from_registry <- Find(is_compatible, registry_candidates, right = TRUE)
  if (is.null(from_registry)) {
    # In registry, but not compatible.
    versions <- vapply(registry_candidates, function(x) x$version, character(1))
    message("WARNING: Rtools is required to build R packages, but no version ",
            "of Rtools compatible with R ", getRversion(), " was found. ",
            "(Only the following incompatible version(s) of Rtools were found:",
            paste(versions, collapse = ","), ")\n\n",
            "Please download and install ", rtools_needed(), " from ", rtools_url,
            " and then run find_rtools().")
    return(invisible(FALSE))
  }

  installed_ver <- installed_version(from_registry$path, debug = debug)
  if (is.null(installed_ver)) {
    # Previously installed version now deleted
    message("WARNING: Rtools is required to build R packages, but the ",
            "version of Rtools previously installed in ", from_registry$path,
            " has been deleted.\n\n",
            "Please download and install ", rtools_needed(), " from ", rtools_url,
            " and then run find_rtools().")
    return(invisible(FALSE))
  }

  if (installed_ver != from_registry$version) {
    # Installed version doesn't match registry version
    message("WARNING: Rtools is required to build R packages, but no version ",
            "of Rtools compatible with R ", getRversion(), " was found. ",
            "Rtools ", from_registry$version, " was previously installed in ",
            from_registry$path, " but now that directory contains Rtools ",
            installed_ver, ".\n\n",
            "Please download and install ", rtools_needed(), " from ", rtools_url,
            " and then run find_rtools().")
    return(invisible(FALSE))
  }

  # Otherwise it must be ok :)
  set_rtools_path(from_registry)
  TRUE
}

scan_path_for_rtools <- function(debug = FALSE) {
  if (debug) cat("Scanning path...\n")

  # First look for ls and gcc
  ls_path <- Sys.which("ls")
  if (ls_path == "") return(NULL)
  if (debug) cat("ls :", ls_path, "\n")

  gcc_path <- Sys.which("gcc")
  if (gcc_path == "") return(NULL)
  if (debug) cat("gcc:", gcc_path, "\n")

  # We have a candidate installPath
  install_path <- dirname(dirname(ls_path))
  install_path2 <- dirname(dirname(dirname(gcc_path)))
  if (install_path2 != install_path) return(NULL)

  version <- installed_version(install_path, debug = debug)
  if (debug) cat("Version:", version, "\n")

  rtools(install_path, version)
}

scan_registry_for_rtools <- function(debug = FALSE) {
  if (debug) cat("Scanning registry...\n")

  keys <- NULL
  try(keys <- utils::readRegistry("SOFTWARE\\R-core\\Rtools",
                                  hive = "HLM", view = "32-bit", maxdepth = 2), silent = TRUE)
  if (is.null(keys)) return(NULL)

  rts <- vector("list", length(keys))

  for (i in seq_along(keys)) {
    version <- names(keys)[[i]]
    key <- keys[[version]]
    if (!is.list(key) || is.null(key$InstallPath)) next;
    install_path <- normalizePath(key$InstallPath,
                                  mustWork = FALSE, winslash = "/")

    if (debug) cat("Found", install_path, "for", version, "\n")
    rts[[i]] <- rtools(install_path, version)
  }

  Filter(Negate(is.null), rts)
}

installed_version <- function(path, debug) {
  if (!file.exists(file.path(path, "Rtools.txt"))) return(NULL)

  # Find the version path
  version_path <- file.path(path, "VERSION.txt")
  if (debug) {
    cat("VERSION.txt\n")
    cat(readLines(version_path), "\n")
  }
  if (!file.exists(version_path)) return(NULL)

  # Rtools is in the path -- now crack the VERSION file
  contents <- NULL
  try(contents <- readLines(version_path), silent = TRUE)
  if (is.null(contents)) return(NULL)

  # Extract the version
  contents <- gsub("^\\s+|\\s+$", "", contents)
  version_re <- "Rtools version (\\d\\.\\d+)\\.[0-9.]+$"

  if (!grepl(version_re, contents)) return(NULL)

  m <- regexec(version_re, contents)
  regmatches(contents, m)[[1]][2]
}

is_compatible <- function(rtools) {
  if (is.null(rtools)) return(FALSE)
  if (is.null(rtools$version)) return(FALSE)

  stopifnot(is.rtools(rtools))
  info <- version_info[[rtools$version]]
  if (is.null(info)) return(FALSE)

  r_version <- getRversion()
  r_version >= info$version_min && r_version <= info$version_max
}

rtools <- function(path, version) {
  structure(list(version = version, path = path), class = "rtools")
}
is.rtools <- function(x) inherits(x, "rtools")

rtools_url <- "http://cran.r-project.org/bin/windows/Rtools/"
version_info <- list(
  "2.11" = list(
    version_min = "2.10.0",
    version_max = "2.11.1",
    path = c("bin", "perl/bin", "MinGW/bin")
  ),
  "2.12" = list(
    version_min = "2.12.0",
    version_max = "2.12.2",
    path = c("bin", "perl/bin", "MinGW/bin", "MinGW64/bin")
  ),
  "2.13" = list(
    version_min = "2.13.0",
    version_max = "2.13.2",
    path = c("bin", "MinGW/bin", "MinGW64/bin")
  ),
  "2.14" = list(
    version_min = "2.13.0",
    version_max = "2.14.2",
    path = c("bin", "MinGW/bin", "MinGW64/bin")
  ),
  "2.15" = list(
    version_min = "2.14.2",
    version_max = "2.15.1",
    path = c("bin", "gcc-4.6.3/bin")
  ),
  "2.16" = list(
    version_min = "2.15.2",
    version_max = "3.0.0",
    path = c("bin", "gcc-4.6.3/bin")
  ),
  "3.0" = list(
    version_min = "2.15.2",
    version_max = "3.0.99",
    path = c("bin", "gcc-4.6.3/bin")
  ),
  "3.1" = list(
    version_min = "3.0.0",
    version_max = "3.1.99",
    path = c("bin", "gcc-4.6.3/bin")
  )
)

rtools_needed <- function() {
  r_version <- getRversion()

  for (i in rev(seq_along(version_info))) {
    version <- names(version_info)[i]
    info <- version_info[[i]]
    ok <- r_version >= info$version_min && r_version <= info$version_max
    if (ok) return(paste("Rtools", version))
  }
  "the appropriate version of Rtools"
}

system_check <- function(cmd, args = character(), env = character(),
                         quiet = FALSE, ...) {
  full <- paste(shQuote(cmd), paste(args, collapse = ", "))

  if (!quiet) {
    message(wrap_command(full))
    message()
  }

  # Use system2 instead of system as we can then handle redirection
  # on Windows
  result <- suppressWarnings(with_envvar(
    env,
    if (quiet) {
      system2(cmd, args, stdout = TRUE, stderr = TRUE)
    } else {
      system2(cmd, args)
    }
  ))

  status <- attr(result, "status")
  if (!is.null(status) && status != 0) {

    stopMsg <- paste0(
      "Command failed (", status, ")",
      "\n\nFailed to run system command:\n\n",
      "\t", full
    )

    if (length(result)) {
      stopMsg <- paste0(
        stopMsg,
        "\n\nThe command failed with output:\n",
        paste(result, collapse = "\n")
      )
    }

    # issue #186
    if (nchar(stopMsg) > getOption("warning.length")) {
      print(stopMsg, file = stderr())
    }
    stop(stopMsg, call. = FALSE)
  }

  invisible(TRUE)
}


wrap_command <- function(x) {
  lines <- strwrap(x, getOption("width") - 2, exdent = 2)
  continue <- c(rep(" \\", length(lines) - 1), "")
  paste(lines, continue, collapse = "\n")
}

set_envvar <- function(envs, action = "replace") {
  stopifnot(is.named(envs))
  stopifnot(is.character(action), length(action) == 1)
  action <- match.arg(action, c("replace", "prefix", "suffix"))

  old <- Sys.getenv(names(envs), names = TRUE, unset = NA)
  set <- !is.na(envs)

  both_set <- set & !is.na(old)
  if (any(both_set)) {
    if (action == "prefix") {
      envs[both_set] <- paste(envs[both_set], old[both_set])
    } else if (action == "suffix") {
      envs[both_set] <- paste(old[both_set], envs[both_set])
    }
  }

  if (any(set))  do.call("Sys.setenv", as.list(envs[set]))
  if (any(!set)) Sys.unsetenv(names(envs)[!set])

  invisible(old)
}

is.named <- function(x) {
  !is.null(names(x)) && all(names(x) != "")
}

"%||%" <- function(a, b) if (!is.null(a)) a else b

get_path <- function() {
  strsplit(Sys.getenv("PATH"), .Platform$path.sep)[[1]]
}

set_path <- function(path) {
  path <- normalizePath(path, mustWork = FALSE)

  old <- get_path()
  path <- paste(path, collapse = .Platform$path.sep)
  Sys.setenv(PATH = path)
  invisible(old)
}

add_path <- function(path, after = Inf) {
  set_path(append(get_path(), path, after))
}

reload <- function(pkg = ".", quiet = FALSE) {
  if (paste0("package:", pkg) %in% search()) {
    if (!quiet) message("Reloading installed package: \", pkg, \"")
    forceUnload(pkg)
    library(pkg, character.only = TRUE, quietly = TRUE)
  }
}
