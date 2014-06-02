silent <- function(expr) {
  suppressWarnings(suppressMessages(
    capture.output(result <- eval(expr, envir = parent.frame()))
  ))
  result
}

list_files <- function(path = ".", pattern = NULL, all.files = FALSE,
                       full.names = FALSE, recursive = FALSE, ignore.case = FALSE,
                       include.dirs = FALSE, no.. = TRUE) {

  files <- list.files(path = path, pattern = pattern, all.files = all.files,
                      full.names = full.names, recursive = recursive,
                      ignore.case = ignore.case, include.dirs = include.dirs, no.. = no..)

  dirs <- list.dirs(path = path, full.names = full.names, recursive = recursive)
  setdiff(files, dirs)

}

# wrapper around read.dcf to workaround LC_CTYPE bug
# (see: http://r.789695.n4.nabble.com/Bug-in-read-dcf-all-TRUE-td4690578.html)
readDcf <- function(...) {
  loc <- Sys.getlocale('LC_CTYPE')
  on.exit(Sys.setlocale('LC_CTYPE', loc))
  read.dcf(...)
}

is_dir <- function(file) {
  isTRUE(file.info(file)$isdir) ## isTRUE guards against NA (ie, missing file)
}

# Copy a directory at file location 'from' to location 'to' -- this is kludgey,
# but file.copy does not handle copying of directories cleanly
dir_copy <- function(from, to, overwrite = FALSE, all.files = TRUE,
                     pattern = NULL, ignore.case = TRUE) {

  # Make sure we're doing sane things
  if (!is_dir(from)) stop("'", from, "' is not a directory.")

  if (file.exists(to)) {
    if (overwrite) {
      unlink(to, recursive = TRUE)
    } else {
      stop(paste( sep = "",
        if (is_dir(to)) "Directory" else "File",
        " already exists at path '", to, "'."
      ))
    }
  }

  success <- dir.create(to)
  if (!success) stop("Couldn't create directory '", to, "'.")

  # Get relative file paths
  files.relative <- list.files(from, all.files = all.files, full.names = FALSE,
                               pattern = pattern, recursive = TRUE, no.. = TRUE)

  # Get paths from and to
  files.from <- file.path(from, files.relative)
  files.to <- file.path(to, files.relative)

  # Create the directory structure
  dirnames <- unique(dirname(files.to))
  sapply(dirnames, function(x) dir.create(x, recursive = TRUE, showWarnings = FALSE))

  # Copy the files
  res <- file.copy(files.from, files.to)
  if (!all(res)) {
    # The copy failed; we should clean up after ourselves and return an error
    unlink(to, recursive = TRUE)
    stop("Could not copy all files from directory '", from, "' to directory '", to, "'.")
  }
  setNames(res, files.relative)

}

wrap <- function(x, width = 78, ...) {
  paste(strwrap(x = paste(x, collapse = " "), width = width, ...), collapse = "\n")
}

pkgDescriptionDependencies <- function(file) {

  fields <- c("Depends", "Imports", "Suggests", "LinkingTo")

  if (!file.exists(file)) stop("no file '", file, "'")
  DESCRIPTION <- readDcf("DESCRIPTION")
  requirements <- DESCRIPTION[1, fields[fields %in% colnames(DESCRIPTION)]]

  ## Remove whitespace
  requirements <- gsub("[[:space:]]*", "", requirements)

  ## Parse packages + their version
  parsed <- vector("list", length(requirements))
  for (i in seq_along(requirements)) {
    x <- requirements[[i]]
    splat <- unlist(strsplit(x, ",", fixed = TRUE))
    res <- lapply(splat, function(y) {
      if (grepl("(", y, fixed = TRUE)) {
        list(
          Package = gsub("\\(.*", "", y),
          Version = gsub(".*\\((.*?)\\)", "\\1", y, perl = TRUE),
          Field = names(requirements)[i]
        )
      } else {
        list(
          Package = y,
          Version = NA,
          Field = names(requirements)[i]
        )
      }
    })
    parsed[[i]] <- list(
      Package = sapply(res, "[[", "Package"),
      Version = sapply(res, "[[", "Version"),
      Field = sapply(res, "[[", "Field")
    )
  }

  result <- do.call(rbind, lapply(parsed, function(x) {
    as.data.frame(x, stringsAsFactors = FALSE)
  }))

  ## Don't include 'base' packages
  ip <- installed.packages()
  basePkgs <- ip[ Vectorize(isTRUE)(ip[, "Priority"] == "base"), "Package" ]
  result <- result[ !(result$Package %in% basePkgs), ]

  ## Don't include R
  result <- result[ !result$Package == "R", ]

  result

}

# does str1 start with str2?
startswith <- function(str1, str2) {
  if (!length(str2) == 1) stop("expecting a length 1 string for 'str2'")
  sapply(str1, function(x) {
    identical(
      substr(x, 1, min(nchar(x), nchar(str2))),
      str2
    )
  })
}

# does str1 end with str2?
endswith <- function(str1, str2) {
  if (!length(str2) == 1) stop ("expecting a length 1 string for 'str2'")
  n2 <- nchar(str2)
  sapply(str1, function(x) {
    nx <- nchar(x)
    identical(
      substr(x, nx - n2 + 1, nx),
      str2
    )
  })
}

stopIfNotPackified <- function(projDir) {
  if (!checkPackified(projDir, quiet = TRUE)) {
    if (identical(projDir, getwd())) {
      stop("This project has not yet been packified.\nRun 'packrat::bootstrap() to bootstrap packrat.",
           call. = FALSE)
    } else {
      stop("The project at '", projDir, "' has not yet been packified.\nRun 'packrat::bootstrap('", projDir, "') to bootstrap packrat.",
           call. = FALSE)
    }
  }
}

isGitProject <- function(projDir) {
  .git <- file.path(projDir, ".git")
  file.exists(.git) && is_dir(.git)
}

isSvnProject <- function(projDir) {
  ## TODO
  FALSE
}

updateSvnIgnore <- function(projDir) {
  ## TODO
  FALSE
}
