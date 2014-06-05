getRestoreActions <- function(project = NULL) {
  project <- getProjectDir(project)
  suppressMessages(restore(project, dry.run = TRUE))
}

restoreActionMessages <- function(records) {

  if (!length(records)) {
    message("No restore actions to perform!")
    return(invisible(NULL))
  }

  pkgNames <- sapply(records$pkgRecords, "[[", "name")
  ip <- installed.packages()
  installedPkgInfo <- suppressWarnings(getInstalledPkgInfo(pkgNames, ip))

  #   pkgRecords
  #   |__ .
  #   |   |__ name
  #   |   |__ source
  #   |   |__ version
  #   |   |__ source_path
  #   |   \__ depends
  #   \__ .
  #       |__ name
  #       |__ source
  #       |__ version
  #       |__ source_path
  #       \__ depends
  #   actions
  #   repos
  #   project
  #   targetLib

  parens <- function(x) {
    paste("(", x, ")", sep = "", collapse = ", ")
  }

  n <- length(records$actions)
  msgs <- data.frame(
    package = names(records$actions),
    action = unname(records$actions),
    packrat.version = character(n),
    library.version = character(n),
    message = character(n),
    stringsAsFactors = FALSE
  )
  for (i in seq_along(records$actions)) {
    action <- records$actions[[i]]
    package <- names(records$actions)[[i]]
    record <- records$pkgRecords[sapply(records$pkgRecords, function(x) {
      x$name == package
    })][[1]]
    packrat.version <- record$version
    library.version <- installedPkgInfo[[package]][["Version"]] %||% NA
    msgs$message[[i]] <-
      switch(action,
             add = paste("Install", shQuote(package), parens(packrat.version)),
             remove = paste("Uninstall", shQuote(package), parens(packrat.version)),
             upgrade = paste("Upgrade", shQuote(package), parens(paste(library.version, "->", packrat.version))),
             downgrade = paste("Downgrade", shQuote(package), parens(paste(library.version, "->", packrat.version))),
             crossgrade = paste("Crossgrade", shQuote(package), parens(paste(library.version, "->", packrat.version))),
             stop("Unrecognized action")
      )
    msgs$packrat.version[[i]] <- packrat.version
    msgs$library.version[[i]] <- library.version
  }
  msgs
}

getRestoreActionMessages <- function(project = NULL) {
  actions <- getRestoreActions(project)
  suppressMessages(restoreActionMessages(actions))
}
