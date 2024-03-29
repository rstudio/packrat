enumerate <- function(x, f, ...) {
  result <- vector("list", length(x))
  for (i in seq_along(x)) {
    result[[i]] <- f(x[[i]], ...)
  }
  names(result) <- names(x)
  result
}

githubUpdates <- function(lib.loc = .libPaths()) {

  do.call(rbind, enumerate(lib.loc, function(lib) {
    pkgs <- list.files(lib, full.names = TRUE)
    DESCRIPTIONS <- enumerate(pkgs, function(pkg) {
      path <- file.path(pkg, "DESCRIPTION")
      if (!file.exists(path)) return(NULL)
      readDcf(path)
    })
    names(DESCRIPTIONS) <- pkgs
    DESCRIPTIONS <-
      Filter(function(x) any(grepl("^Github", colnames(x))), DESCRIPTIONS)
    if (!length(DESCRIPTIONS)) return(NULL)
    if (!requireNamespace("httr")) stop("Need package 'httr' to check for GitHub updates")
    do.call(rbind, enumerate(DESCRIPTIONS, function(x) {
      url <- file.path("https://api.github.com",
                       "repos",
                       x[, "GithubUsername"],
                       x[, "GithubRepo"],
                       "branches")
      response <- httr::GET(url)
      status <- response$status
      if (response$status == 403) {
        warning("rejected by server", call. = FALSE)
        sha1 <- NA
      } else if (!response$status == 200) {
        warning("failed to get tracking information for GitHub package '",
                x[, "Package"],
                "'; did its associated repository move?",
                call. = FALSE)
        sha1 <- NA
      } else {
        content <- httr::content(response, "parsed")
        ## Find the index of the response with the appropriate name
        index <- which(sapply(content, `[[`, "name") == x[, "GithubRef"])
        if (!length(index)) {
          warning("no reference '", x[, "GithubRef"],
                  "' found associated with this repository; was the branch deleted?",
                  call. = FALSE)
          sha1 <- NA
        } else {
          sha1 <- content[[index]]$commit$sha
        }
      }

      data.frame(
        stringsAsFactors = FALSE,
        Package = unname(x[, "Package"]),
        LibPath = lib,
        Installed = unname(x[, "GithubSHA1"]),
        Built = gsub(";.*", "", x[, "Built"]),
        ReposVer = sha1,
        Repository = file.path("https://github.com",
                               x[, "GithubUsername"],
                               x[, "GithubRepo"],
                               "tree",
                               x[, "GithubRef"])
      )
      # Gross dependency-free version
      #       dest <- tempfile()
      #       status <- download(url, destfile = dest, quiet = TRUE)
      #       content <- readLines(dest)
      #       sha1_scraped <- grep("Copy SHA", content, fixed = TRUE, value = TRUE)
      #       sha1 <- gsub("^(.*?)data-clipboard-text=\"(.*?)\"(.*?)$", "\\2", sha1_scraped, perl = TRUE)
    }))
  }))
}


bitbucketUpdates <- function(lib.loc = .libPaths()) {

  do.call(rbind, enumerate(lib.loc, function(lib) {
    pkgs <- list.files(lib, full.names = TRUE)
    DESCRIPTIONS <- enumerate(pkgs, function(pkg) {
      path <- file.path(pkg, "DESCRIPTION")
      if (!file.exists(path)) return(NULL)
      readDcf(path)
    })
    names(DESCRIPTIONS) <- pkgs
    DESCRIPTIONS <-
      Filter(function(x) "RemoteType" %in% colnames(x) && x[, "RemoteType"] == "bitbucket", DESCRIPTIONS)
    if (!length(DESCRIPTIONS)) return(NULL)
    if (!requireNamespace("httr")) stop("Need package 'httr' to check for Bitbucket updates")
    do.call(rbind, enumerate(DESCRIPTIONS, function(x) {
      url <- file.path("https://api.bitbucket.org",
                       "2.0",
                       "repositories",
                       x[, "RemoteUsername"],
                       x[, "RemoteRepo"],
                       "refs",
                       "branches")
      response <- httr::GET(url)
      status <- response$status
      if (response$status == 403) {
        warning("rejected by server", call. = FALSE)
        sha <- NA
      } else if (!response$status == 200) {
        warning("failed to get tracking information for Bitbucket package '",
                x[, "Package"],
                "'; did its associated repository move?",
                call. = FALSE)
        sha <- NA
      } else {
        content <- httr::content(response, "parsed")
        ## Find the index of the response with the appropriate name
        index <- which(sapply(content$values, `[[`, "name") == x[, "RemoteRef"])
        if (!length(index)) {
          warning("no reference '", x[, "RemoteRef"],
                  "' found associated with this repository; was the branch deleted?",
                  call. = FALSE)
          sha <- NA
        } else {
          sha <- content$values[[index]]$target$hash
        }
      }

      data.frame(
        stringsAsFactors = FALSE,
        Package = unname(x[, "Package"]),
        LibPath = lib,
        Installed = unname(x[, "RemoteSha"]),
        Built = gsub(";.*", "", x[, "Built"]),
        ReposVer = sha,
        Repository = file.path("https://bitbucket.org",
                               x[, "RemoteUsername"],
                               x[, "RemoteRepo"],
                               "src",
                               x[, "RemoteRef"])
      )
    }))
  }))
}

gitlabUpdates <- function(lib.loc = .libPaths()) {

  do.call(rbind, enumerate(lib.loc, function(lib) {
    pkgs <- list.files(lib, full.names = TRUE)
    DESCRIPTIONS <- enumerate(pkgs, function(pkg) {
      path <- file.path(pkg, "DESCRIPTION")
      if (!file.exists(path)) return(NULL)
      readDcf(path)
    })
    names(DESCRIPTIONS) <- pkgs
    DESCRIPTIONS <-
      Filter(function(x) "RemoteType" %in% colnames(x) &&
               x[, "RemoteType"] == "gitlab", DESCRIPTIONS)
    if (!length(DESCRIPTIONS)) return(NULL)
    if (!requireNamespace("httr")) stop("Need package 'httr' to check for Gitlab updates")
    do.call(rbind, enumerate(DESCRIPTIONS, function(x) {
      url <- file.path("https://gitlab.com/",
                       "api/v4/projects/",
                       URLencode(paste0(x[, "RemoteUsername"],
                              "/",
                              x[, "RemoteRepo"]), reserved = TRUE),
                       "repository",
                       "archive.tar.gz")
      response <- httr::GET(url)
      status <- response$status
      if (response$status == 403) {
        warning("rejected by server", call. = FALSE)
        sha1 <- NA
      } else if (!response$status == 200) {
        warning("failed to get tracking information for Gitlab package '",
                x[, "Package"],
                "'; did its associated repository move?",
                call. = FALSE)
        sha1 <- NA
      } else {
        content <- httr::content(response, "parsed")
        ## Find the index of the response with the appropriate name
        index <- which(sapply(content, `[[`, "name") == x[, "RemoteRef"])
        if (!length(index)) {
          warning("no reference '", x[, "RemoteRef"],
                  "' found associated with this repository; was the branch deleted?",
                  call. = FALSE)
          sha1 <- NA
        } else {
          sha1 <- content[[index]]$commit$sha
        }
      }

      data.frame(
        stringsAsFactors = FALSE,
        Package = unname(x[, "Package"]),
        LibPath = lib,
        Installed = unname(x[, "RemoteSha"]),
        Built = gsub(";.*", "", x[, "Built"]),
        ReposVer = sha1,
        Repository = file.path("https://gitlab.com",
                               x[, "RemoteUsername"],
                               x[, "RemoteRepo"],
                               "src",
                               x[, "RemoteRef"])
      )
    }))
  }))
}

available_updates <- function() {
  cranUpdates <- as.data.frame(old.packages(), stringsAsFactors = FALSE)
  githubUpdates <- githubUpdates()
  bitbucketUpdates <- bitbucketUpdates()
  gitlabUpdates <- gitlabUpdates()

  list(
    CRAN = cranUpdates,
    GitHub = githubUpdates,
    Bitbucket = bitbucketUpdates,
    GitLab = gitlabUpdates
  )
}
