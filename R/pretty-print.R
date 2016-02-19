## Pretty printers, primarily used for status output
prettyPrint <- function(packages, header, footer = NULL) {
  if (length(packages) > 0) {
    cat('\n')
    if (!is.null(header)) {
      cat(paste(header, collapse = ''))
      cat('\n')
    }
    print.simple.list(lapply(packages, function(pkg) {
      result <- ifelse(is.na(pkg$version), '', pkg$version)
      result <- paste(" ", result)
      names(result) <- paste("   ", pkg$name)
      result
    }))
    if (!is.null(footer)) {
      cat('\n')
      cat(paste(footer, collapse = ''))
    }
    cat('\n')
  }
}

summarizeDiffs <- function(diffs, pkgsA, pkgsB, addMessage,
                           removeMessage, upgradeMessage, downgradeMessage,
                           crossgradeMessage)
{
  prettyPrint(
    searchPackages(pkgsB, names(diffs)[!is.na(diffs) & diffs == 'add']),
    addMessage
  )
  prettyPrint(
    searchPackages(pkgsA, names(diffs)[!is.na(diffs) & diffs == 'remove']),
    removeMessage
  )
  prettyPrintPair(
    searchPackages(pkgsA, names(diffs)[!is.na(diffs) & diffs == 'upgrade']),
    searchPackages(pkgsB, names(diffs)[!is.na(diffs) & diffs == 'upgrade']),
    upgradeMessage
  )
  prettyPrintPair(
    searchPackages(pkgsA, names(diffs)[!is.na(diffs) & diffs == 'downgrade']),
    searchPackages(pkgsB, names(diffs)[!is.na(diffs) & diffs == 'downgrade']),
    downgradeMessage
  )
  prettyPrintPair(
    searchPackages(pkgsA, names(diffs)[!is.na(diffs) & diffs == 'crossgrade']),
    searchPackages(pkgsB, names(diffs)[!is.na(diffs) & diffs == 'crossgrade']),
    crossgradeMessage
  )
}

prettyPrintPair <- function(packagesFrom, packagesTo, header, footer = NULL,
                            fromLabel = 'from', toLabel = 'to') {

  if (length(packagesFrom) != length(packagesTo)) {
    stop('Invalid arguments--package record lengths mismatch')
  }

  if (length(packagesFrom) > 0) {
    if (any(sapply(packagesFrom, is.null) & sapply(packagesTo, is.null))) {
      stop('Invalid arguments--NULL packages')
    }
    for (i in seq_along(packagesFrom)) {
      if (!is.null(packagesFrom[[i]]) && !is.null(packagesTo[[i]])) {
        if (!identical(packagesFrom[[i]]$name , packagesTo[[i]]$name)) {
          stop('Invalid arguments--package names did not match')
        }
      }
    }

    cat('\n')
    if (!is.null(header)) {
      cat(paste(header, collapse = ''))
      cat('\n')
    }

    pickVersion <- pick("version", defaultValue = "NA")
    df <- data.frame(paste(" ", sapply(packagesFrom, pickVersion)),
                     paste(" ", sapply(packagesTo, pickVersion)))
    names(df) <- c(paste(" ", fromLabel), paste(" ", toLabel))
    row.names(df) <- paste("   ", pkgNames(packagesFrom))
    print(df)

    if (!is.null(footer)) {
      cat('\n')
      cat(paste(footer, collapse = ''))
    }
    cat('\n')
  }
}

prettyPrintNames <- function(packageNames, header, footer = NULL) {
  if (length(packageNames) > 0) {
    cat('\n')
    if (!is.null(header)) {
      cat(paste(header, collapse = ''))
      cat('\n')
    }
    cat(paste("    ", packageNames, sep = '', collapse = '\n'))
    cat('\n')
    if (!is.null(footer)) {
      cat(paste(footer, collapse = ''))
    }
    cat('\n')
  }
}
