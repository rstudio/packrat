getRemoteInfo <- function(pkgRecord) {
  if (pkgRecord$source == "github") {
    return(
      as.data.frame(
        c(
          list(
            RemoteType     = pkgRecord$source,
            GithubRepo     = pkgRecord$gh_repo,
            GithubUsername = pkgRecord$gh_username,
            GithubRef      = pkgRecord$gh_ref,
            GithubSHA1     = pkgRecord$gh_sha1
          ),
          c(GithubSubdir   = pkgRecord$gh_subdir)
        ), stringsAsFactors = FALSE
      )
    )
  } else {
    return(
      as.data.frame(
        c(
          list(
            RemoteType     = pkgRecord$source,
            RemoteHost     = pkgRecord$remote_host,
            RemoteRepo     = pkgRecord$remote_repo,
            RemoteUsername = pkgRecord$remote_username,
            RemoteRef      = pkgRecord$remote_ref,
            RemoteSha      = pkgRecord$remote_sha
          ),
          c(RemoteSubdir = pkgRecord$remote_subdir)
        ), stringsAsFactors = FALSE
      )
    )
  }
}
