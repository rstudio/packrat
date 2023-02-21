# This code is difficult to read and has caused problems in the past. Heed my
# warning. This so that as.data.frame() is given a list. The *_subdir fields are
# missing in most cases. If they were included in the main list() calls, list()
# would include a field with that name with a NULL value.
#
# Creating a list and then concatenating the possibly-NULL subdir fields means
# that they are NULL, they will not appear at all in the resulting list at all.
# The resulting data frame is later appended to the DESCRIPTION file, so this is
# desirable.
getRemoteInfo <- function(pkgRecord) {
  if (pkgRecord$source == "github") {
    return(
      as.data.frame(as.list(c(
        RemoteType     = pkgRecord$source,
        GithubRepo     = pkgRecord$gh_repo,
        GithubUsername = pkgRecord$gh_username,
        GithubRef      = pkgRecord$gh_ref,
        GithubSHA1     = pkgRecord$gh_sha1,
        GithubSubdir = pkgRecord$gh_subdir
      )), stringsAsFactors = FALSE)
    )
  } else {
    return(
      as.data.frame(as.list(c(
        RemoteType     = pkgRecord$source,
        RemoteHost     = pkgRecord$remote_host,
        RemoteRepo     = pkgRecord$remote_repo,
        RemoteUsername = pkgRecord$remote_username,
        RemoteRef      = pkgRecord$remote_ref,
        RemoteSha      = pkgRecord$remote_sha,
        RemoteSubdir = pkgRecord$remote_subdir
      )), stringsAsFactors = FALSE)
    )
  }
}
