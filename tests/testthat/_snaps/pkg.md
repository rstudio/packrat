# inferPackageRecord preserves fields: GitHub

    Code
      inferPackageRecord(df)
    Output
      $name
      [1] "pkg"
      
      $source
      [1] "github"
      
      $version
      [1] "0.1.0"
      
      $gh_repo
      [1] "plain_ol_pkg"
      
      $gh_username
      [1] "my-github-username"
      
      $gh_ref
      [1] "HEAD"
      
      $gh_sha1
      [1] "abc123"
      
      $remote_host
      [1] "api.github.com"
      
      $remote_repo
      [1] "plain_ol_pkg"
      
      $remote_username
      [1] "my-github-username"
      
      $remote_ref
      [1] "HEAD"
      
      $remote_sha
      [1] "abc123"
      
      attr(,"class")
      [1] "packageRecord" "github"       

# inferPackageRecord preserves fields: GitHub, pkg in subdir

    Code
      inferPackageRecord(df)
    Output
      $name
      [1] "pkginsubdir"
      
      $source
      [1] "github"
      
      $version
      [1] "0.1.0"
      
      $gh_repo
      [1] "pkg_in_subdir"
      
      $gh_username
      [1] "my-github-username"
      
      $gh_ref
      [1] "HEAD"
      
      $gh_sha1
      [1] "abc123"
      
      $gh_subdir
      [1] "pkginsubdir"
      
      $remote_host
      [1] "api.github.com"
      
      $remote_repo
      [1] "pkg_in_subdir"
      
      $remote_username
      [1] "my-github-username"
      
      $remote_ref
      [1] "HEAD"
      
      $remote_sha
      [1] "abc123"
      
      $remote_subdir
      [1] "pkginsubdir"
      
      attr(,"class")
      [1] "packageRecord" "github"       

# inferPackageRecord preserves fields: GitLab

    Code
      inferPackageRecord(df)
    Output
      $name
      [1] "pkg"
      
      $source
      [1] "gitlab"
      
      $version
      [1] "0.1.0"
      
      $remote_repo
      [1] "plain_ol_pkg"
      
      $remote_username
      [1] "my-gitlab-username"
      
      $remote_ref
      [1] "HEAD"
      
      $remote_sha
      [1] "abc123"
      
      $remote_host
      [1] "gitlab.com"
      
      attr(,"class")
      [1] "packageRecord" "gitlab"       

# inferPackageRecord preserves fields: GitLab, pkg in subdir

    Code
      inferPackageRecord(df)
    Output
      $name
      [1] "pkginsubdir"
      
      $source
      [1] "gitlab"
      
      $version
      [1] "0.1.0"
      
      $remote_repo
      [1] "pkg_in_subdir"
      
      $remote_username
      [1] "my-gitlab-username"
      
      $remote_ref
      [1] "HEAD"
      
      $remote_sha
      [1] "abc123"
      
      $remote_host
      [1] "gitlab.com"
      
      $remote_subdir
      [1] "pkginsubdir"
      
      attr(,"class")
      [1] "packageRecord" "gitlab"       

