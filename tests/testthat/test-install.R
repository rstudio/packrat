test_that("Git service token variables are masked from subprocesses by default", {
  git_token_vars <- c(
    "GITHUB_PAT" = "secret",
    "GITLAB_PAT" = "secret",
    "BITBUCKET_USERNAME" = "secret",
    "BITBUCKET_USER" = "secret",
    "BITBUCKET_PASSWORD" = "secret",
    "BITBUCKET_PASS" = "secret",
    "GITHUB_USERNAME" = "secret",
    "GITHUB_USER" = "secret",
    "GITHUB_PASSWORD" = "secret",
    "GITHUB_PASS" = "secret",
    "GITLAB_USERNAME" = "secret",
    "GITLAB_USER" = "secret",
    "GITLAB_PASSWORD" = "secret",
    "GITLAB_PASS" = "secret"
  )
  prior_envvars <- set_envvar(git_token_vars, "replace")
  on.exit(set_envvar(prior_envvars, "replace"))

  unmask_option <- options("packrat.unmask.git.service.vars" = NULL)
  on.exit(options(unmask_option), add = TRUE, after = FALSE)

  subprocess_output <- R("-e 'Sys.getenv()'", return_output = TRUE)

  # Check to see if the supposedly_masked vars appear in the subprocess output.
  found_in_output <- sapply(git_token_vars, function(x) any(grepl(x, subprocess_output)))
  expect_false(any(found_in_output))
})

test_that("Git service token variable masking can be disabled", {
  git_token_vars <- c(
    "GITHUB_PAT" = "secret",
    "GITLAB_PAT" = "secret",
    "BITBUCKET_USERNAME" = "secret",
    "BITBUCKET_USER" = "secret",
    "BITBUCKET_PASSWORD" = "secret",
    "BITBUCKET_PASS" = "secret",
    "GITHUB_USERNAME" = "secret",
    "GITHUB_USER" = "secret",
    "GITHUB_PASSWORD" = "secret",
    "GITHUB_PASS" = "secret",
    "GITLAB_USERNAME" = "secret",
    "GITLAB_USER" = "secret",
    "GITLAB_PASSWORD" = "secret",
    "GITLAB_PASS" = "secret"
  )
  prior_envvars <- set_envvar(git_token_vars, "replace")
  on.exit(set_envvar(prior_envvars, "replace"))

  unmask_option <- options("packrat.unmask.git.service.vars" = TRUE)
  on.exit(options(unmask_option), add = TRUE, after = FALSE)

  subprocess_output <- R("-e 'Sys.getenv()'", return_output = TRUE)

  # Check to see if the supposedly_masked vars appear in the subprocess output.
  found_in_output <- sapply(git_token_vars, function(x) any(grepl(x, subprocess_output)))
  expect_true(all(found_in_output))
})

test_that("Other environment variables can be masked via the new option", {
  envvars <- c(
    "MY_SPECIAL_PAT" = "veggie_patty",
    "MEAT_EATERS_OPTION" = "beef_patty"
  )
  prior_envvars <- set_envvar(envvars, "replace")
  on.exit(set_envvar(envvars, "replace"), add = TRUE, after = FALSE)

  # First, we should check that our environment variables appear in the subprocess
  # environment when they aren't masked.
  subprocess_output <- R("-e 'Sys.getenv()'", return_output = TRUE)
  found_in_output <- sapply(envvars, function(x) any(grepl(x, subprocess_output)))
  expect_true(all(found_in_output))


  unmask_option <- options("packrat.masked.envvars" = names(envvars))
  on.exit(options(unmask_option), add = TRUE, after = FALSE)

  # Now that they are named in the option, they should be absent from this output
  subprocess_output <- R("-e 'Sys.getenv()'", return_output = TRUE)
  found_in_output <- sapply(envvars, function(x) any(grepl(x, subprocess_output)))
  expect_false(any(found_in_output))
})
