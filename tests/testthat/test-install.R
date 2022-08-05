test_that("Git service token variables are masked from subprocesses by default", {
  prior_envvars <- Sys.getenv(c(
    "GITHUB_PAT",
    "GITLAB_PAT",
    "BITBUCKET_USERNAME",
    "BITBUCKET_USER",
    "BITBUCKET_PASSWORD",
    "BITBUCKET_PASS",
    "GITHUB_USERNAME",
    "GITHUB_USER",
    "GITHUB_PASSWORD",
    "GITHUB_PASS",
    "GITLAB_USERNAME",
    "GITLAB_USER",
    "GITLAB_PASSWORD",
    "GITLAB_PASS"
  ), unset = NA)
  for (varname in names(prior_envvars)) {
    if (is.na(prior_envvars[varname])) {
      on.exit(Sys.unsetenv(varname), add = TRUE, after = FALSE)
    } else {
      on.exit(Sys.setenv(varname = prior_envvars[varname]), add = TRUE, after = FALSE)
    }
  }

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
  set_envvar(git_token_vars)

  unmask_option <- options("packrat.unmask.git.service.vars" = NULL)
  on.exit(options(unmask_option), add = TRUE, after = FALSE)

  subprocess_output <- R("-e 'Sys.getenv()'", return_output = TRUE)

  # Check to see if the supposedly_masked vars appear in the subprocess output.
  found_in_output <- sapply(git_token_vars, function(x) any(grepl(x, subprocess_output)))
  expect_false(any(found_in_output))
})

test_that("Git service token variable masking can be disabled", {
  prior_envvars <- Sys.getenv(c(
    "GITHUB_PAT",
    "GITLAB_PAT",
    "BITBUCKET_USERNAME",
    "BITBUCKET_USER",
    "BITBUCKET_PASSWORD",
    "BITBUCKET_PASS",
    "GITHUB_USERNAME",
    "GITHUB_USER",
    "GITHUB_PASSWORD",
    "GITHUB_PASS",
    "GITLAB_USERNAME",
    "GITLAB_USER",
    "GITLAB_PASSWORD",
    "GITLAB_PASS"
  ), unset = NA)
  for (varname in names(prior_envvars)) {
    if (is.na(prior_envvars[varname])) {
      on.exit(Sys.unsetenv(varname), add = TRUE, after = FALSE)
    } else {
      on.exit(Sys.setenv(varname = prior_envvars[varname]), add = TRUE, after = FALSE)
    }
  }

  git_token_vars <- c(
    "GITHUB_PAT" = "secret",
    "GITLAB_PAT" = "secret",
    "BITBUCKET_USERNAME" = "secret",
    "BITBUCKET_USER" = "secret",
    "BITBUCKET_PASSWORD" = "secret",
    "BITBUCKET_PASS" = "secret",
    # Varnames that may have been used previously
    "GITHUB_USERNAME" = "secret",
    "GITHUB_USER" = "secret",
    "GITHUB_PASSWORD" = "secret",
    "GITHUB_PASS" = "secret",
    "GITLAB_USERNAME" = "secret",
    "GITLAB_USER" = "secret",
    "GITLAB_PASSWORD" = "secret",
    "GITLAB_PASS" = "secret"
  )
  set_envvar(git_token_vars)

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

  prior_envvars <- Sys.getenv(names(envvars), unset = NA)
  for (varname in names(prior_envvars)) {
    if (is.na(prior_envvars[varname])) {
      on.exit(Sys.unsetenv(varname), add = TRUE, after = FALSE)
    } else {
      on.exit(Sys.setenv(varname = prior_envvars[varname]), add = TRUE, after = FALSE)
    }
  }
  set_envvar(envvars)

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
