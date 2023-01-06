test_that("The default list of environment variables is masked correctly", {
    # We won't check GITHUB_PAT because it's always present in process on CI.
  new_envvars <- c(
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
  prior_envvars <- set_envvar(new_envvars, "replace")
  on.exit(set_envvar(prior_envvars, "replace"))

  git_mask_option <- options("packrat.mask.git.service.envvars" = NULL)
  on.exit(options(git_mask_option), add = TRUE, after = FALSE)

  subprocess_output <- R('-e "Sys.getenv()"', return_output = TRUE)

  # Check to see if the masked envvar names appear in the subprocess output.
  found_in_output <- sapply(names(new_envvars), function(x) any(grepl(x, subprocess_output)))

  # Read `any(found_in_output)` as "Are any of the outputs TRUE?"
  expect_false(any(found_in_output), info = print(subprocess_output))
})

test_that("The default list of masked environment variables can be disabled", {
  new_envvars <- c(
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
  prior_envvars <- set_envvar(new_envvars, "replace")
  on.exit(set_envvar(prior_envvars, "replace"))

  git_mask_option <- options("packrat.mask.git.service.envvars" = FALSE)
  on.exit(options(git_mask_option), add = TRUE, after = FALSE)

  subprocess_output <- R('-e "Sys.getenv()"', return_output = TRUE)

  # Check to see if the masked envvar names appear in the subprocess output.
  found_in_output <- sapply(names(new_envvars), function(x) any(grepl(x, subprocess_output)))
  expect_true(all(found_in_output), info = print(subprocess_output))
})

test_that("Environment variables appear in an R subprocess", {
  new_envvars <- c(
    "PLANT_BASED" = "veggie_patty",
    "MEAT_EATERS_OPTION" = "beef_patty"
  )
  prior_envvars <- set_envvar(new_envvars, "replace")
  on.exit(set_envvar(new_envvars, "replace"), add = TRUE, after = FALSE)

  user_mask <- options("packrat.masked.envvars" = NULL)
  on.exit(options(user_mask), add = TRUE, after = FALSE)

  subprocess_output <- R('-e "Sys.getenv()"', return_output = TRUE)

  found_in_output <- sapply(names(new_envvars), function(x) any(grepl(x, subprocess_output)))
  expect_true(all(found_in_output), info = print(subprocess_output))
})

test_that("User-specified masked envvars do not appear in an R subprocess", {
  new_envvars <- c(
    "PLANT_BASED" = "veggie_patty",
    "MEAT_EATERS_OPTION" = "beef_patty"
  )
  prior_envvars <- set_envvar(new_envvars, "replace")
  on.exit(set_envvar(new_envvars, "replace"), add = TRUE, after = FALSE)

  user_mask <- options("packrat.masked.envvars" = names(new_envvars))
  on.exit(options(user_mask), add = TRUE, after = FALSE)

  subprocess_output <- R('-e "Sys.getenv()"', return_output = TRUE)

  found_in_output <- sapply(names(new_envvars), function(x) any(grepl(x, subprocess_output)))

  # Read `any(found_in_output)` as "Are any of the outputs TRUE?"
  expect_false(any(found_in_output), info = print(subprocess_output))
})

test_that("Git and user-specified variables can be masked while other variables still appear", {
  new_envvars <- c(
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
    "GITLAB_PASS" = "secret",
    "PLANT_BASED" = "veggie_patty",
    "MEAT_EATERS_OPTION" = "beef_patty"
  )
  prior_envvars <- set_envvar(new_envvars, "replace")
  on.exit(set_envvar(prior_envvars, "replace"), add = TRUE, after = FALSE)


  git_mask_option <- options("packrat.mask.git.service.envvars" = NULL)
  on.exit(options(git_mask_option), add = TRUE, after = FALSE)

  user_mask_option <- options("packrat.masked.envvars" = "PLANT_BASED")
  on.exit(options(user_mask_option), add = TRUE, after = FALSE)

  # We're expecting everything but the last item to be masked.
  masked_names <- names(new_envvars)[1:length(new_envvars) - 1]
  unmasked_name <- names(new_envvars)[length(new_envvars)]

  subprocess_output <- R('-e "Sys.getenv()"', return_output = TRUE)

  # Check masked vars
  not_expected <- sapply(masked_names, function(x) any(grepl(x, subprocess_output)))
  expect_false(any(not_expected), info = print(subprocess_output)) # expect_false("Are any of the outputs TRUE?")

  # Check unmasked var
  expect_true(any(grepl(unmasked_name, subprocess_output)), info = print(subprocess_output))
})
