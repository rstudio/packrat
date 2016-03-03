replaceBinding <- function(name, value) {

  ## Need to clobber in package:base, namespace:base
  envs <- c(
    as.environment("package:base"),
    .BaseNamespaceEnv
  )

  for (env in envs) {
    do.call("unlockBinding", list(name, env))
    assign(name, value, envir = env)
    lockBinding(name, env)
  }

}
