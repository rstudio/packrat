
# Call an action hook (indicating whether the action is running or not)
callHook <- function(action, running) {
  for (fun in getHooksList("packrat.onAction")) {
    if (is.character(fun))
      fun <- get(fun)
    try(fun(action, running))
  }
}

# The value for getHook can be a single function or a list of functions,
# This function ensures that the result can always be processed as a list
getHooksList <- function(name) {
  hooks <- getHook(name)
  if (!is.list(hooks))
    hooks <- list(hooks)
  hooks
}
