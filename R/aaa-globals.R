.packrat <- new.env(parent = emptyenv())
.packrat$packratFormat <- "1.4"
.packrat$options <- NULL

## Mutable values that might be modified by the user (code borrowed from knitr)
# merge elements of y into x with the same names
merge_list = function(x, y) {
  x[names(y)] = y
  x
}

new_defaults = function(value = list()) {
  defaults = value

  get = function(name, default = FALSE, drop = TRUE) {
    if (default) defaults = value  # this is only a local version
    if (missing(name)) defaults else {
      if (drop && length(name) == 1) defaults[[name]] else {
        setNames(defaults[name], name)
      }
    }
  }
  set = function(...) {
    dots = list(...)
    if (length(dots) == 0) return()
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]]))
      if (length(dots <- dots[[1]]) == 0) return()
    defaults <<- merge(dots)
    invisible(NULL)
  }
  merge = function(values) merge_list(defaults, values)
  restore = function(target = value) defaults <<- target

  list(get = get, set = set, merge = merge, restore = restore)
}

## These should be set on entering, exiting packrat mode
.packrat_mutables <- new_defaults(list(
  origLibPaths = NULL,
  project = NULL
))
