# Testcase inspired by
# https://github.com/rstudio/packrat/issues/602
#
# Ignore the package argument to requireNamespace when it is a symbol.

# This statement should produce a dependency, since library accepts
# symbols as package names.
library(bread)

# This statement should produce a dependency, since we have a
# literal string.
requireNamespace("oatmeal")

# These statements should not produce dependencies, since requireNamespace
# does not accept symbols as package names and we have a symbol (pkg) as
# input.
wanted <- c("egg", "toast")
missing <- lapply(wanted, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    pkg
  }
})

# This statement should not produce dependencies, as we cannot read through
# the pkg variable and symbols are not permitted with character.only = TRUE.
pkg <- "breakfast"
library(pkg, character.only = TRUE)
