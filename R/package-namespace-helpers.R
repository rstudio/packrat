loaded_user_pkgs <- function() {
  loaded <- loadedNamespaces()
  ip <- installed.packages()
  basePkgs <- rownames(ip)[!is.na(ip[, "Priority"])]
  loaded[!(loaded %in% basePkgs)]
}

ns_imports <- function(packages) {
  setNames(lapply(packages, ns_imports_single), packages)
}

ns_imports_single <- function(package) {
  ns <- asNamespace(package)
  names(getNamespaceImports(ns))
}

is_imported <- function(package) {
  imports <- ns_imports(loadedNamespaces())
  any(sapply(imports, function(x) package %in% x))
}

unloadable <- function(packages) {
  result <- sapply(packages, Negate(is_imported))
  names(result)[result]
}
