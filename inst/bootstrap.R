## Install packrat into local project library
packratSrcPath <- list.files("packrat/src/packrat", full.names = TRUE)[1]
if (!length(packratSrcPath)) {
  stop("Could not find a local packrat source tarball")
}
lib <- file.path("packrat", "lib", R.version$platform, getRversion())
if (!file.exists(lib)) {
  dir.create(lib, recursive = TRUE)
}
lib <- normalizePath(lib, winslash = "/")
message("> Installing packrat into project private library:")
message("> ", shQuote(lib))
peq <- function(x, y) paste(x, y, sep = " = ")
installArgs <- c(
  peq("pkgs", shQuote(packratSrcPath)),
  peq("lib", shQuote(lib)),
  peq("repos", "NULL")
)
installCmd <- paste(sep = "",
                    "install.packages(", paste(installArgs, collapse = ", "), ")")

fullCmd <- paste(
  shQuote(file.path(R.home("bin"), "R")),
  "--vanilla",
  "--slave",
  "-e",
  shQuote(installCmd)
)
system(fullCmd)

message("> Attaching packrat")
library("packrat", character.only = TRUE, lib.loc = lib)

message("Packrat successfully installed. Run 'packrat_mode()' to enter packrat mode.")
