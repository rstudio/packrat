.packrat <- new.env(parent = emptyenv())

# 'Global' options that we might want to allow users to set
.packrat$packratFolderName <- ".packrat"

# Settings seen when packrat is loaded (used when turning packrat mode on and off)
.packrat$promptOnLoad <- getOption("prompt")
.packrat$origLibPaths <- .libPaths()
