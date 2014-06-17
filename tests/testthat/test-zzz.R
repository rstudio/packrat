context("cleanup")

# Remove the packrat directory generated for tests
unlink("packrat", recursive = TRUE)
