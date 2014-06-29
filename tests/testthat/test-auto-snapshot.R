context("Auto Snapshot")

test_that("The auto snapshot call is parsable", {
  call <- buildSnapshotHookCall()
  parsed <- parse(text = paste(call, collapse = "; "))
})

test_that("Auto snapshot works", {
  snapshotHookImpl(debug = TRUE)
})
