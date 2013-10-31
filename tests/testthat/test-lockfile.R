# Lockfile tests
# 
# To run these tests, set the working directory to packrat/tests and run 
# test_check("lockfile")
# 
# Also run by R CMD CHECK.

library(testthat)

# Set up test context. 
context("Lockfile tests")

test_that("Topo sort works", {
  
  # Good
  graph <- list(
    'A' = c('B', 'C'),
    'B' = c(),
    'C' = c('B'),
    'D' = c()
  )
  expect_true(verifyTopoSort(graph, topoSort(graph)))
  
  # Bad: Circular graph
  bad.graph <- list(
    'A' = c('B'),
    'B' = c('C'),
    'C' = c('A')
  )
  expect_error(topoSort(bad.graph))
  
  # Bad: Dependency that has no row
  bad.graph.2 <- list(
    'A' = c('D')
  )
  expect_error(topoSort(bad.graph.2))
})
