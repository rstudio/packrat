context("lockfile")

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

test_that("Repository is properly split by readLockFile", {

  lf <- readLockFile("lockfiles/lockfile-multipleRepos.txt")
  expect_equal(
    lf$repos,
    c(CRAN = "https://cran.rstudio.org",
      BioCsoft = "https://bioconductor.org/packages/3.0/bioc",
      BioCann = "https://bioconductor.org/packages/3.0/data/annotation",
      BioCexp = "https://bioconductor.org/packages/3.0/data/experiment",
      BioCextra = "https://bioconductor.org/packages/3.0/extra"
    )
  )

})
