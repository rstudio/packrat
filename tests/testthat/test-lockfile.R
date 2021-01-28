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

test_that("multiple styles of repository parsing are supported", {
  # One old style repository, no name. Assumed to be CRAN.
  expect_equal(
    parseRepositories("A"),
    c(CRAN = "A")
  )

  # Two old repositories, no names.
  expect_warning(parseRepositories("A,B"))

  # One repository, no whitespace separating name and URL
  expect_equal(
    parseRepositories("A=B"),
    c(A = "B")
  )

  # One repository, whitespace separating name and URL
  expect_equal(
    parseRepositories("A = B"),
    c(A = "B")
  )

  # Two repositories, no whitespace.
  expect_equal(
    parseRepositories("A=B,C=D"),
    c(A = "B", C = "D")
  )

  # Two repositories, with whitespace.
  expect_equal(
    parseRepositories("A = B , C = D"),
    c(A = "B", C = "D")
  )

  # Two repositories, with newlines.
  expect_equal(
    parseRepositories("A = B,\nC = D\n"),
    c(A = "B", C = "D")
  )

  # Three repositories
  repos <- parseRepositories("A = B,\nC = D,\nE = F\n")
  expect_equal(
    repos,
    c(A = "B", C = "D", E = "F")
  )

  # Four repositories
  expect_equal(
    parseRepositories("A = B,\nC = D,\nE = F,\nG = H\n"),
    c(A = "B", C = "D", E = "F", G = "H")
  )

  # One repository name with a comma
  expect_equal(
    parseRepositories("A,B=C"),
    c("A,B" = "C")
  )

  # Two repository names with commas
  expect_equal(
    parseRepositories("A,B=C,D,E=F"),
    c("A,B" = "C", "D,E" = "F")
  )

  # Three repository names with commas
  expect_equal(
    parseRepositories("A,B=C,D,E=F,G,H=I"),
    c("A,B" = "C", "D,E" = "F", "G,H" = "I")
  )
})
