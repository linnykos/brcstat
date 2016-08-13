context("Test BrcGraph")

test_that("BrcGraph errors when graph is not igraph",{
  mat <- matrix(rbinom(100,1,0.5),10,10)
  expect_error(BrcGraph(mat, "asdf", 5))
})

test_that("BrcGraph errors when type is not character", {
  graph <- igraph::make_graph(c(1, 2, 2, 3, 3, 4, 5, 6), directed = FALSE)
  expect_error(BrcGraph(graph, 5, 5))
})

test_that("BrcGraph errors when parameter is not numeric", {
  graph <- igraph::make_graph(c(1, 2, 2, 3, 3, 4, 5, 6), directed = FALSE)
  expect_error(BrcGraph(graph, "asdf", "asdf"))
})

test_that("isValid works", {
  graph <- igraph::make_graph(c(1, 2, 2, 3, 3, 4, 5, 6), directed = FALSE)
  obj <- BrcGraph(graph, "asdf", 5)
  expect_true(brcbase::isValid(obj))
})