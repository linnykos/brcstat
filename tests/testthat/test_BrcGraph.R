context("Test BrcGraph")

test_that("it errors when graph is not igraph",{
  set.seed(10)
  mat <- matrix(rbinom(100,1,0.5),10,10)
  expect_error(BrcGraph(mat, "asdf", 5))
})

test_that("it errors when type is not character", {
  graph <- igraph::make_graph(c(1, 2, 2, 3, 3, 4, 5, 6), directed = FALSE)
  expect_error(BrcGraph(graph, 5, 5))
})

test_that("it errors when parameter is not numeric", {
  graph <- igraph::make_graph(c(1, 2, 2, 3, 3, 4, 5, 6), directed = FALSE)
  expect_error(BrcGraph(graph, "asdf", "asdf"))
})

test_that("it can validate itself", {
  graph <- igraph::make_graph(c(1, 2, 2, 3, 3, 4, 5, 6), directed = FALSE)
  obj <- BrcGraph(graph, "asdf", 5)
  expect_true(class(obj) == "BrcGraph")
  expect_true(brcbase::isValid(obj))
})

test_that("it makes a valid BrcGraph when making a graph from matrix", {
  set.seed(10)
  mat <- matrix(rbinom(100,1,0.5),10,10)
  obj <- BrcGraph(igraph::graph_from_incidence_matrix(mat), "asdf", 5)
  expect_true(brcbase::isValid(obj))
})

test_that("it makes a valid BrcGraph when making a graph from edge matrix", {
  set.seed(10)
  mat <- matrix(c(1:10,2:11), ncol = 2, nrow = 10)
  obj <- BrcGraph(igraph::graph_from_edgelist(mat), "asdf", 5)
  expect_true(brcbase::isValid(obj))
})

test_that("it makes a valid BrcGraph when no type or parameter are used", {
  graph <- igraph::make_graph(c(1, 2, 2, 3, 3, 4, 5, 6), directed = FALSE)
  obj <- BrcGraph(graph)
  expect_true(brcbase::isValid(obj))
})
