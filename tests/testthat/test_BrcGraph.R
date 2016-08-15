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

#####################

## test makeGraphDefault 

test_that("it makes an undireted graph", {
  obj <- makeGraphDefault(1:10)
  expect_true(!igraph::is.directed(obj))
})

test_that("it makes a graph without loops", {
  obj <- makeGraphDefault(c(1:10,rep(1:10, each = 2)))
  expect_true(!any(igraph::which_loop(obj)))
  expect_true(igraph::ecount(obj) == 5)
})

test_that("it makes a graph without duplicate edges", {
  obj <- makeGraphDefault(c(1:10,1:10))
  expect_true(igraph::ecount(obj) == 5)
})

test_that("it makes graph properly when there are isolated edges", {
  obj <- makeGraphDefault(c(1:10), n = 15)
  expect_true(igraph::ecount(obj) == 5)
  expect_true(igraph::vcount(obj) == 15)
})

test_that("it makes graph with edge.list as a 2x(num.edges) matrix",{
  obj <- makeGraphDefault(matrix(1:10,nrow = 2))
  mat <- igraph::as_adjacency_matrix(obj)
  expect_true(mat[1,2] == 1)
  expect_true(mat[1,3] == 0)
})

test_that("it can make graph from empty edge.list", {
  obj <- makeGraphDefault(numeric(0),n=12)
  expect_true(igraph::vcount(obj) == 12)
  expect_true(igraph::ecount(obj) == 0)
})

test_that("it does not require unique consecutive edges", {
  obj <- makeGraphDefault(c(1,3,6,10))
    expect_true(igraph::vcount(obj) == 10)
  expect_true(igraph::ecount(obj) == 2)
})