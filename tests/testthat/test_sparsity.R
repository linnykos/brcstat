context("Test sparsity")

## test sparsity.igraph

test_that("it computes sparsity correctly",{
  obj <- makeGraphDefault(c(1:10))
  expect_true(sparsity(obj) == 5/(10*9/2))
})

## test sparsity.BrcGraph

test_that("it computes sparsity correctly",{
  obj <- makeGraphDefault(c(1:10))
  graph <- BrcGraph(obj)
  expect_true(sparsity(obj) == 5/(10*9/2))
})

## test sparsity.BrcFmriGraphList

test_that("it computes sparsity correctly",{
  parcellation <- brcbase::BrcParcellation(dim3d=c(2, 2, 2), 1:8)
  graph1 <- igraph::make_graph(1:8, directed = FALSE)
  graph2 <- igraph::make_graph(c(1,rep(2:7,each = 2), 8), directed = FALSE)
  bgraph1 <- BrcGraph(graph1)
  bgraph2 <- BrcGraph(graph2)
  graph.list <- list(bgraph1, bgraph2)
  
  obj <- BrcFmriGraphList(graph.list, "01", parcellation)
  
  expect_true(all(sparsity(obj) == c(4/(8*7/2), 7/(8*7/2))))
})