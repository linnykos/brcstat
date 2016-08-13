context("Test BrcFmriGraphList")

parcellation <- brcbase::BrcParcellation(dim3d=c(2, 2, 2), 1:8)
graph1 <- igraph::make_graph(1:8, directed = FALSE)
graph2 <- igraph::make_graph(c(1,rep(2:7,each = 2), 8), directed = FALSE)
bgraph1 <- BrcGraph(graph1)
bgraph2 <- BrcGraph(graph2)
graph.list <- list(bgraph1, bgraph2)

test_that("it errors when graph.list is not a list", {
  expect_error(BrcFmriGraphList(bgraph1, "01", parcellation))
})

test_that("it errors when graph.list is invalid", {
  expect_error(BrcFmriGraphList(graph1, "01", parcellation))
})

test_that("it errors when graph.list is not a list", {
  expect_error(BrcFmriGraphList(graph.list, "01", parcellation$partition))
})

test_that("it makes a valid BrcFmriGraphList", {
  obj <- BrcFmriGraphList(graph.list, "01", parcellation)
  expect_true(class(obj) == "BrcFmriGraphList")
  expect_true(brcbase::isValid(obj))
})

#######################

## test .checkNumberofNodes

test_that("it passes on a valid instance", {
  obj <- BrcFmriGraphList(graph.list, "01", parcellation)
  expect_true(.checkNumberofNodes(obj))
})

test_that("it errors when graphs have different sizes", {
  obj <- BrcFmriGraphList(graph.list, "01", parcellation)
  
  graph3 <- igraph::make_graph(c(1:10), directed = FALSE)
  bgraph3 <- BrcGraph(graph3)
  
  obj$graph.list[[3]] <- bgraph3
  
  expect_error(.checkNumberofNodes(obj))
})