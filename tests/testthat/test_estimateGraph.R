context("Test estimate.Graph")

mat <- matrix(data=c(1, 2, 3, 4, 5, 6, 7, 8), nrow=2, ncol=4)
partition <- c(0, 1, 2, 2, 3, 3, 4, 0)
parcellation <- brcbase::BrcParcellation(dim3d=c(2, 2, 2), partition)
mri <- brcbase::BrcFmri(data2d=mat, id="01", parcellation=parcellation)
rm(list = c("mat", "partition", "parcellation"))

test_that("it produces a valid BrcFmriGraphList obj", {
  obj <- estimateGraph(mri)
  expect_true(brcbase::isValid(obj))
})

#############

set.seed(10)
n <- 20
vec1 <- rnorm(n) 
vec2 <- rnorm(n)
mat <- matrix(0, ncol = 10, nrow = n)
for(i in 1:5){
  mat[,i] <- vec1 + 0.2*rnorm(n)
}
for(i in 6:10){
  mat[,i] <- vec2 + 0.3*rnorm(n)
}
obj <- .estimateGraphCorrelation(mat, lambda = seq(0,1,length.out=11))
 

## test .estimateGraphCorrelation
test_that("it produces a valid list of graphs", {
  expect_true(is.list(obj))
  expect_true(all(sapply(obj, class) == "BrcGraph"))
})

test_that("it puts the lambda in right places", {
  expect_true(obj[[1]]$parameter == 0)
  expect_true(obj[[length(obj)]]$parameter == 1)
})

test_that("all elements in the list are correlation graphs",{
  for(i in 1:length(obj)){
    expect_true(obj[[i]]$type == "correlation")
  }
})

test_that("all graphs have the right number of nodes", {
  expect_true(all(sapply(obj, function(x){igraph::vcount(x$graph)}) == 10))
})

test_that("the first graph is completely dense", {
  expect_true(igraph::ecount(obj[[1]]$graph) == 10*9/2)
})

test_that("the last graph is empty", {
expect_true(igraph::ecount(obj[[length(obj)]]$graph) == 0)
})

###############################

## test .estimateGraphGlasso
obj <- .estimateGraphGlasso(mat)

test_that("all elements in list are glasso graphs",{
  for(i in 1:length(obj)){
    expect_true(obj[[i]]$type == "partialCorrelation.glasso")
  }
})

test_that("the first graph is empty", {
  expect_true(igraph::ecount(obj[[1]]$graph) == 0)
})

##########################

## test .estimateGraphNS
obj <- .estimateGraphNS(mat)

test_that("all elements in list are ns graphs",{
  for(i in 1:length(obj)){
    expect_true(obj[[i]]$type == "partialCorrelation.neighborhoodSelect")
  }
})

test_that("the first graph is empty", {
  expect_true(igraph::ecount(obj[[1]]$graph) == 0)
})

 