sparsity <- function(obj) UseMethod("sparsity")

sparsity.igraph <- function(obj){
  n <- igraph::vcount(obj)
  igraph::ecount(obj)/(n*(n-1)/2)
}

sparsity.BrcGraph <- function(obj){
  if(!isValid(obj)) stop("obj is not a valid BrcGraph instance")
  
  sparsity(obj$graph)
}

sparsity.BrcFmriGraphList <- function(obj){
  if(!isValid(obj)) stop("obj is not a valid BrcFmriGraphList instance")
  
  len <- length(obj$graph.list)
  sapply(1:len, function(x){
    sparsity(obj$graph[[x]])
  })
}