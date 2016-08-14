BrcGraphTypes <- function(){
  c("correlation", "partialCorrelation.glasso", 
    "partialCorrelation.neighborhoodSelect")
}

estimateGraph <- function(x, ...) {UseMethod("estimateGraph")}

estimateGraph.BrcFmri <- function(x, method = "correlation", ...){
  if(class(x) != "BrcFmri") stop("x must be of class BrcFmri")
  if(length(method) != 1) stop("method must be length 1")
  if(!method %in% BrcGraphTypes()) 
    stop("method must be a method in brcGraphTypes()")
  
  if(method == "correlation"){
    graph.list <- .estimateGraphCorrelation(x$data2d, ...)
  } else if(method == "partialCorrelation.glasso"){
    graph.list <- .estimateGraphGlasso(x$data2d, ...)
  } else if(method == "partialCorrelation.neighborhoodSelect"){
    graph.list <- .estimateGraphNS(x$data2d, ...)
  }
  
  structure(list(graph.list = graph.list, id = x$id, parcellation = 
    x$parcellation), class = "BrcFmriGraphList")
}

.estimateGraphCorrelation <- function(mat, lambda = seq(0,1,length.out=11),
 ...){
  stopifnot(is.numeric(lambda))
  stopifnot(all(diff(lambda) > 0))
  
  len <- length(lambda)
  n <- ncol(mat)
  cor.mat <- stats::cor(mat, ...)
  
  lapply(lambda, function(x){
    idx <- which(abs(cor.mat) > x, arr.ind = T); idx <- t(idx)
    graph <- .makeGraphDefault(idx, n = n)
    BrcGraph(graph, "correlation", x)
  })
}

.estimateGraphGlasso <- function(mat, ...){
  x <- igraph::make_graph(rep(1:ncol(mat), times = 2), directed = F)
  obj <- BrcGraph(x)
  graph.list <- vector("list", 1)
  graph.list[[1]] <- obj
  
  graph.list
}

.estimateGraphNS <- function(mat, ...){
  x <- igraph::make_graph(rep(1:ncol(mat), times = 2), directed = F)
  obj <- BrcGraph(x)
  graph.list <- vector("list", 1)
  graph.list[[1]] <- obj
  
  graph.list
}