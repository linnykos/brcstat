#' Types of Graphs to be used in \code{estimateGraph}
#'
#' @return a vector of available graphs, to be passed into "method" of
#' \code{estimateGraph}.
#' @export
BrcGraphTypes <- function(){
  c("correlation", "partialCorrelation.glasso", 
    "partialCorrelation.neighborhoodSelect")
}

#' Estimate Connectome (Graph) from BrcFmri data
#'
#' The implementation for "partialCorrelation.glasso" and 
#' "partialCorrleation.neighborhoodselect" is based on the \code{HUGE} package.
#' The graphs output will have nodes, one for each unique parcel in 
#' x. The \code{BrcFmriGraphList} will contain a list of BrcGraphs, each 
#' corresponding to a different set of tuning parameters.
#'
#' @param x the \code{BrcFmri} object
#' @param method the method to use, must be one of the methods shown in
#' \code{BrcGraphTypes()}. "correlation" is the default.
#' @param ... additional parameters to be used when estimating the graph
#'
#' @return a new \code{BrcFmriGraphList} instance
#' @export
estimateGraph <- function(x, method = "correlation", ...){
  if(class(x) != "BrcFmri") stop("x must be of class BrcFmri")
  if(length(method) != 1) stop("method must be length 1")
  if(!method %in% BrcGraphTypes()) 
    stop("method must be a method in brcGraphTypes()")
  if(!isValid(x)) stop("x must be a valid instance of BrcFmri")
  
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

.estimateGraphCorrelation <- function(mat, lambda = seq(0,1,length.out=10),
 ...){
  stopifnot(is.numeric(lambda))
  stopifnot(all(diff(lambda) > 0))
  
  len <- length(lambda)
  n <- ncol(mat)
  cor.mat <- stats::cor(mat, ...)
  
  lapply(lambda, function(x){
    idx <- which(abs(cor.mat) > x, arr.ind = T); idx <- t(idx)
    graph <- makeGraphDefault(idx, n = n)
    BrcGraph(graph, "correlation", x)
  })
}

.estimateGraphGlasso <- function(mat, verbose = F, ...){
  res <- huge::huge(mat, method = "glasso", verbose = verbose, ...)
  n <- nrow(mat)

  lapply(1:length(res$path), function(x){
    idx <- Matrix::which(res$path[[x]] != 0, arr.ind = T); idx <- t(idx)
    graph <- makeGraphDefault(idx, n = n)
    BrcGraph(graph, "partialCorrelation.glasso", res$lambda[x])
  })
}

.estimateGraphNS <- function(mat, verbose = F, ...){
  res <- huge::huge(mat, method = "mb", verbose = verbose, ...)
  n <- nrow(mat)
  
  lapply(1:length(res$path), function(x){
    idx <- Matrix::which(res$path[[x]] != 0, arr.ind = T); idx <- t(idx)
    graph <- makeGraphDefault(idx, n = n)
    BrcGraph(graph, "partialCorrelation.neighborhoodSelect", res$lambda[x])
  })
}