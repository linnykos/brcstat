#' \code{BrcGraph} Constructor
#' 
#' \code{BrcGraph} makes a new \code{BrcGraph} instance
#' 
#' A \code{BrcGraph} datatype contains all the information on the graph
#' as an \code{igraph} representation, the type of graph it statistically
#' represents, and the parameters needed when estimating the graph. 
#' 
#' Because \code{BrcGraph} is a dataype, not an object there are no accessor 
#' functions. You can get its components directly with the \code{$} operator.
#' In particular, you can use all of functions in \code{igraph} to interact
#' with the \code{graph} element in \code{BrcGraph}
#'
#' @param graph an igraph object representing the graph
#' @param type a character representing the type of graph (statistically)
#' @param parameter a vector of numerics to specify how the graph was estimated
#'
#' @return a new \code{BrcGraph} instance
#' @export
BrcGraph <- function(graph, type = NULL, parameter = NULL){
  obj <- structure(list(graph = graph, type = type, parameter = parameter),
            class = "BrcGraph")
  
  brcbase::isValid(obj)
  
  obj
}


#' Checking BrcGraph instance validity.
#' 
#' \code{isValid} method for class \code{BrcGraph}.
#' 
#' Fails noisily with a stop message if the \code{BrcGraph}
#' instance is invalid. Otherwise, returns TRUE.
#'
#' @param obj the \code{BrcGraph} object
#'
#' @return TRUE if valid
#' @export
isValid.BrcGraph <- function(obj){
  if(!igraph::is_igraph(obj$graph)) stop("obj$graph must be of class igraph")
  if(!is.character(obj$type) & !is.null(obj$type)) 
    stop("obj$type must be of character")
  if(!is.numeric(obj$parameter) & !is.null(obj$parameter)) 
    stop("obj$parameter must be of numeric")
  
  TRUE
}

#' Construct an \code{igraph} object from a matrix of nodes
#'
#' \code{makeGraphDefault} makes a new \code{igraph} instance.
#' 
#' The \code{edges} can represent a matrix or vector. If there are 
#' isolated edges, be sure to use the \code{n} parameter. The default
#' value of \code{n} is the maximum value in \code{edges}. The unique
#' values in \code{edges} do not need to have unique, consecutive values.
#' If a matrix is passed in for \code{edges}, the code will implicitly
#' convert the matrix into \code{as.numeric(edges)}, and each consecutive
#' pair of values will represent an edge.
#' 
#' The graph outputted is simplified, meaning there are no self-edges
#' (i.e., loops, or edges that start and end at the same node) or
#' duplicated edges. It is also undirected. 
#'
#' @param edges a matrix or vector of numerics (positive integers) representing
#' the edges between nodes
#' @param n the total number of nodes
#' @param ... additional parameters to be passed into igraph::make_graph
#'
#' @return a new \code{igraph} instance
#' @export
makeGraphDefault <- function(edges, n = max(edges), ...){
  obj <- igraph::make_graph(edges, n, directed = FALSE)
  igraph::simplify(obj)
}