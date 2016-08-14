BrcGraph <- function(graph, type = NULL, parameter = NULL){
  obj <- structure(list(graph = graph, type = type, parameter = parameter),
            class = "BrcGraph")
  
  brcbase::isValid(obj)
  
  obj
}

isValid.BrcGraph <- function(obj){
  if(!igraph::is_igraph(obj$graph)) stop("obj$graph must be of class igraph")
  if(!is.character(obj$type) & !is.null(obj$type)) 
    stop("obj$type must be of character")
  if(!is.numeric(obj$parameter) & !is.null(obj$parameter)) 
    stop("obj$parameter must be of numeric")
  
  TRUE
}

.makeGraphDefault <- function(edges, n = max(edges), ...){
  obj <- igraph::make_graph(edges, n, directed = FALSE)
  igraph::simplify(obj)
}