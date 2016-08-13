BrcGraph <- function(graph, type, parameter){
  if(!igraph::is_igraph(graph)) stop("graph must be of class igraph")
  if(!is.character(type)) stop("type must be of character")
  if(!is.numeric(parameter)) stop("parameter must be of numeric")
  
  structure(list(graph = graph, type = type, parameter = parameter),
            class = "BrcGraph")
}

isValid.BrcGraph <- function(obj){
  if(!igraph::is_igraph(obj$graph)) stop("obj$graph must be of class igraph")
  if(!is.character(obj$type)) stop("obj$type must be of character")
  if(!is.numeric(obj$parameter)) stop("obj$parameter must be of numeric")
  
  TRUE
}