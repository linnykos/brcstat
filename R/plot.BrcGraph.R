plot.BrcGraph <- function(x, y = NA, ...){
  igraph::plot.igraph(x$graph, ...)
  
  invisible()
}