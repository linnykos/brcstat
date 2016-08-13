BrcFmriGraphList <- function(graph.list, id, parcellation){
  obj <- structure(list(graph.list = graph.list, id = id, 
    parcellation = parcellation), class = "BrcFmriGraphList")
  
  brcbase::isValid(obj)
  
  obj
}

isValid.BrcFmriGraphList <- function(obj){
  if(class(obj) != "BrcFmriGraphList") stop("obj must be of class 
    BrcFmriGraphList")
  
  if(!is.list(obj$graph.list)) stop("obj$graph.list must be a list")
  if(!all(sapply(obj$graph.list, class) == "BrcGraph")) stop("obj$graph.list must
     be a list of BrcGraph")
  if(!is.character(obj$id)) stop("obj$id must be character")
  if(class(obj$parcellation) != "BrcParcellation") stop("obj$parcellation 
     must be of class BrcParcellation")
  if(!brcbase::isValid(obj$parcellation)) stop("obj$parcellation must be valid")

  .checkNumberofNodes(obj)
  
  TRUE
}

.checkNumberofNodes <- function(obj){
  if(class(obj) != "BrcFmriGraphList") stop("obj must be of class 
                                            BrcFmriGraphList")
  
  partition <- obj$parcellation$partition
  parcels <- unique(partition)
  num.parcels <- length(parcels[parcels != 0])
  
  nvec <- sapply(obj$graph.list, function(x){igraph::vcount(x$graph)})

  if(!all(nvec == num.parcels)) stop("number of vertices in in obj$graph.list
    do not match the number of parcels in obj$parcellation")
  
  TRUE
}