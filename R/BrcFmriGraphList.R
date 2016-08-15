#' \code{BrcFmriGraphList} Constructor
#' 
#' \code{BrcFmriGraphList} makes a new \code{BrcFmriGraphList} instance
#' 
#' A \code{BrcFmriGraphList} datatype contains all the information to 
#' represent a list of graphs within the MRI image. Specifically, it contains
#' a list of \code{BrcGraph} objects, stored in the \code{graph.list} element,
#' an \code{id} and the \code{BrcParcellation} object.
#' 
#' The number of nodes in each \code{BrcGraph} object in \code{graph.list}
#' must have the same number of unique parcellations in \code{parcellation}.
#'
#' @param graph.list a list of \code{BrcGraph} instances
#' @param id an identification string. It is not guaranteed to be
#'      unique or nonempty---those depend on how the \code{BrcFmri} object
#'      is constructed. Typically this field would be used to cross-reference
#'      the MRI with a dataframe containing phenotype information.
#' @param parcellation a \code{BrcParcellation} instance
#' 
#' @return a new \code{BrcFmriGraphList} instance
#' @export
BrcFmriGraphList <- function(graph.list, id, parcellation){
  if(!is.list(graph.list)) stop("graph.list must be a list")
  if(!is.character(id)) stop("id must be a character")
  if(class(parcellation) != "BrcParcellation")
    stop("obj$parcellation must be of class BrcParcellation")
  
  obj <- structure(list(graph.list = graph.list, id = id, 
    parcellation = parcellation), class = "BrcFmriGraphList")
  
  brcbase::isValid(obj)
  
  obj
}

#' Checking \code{BrcFmriGraphList} instance validity.
#' 
#' \code{isValid} method for class "\code{BrcFmriGraphList}.
#' 
#' Fails noisily with a stop message if the \code{BrcFmriGraphList}
#' instance is invalid. Otherwise, returns TRUE.
#'
#' @param obj the \code{BrcFmriGraphList} object
#'
#' @return TRUE if valid
#' @export
isValid.BrcFmriGraphList <- function(obj){
  if(!all(sapply(obj$graph.list, class) == "BrcGraph")) stop("obj$graph.list must
     be a list of BrcGraph")
  if(!brcbase::isValid(obj$parcellation)) stop("obj$parcellation must be valid")

  .checkNumberofNodes(obj)
  
  TRUE
}

.checkNumberofNodes <- function(obj){
  if(class(obj) != "BrcFmriGraphList") stop("obj must be of class 
                                            BrcFmriGraphList")
  
  num.parcels <- brcbase::numParcels(obj$parcellation)
  
  nvec <- sapply(obj$graph.list, function(x){igraph::vcount(x$graph)})

  if(!all(nvec == num.parcels)) stop("number of vertices in in obj$graph.list
    do not match the number of parcels in obj$parcellation")
  
  TRUE
}