plot.BrcFmriGraphList <- function(x, numSlices, graph.idx, 
  view="sagittal", colors=NULL, ...){
  
  if(!isValid(x)) stop("x must be of class BrcFmriGraphList")
  if(!is.numeric(graph.idx)) stop("idx must be a numeric")
  if(graph.idx < 0 | graph.idx%%1 != 0) stop("idx must be a positive integer")
  if(graph.idx > length(x$graph.list)) stop("idx must be less than the number of
    graphs in x$graph.list")
  
   numPar <- brcbase::numParcels(x)

  tryCatch({ brcbase::isValid(x) }, error=function(e) {
    stop(paste("Tried to plot invalid BrcParcellation object: ", e))
  })

  if ((numSlices %% 1 != 0) || (numSlices < 0)) {
    stop("numSlices argument must be a positive integer")
  }

  views <- list(sagittal=1, coronal=2, axial=3)
  if (!(view %in% names(views))) {
    stop(c("view argument must be one of 'sagittal', 'coronal', or 'axial'"))
  }

  if (is.null(colors)) {
    colors <- .defaultColors(numPar)
  } else if (!all(.isColor(colors))) {
    stop("color argument contains invalid colors")
  } else if (brcbase::numParcels(x) != (length(colors) - 1)) {
    stop(paste("colors argument must contain 1 more color than the number ",
               "of parcels in the parcellation"))
  }


  dimension <- views[[view]]

  arr <- .parcellationToArray(x)
  indices <- .makeIndexSequence(max=dim(arr)[dimension], length=numSlices)
  slices <- .extractSlices(arr, indices, dimension)
  invisible(.plotSlices(slices, brcbase::numParcels(x), colors))
}

.findNeighbors <- function(vec, graph){
  stopifnot(max(vec) <= igraph::vcount(graph))
  
  res <- igraph::ego(graph, 1, node = vec, mode = "all")
  for(i in 1:length(res)){
    res[[i]] <- sort(res[[i]])
  }
  
  res
}

.findUniqueParcels <- function(mat){
  sort(unique(mat[mat != 0]))
}

.isColor <- function(colors) {
  unname(sapply(colors, function(x) {
    tryCatch(is.matrix(grDevices::col2rgb(x)), error=function(e) FALSE)
  }))
}

.defaultColors <- function(numParcels) {
  c("#000000FF", grDevices::rainbow(numParcels))
}

.parcellationToArray <- function(parcellation) {
  data <- parcellation$partition
  array(data=data, dim=parcellation$dim3d)
}

.makeIndexSequence <- function(max, length) {
  round(seq(1, max, length.out=length))
}

.extractSlices <- function(arr, indices, dim) {
  .splitAlongDim(arr, dim)[indices]
}

.plotSlices <- function(slices, numParcels, colors) {
  layout <- .plotLayout(numSlices=length(slices))
  maxParcel <- max(unlist(slices))
  graphics::par(mfrow=c(layout$nrow, layout$ncol), mar=rep(0.2, 4), bg="black")
  for (i in 1:length(slices)) {
    graphics::image(slices[[i]],
                    asp=ncol(slices[[i]]) / nrow(slices[[i]]),
                    breaks=(0:(maxParcel + 1)) - 0.5,
                    bty="n",
                    col=colors,
                    xaxt="n",
                    yaxt="n")
  }
}

.plotLayout <- function(numSlices) {
	nrow = ceiling(sqrt(numSlices / 2))
	ncol = ceiling(numSlices / nrow)
	list(nrow=nrow, ncol=ncol)
}

# This function borrowed from
# http://stackoverflow.com/questions/20198751/three-dimensional-array-to-list
# Thanks, internet!
.splitAlongDim <- function(arr, dim) {
  stats::setNames(lapply(split(arr, arrayInd(seq_along(arr), dim(arr))[, dim]),
                         array, dim=dim(arr)[-dim], dimnames(arr)[-dim]),
                  dimnames(arr)[[dim]])
}