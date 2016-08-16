plot.BrcFmriGraphList <- function(x, y = NA, graph.idx, ...){
  if(!isValid(x)) stop("x must be of class BrcFmriGraphList")
  if(!is.numeric(graph.idx)) stop("idx must be a numeric")
  if(graph.idx < 0 | graph.idx%%1 != 0) stop("idx must be a positive integer")
  if(graph.idx > length(x$graph.list)) stop("idx must be less than the number of
    graphs in x$graph.list")
  
  
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