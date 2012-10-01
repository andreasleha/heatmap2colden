#' heatmap2colden
#' 
#' @name heatmap2colden
#' @docType package
NULL

##' Plot a heatmap with colored row/col dendrograms
##'
##' This function is basically a call to heatmap.2 from the gplots package.
##' It has the added functionality to color branches in the row and/or col
##' dendrograms.
##' @param acolNclust number of clusters to color in the columns
##' @param browNclust number of clusters to color in the rows
##' @param ccolCol the colors to use in the column clusters
##' @param drowCol the colors to use in the row clusters
##' @param ... arguments passed through to heatmap.2
##' @return the heatmap.2 returned heatmap
##' @author Andreas Leha
##' @name heatmap.2.colden
##' @export
##' @examples
##' data(mtcars)
##' x  <- as.matrix(mtcars)
##' heatmap.2.colden(browNclust=2, x=x)
heatmap.2.colden <- function(...,
                             acolNclust,
                             browNclust,
                             ccolCol,
                             drowCol) {
  
  ## plot a heatmap only to extract the dendrograms
  tfile <- tempfile()
  png(tfile)
  hv <- heatmap.2(...)
  dev.off()
  unlink(tfile)
  
  ## normal heatmap.2 if no cluster numbers specified
  if (missing(acolNclust) && missing(browNclust)) {
    eval(hv$call)
    return(invisible(hv))
  }
  
  ## extract the dendrograms
  colDend <- hv$colDendrogram
  rowDend <- hv$rowDendrogram
  
  ## put color on the dendrograms
  if (!missing(acolNclust)) {
    colDend <- colorDendrogram(colDend, acolNclust, ccolCol)
  }
  if (!missing(browNclust)) {
    rowDend <- colorDendrogram(rowDend, browNclust, drowCol)
  }
  
  ## plot the heatmap again -- with colored dendrograms
  addargs <- as.list(substitute(list(...)))[-1L]
  addargs$Colv <- colDend
  addargs$Rowv <- rowDend
  hv <- do.call(heatmap.2, addargs)
  
  ## silently return the heatmap object
  invisible(hv)
}

##' Plot a heatmap with colored row/col dendrograms and use seriation on the dendrograms
##'
##' This function is basically a call to heatmap.2.colden from this package
##' and, thus, it has the added functionality to color branches in the row
##' and/or col dendrograms.
##' Additionally the row and col dendrograms are reordered (without
##' changing the clustering!) see the seriation package on the details.
##' @param ... arguments passed through to heatmap.2
##' @param acolNclust number of clusters to color in the columns
##' @param browNclust number of clusters to color in the rows
##' @param ccolCol the colors to use in the column clusters
##' @param drowCol the colors to use in the row clusters
##' @param sermethod which method to use for the seriation ("GW", "OLO")
##' @return the heatmap.2 returned heatmap
##' @author Andreas Leha
##' @name ser.heatmap.2.colden
##' @export
##' @examples
##' data(mtcars)
##' x  <- as.matrix(mtcars)
##' ser.heatmap.2.colden(sermethod="GW", browNclust=2, x=x)
ser.heatmap.2.colden <- function(...,
                                 acolNclust,
                                 browNclust,
                                 ccolCol,
                                 drowCol,
                                 sermethod="OLO") {
  if (!require(seriation))
    stop("the 'require' package is needed for this function")

  addargs <- as.list(substitute(list(...)))[-1L]

  mat <- eval(if ("x" %in% names(addargs)) addargs[["x"]] else addargs[[1]])
  
  colDend <- as.dendrogram(seriate(dist(t(mat)), method=sermethod)[[1]])
  rowDend <- as.dendrogram(seriate(dist(mat), method=sermethod)[[1]])

  heatmap.2.colden(...,
                   Colv=colDend,
                   Rowv=rowDend,
                   acolNclust=acolNclust,
                   browNclust=browNclust,
                   ccolCol=ccolCol,
                   drowCol=drowCol)
}
