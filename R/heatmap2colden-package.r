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
heatmap.2.colden <- function(acolNclust,
                             browNclust,
                             ccolCol,
                             drowCol,
                             ...) {
  
  ## normal heatmap.2 if no cluster numbers specified
  if (missing(acolNclust) && missing(browNclust)) {
    ##eval(hv$call)
    hv <- heatmap.2(...)
    return(invisible(hv))
  }
  
  ## FIXME: depends on named "x" in "..."
  ## extract the dendrograms
  colDend <- as.dendrogram(seriate(dist(t(eval(as.list(substitute(list(...)))[-1L][["x"]]))), method="OLO")[[1]])
  rowDend <- as.dendrogram(seriate(dist(eval(as.list(substitute(list(...)))[-1L][["x"]])), method="OLO")[[1]])
  
  ## put color on the dendrograms
  if (!missing(acolNclust)) {
    colDend <- colorDendrogram(colDend, acolNclust, ccolCol)
  }
  if (!missing(browNclust)) {
    rowDend <- colorDendrogram(rowDend, browNclust, drowCol)
  }
  
  ## plot the heatmap -- with colored dendrograms
  hv <- heatmap.2(..., Colv=colDend, Rowv=rowDend)
  
  ## silently return the heatmap object
  invisible(hv)
}
