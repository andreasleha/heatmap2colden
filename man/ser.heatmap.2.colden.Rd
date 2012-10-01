\name{ser.heatmap.2.colden}
\alias{ser.heatmap.2.colden}
\title{Plot a heatmap with colored row/col dendrograms and use seriation on the dendrograms}
\usage{
  ser.heatmap.2.colden(..., acolNclust, browNclust,
    ccolCol, drowCol, sermethod = "OLO")
}
\arguments{
  \item{...}{arguments passed through to heatmap.2}

  \item{acolNclust}{number of clusters to color in the
  columns}

  \item{browNclust}{number of clusters to color in the
  rows}

  \item{ccolCol}{the colors to use in the column clusters}

  \item{drowCol}{the colors to use in the row clusters}

  \item{sermethod}{which method to use for the seriation
  ("GW", "OLO")}
}
\value{
  the heatmap.2 returned heatmap
}
\description{
  This function is basically a call to heatmap.2.colden
  from this package and, thus, it has the added
  functionality to color branches in the row and/or col
  dendrograms. Additionally the row and col dendrograms are
  reordered (without changing the clustering!) see the
  seriation package on the details.
}
\examples{
data(mtcars)
x  <- as.matrix(mtcars)
ser.heatmap.2.colden(sermethod="GW", browNclust=2, x=x)
}
\author{
  Andreas Leha
}

