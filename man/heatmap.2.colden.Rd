\name{heatmap.2.colden}
\alias{heatmap.2.colden}
\title{heatmap.2.colden}
\usage{
  heatmap.2.colden(acolNclust, browNclust, ccolCol = NULL,
    drowCol = NULL, ...)
}
\arguments{
  \item{acolNclust}{number of clusters to color in the
  columns}

  \item{browNclust}{number of clusters to color in the
  rows}

  \item{ccolCol}{the colors to use in the column clusters}

  \item{drowCol}{the colors to use in the row clusters}

  \item{...}{arguments passed through to heatmap.2}
}
\value{
  the heatmap.2 returned heatmap
}
\description{
  Plot a heatmap with colored row/col dendrograms
}
\details{
  This function is basically a call to heatmap.2 from the
  gplots package. It has the added functionality to color
  branches in the row and/or col dendrograms.
}
\author{
  Andreas Leha
}

