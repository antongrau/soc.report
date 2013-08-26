\name{tab.cross}
\alias{tab.cross}
\title{Cross-table}
\usage{
  tab.cross(x, y, frequencies = TRUE, prop.margin = 1,
    digits = 2, sum.margin = 1:2, sum.label = "Total")
}
\arguments{
  \item{x}{a factor}

  \item{y}{a factor}

  \item{freq}{if TRUE frequencies placed within ()}

  \item{prop.margin}{index, or vector of indices to
  generate margin for, \link{prop.table}}

  \item{digits}{the maximum number of digits, \link{round}}

  \item{sum.margin}{vector of dimensions over which to form
  margins. Margins are formed in the order in which
  dimensions are specified in margin. \link{addmargins}}

  \item{sum.label}{is the label of the margins}
}
\value{
  a table with 2 dimensions
}
\description{
  \code{tab.cross} performs a simple two-way cross
  tabulation of two factor vectors. Easy printing and
  plotting is helped by simple formatting.
}

