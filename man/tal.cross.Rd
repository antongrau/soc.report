\name{tal.cross}
\alias{tal.cross}
\title{Crosstable in the &tal style}
\usage{
tal.cross(x, y, prop.margin = 1, digits = 2, margin = 1:2, stat = TRUE,
  color = rgb(144, 26, 30, maxColorValue = 255), title = "",
  min.width = 120)
}
\arguments{
  \item{x}{a factor}

  \item{y}{a factor}

  \item{freq}{if TRUE frequencies placed within ()}

  \item{prop.margin}{index, or vector of indices to
  generate margin for, \link{prop.table}}

  \item{digits}{the maximum number of digits, \link{round}}

  \item{vector}{of dimensions over which to form margins.
  Margins are formed in the order in which dimensions are
  specified in margin. \link{addmargins}}

  \item{stat}{if TRUE returns various statistical tests}

  \item{color}{sets the color of the main lines}

  \item{min.width}{is the minimum width of the table, see
  \link{standard.with.table}}

  \item{title}{sets the title}
}
\description{
Crosstable in the &tal style
}

