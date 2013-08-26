\name{grob.tab}
\alias{grob.tab}
\title{Create style and plot a tableGrob object on the basis of any table.}
\usage{
  grob.tab(tab, style = "academic", footnote = "",
    title = "", main.line.color = "black")
}
\arguments{
  \item{tab}{is a table}

  \item{style}{is one of the predefined styles :
  "academic"}

  \item{footnote}{is the text displayed below the table}

  \item{title}{is the text displayed above the table}
}
\value{
  a plot version of the table
}
\description{
  grob.tab is mainly used for plotting and styling. Styling
  includes colors and seperator lines but not width and
  alignment. Alignment, width etc. should be handled by
  \link{paste} and \link{format}.
}

