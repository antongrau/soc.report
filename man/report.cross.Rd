\name{report.cross}
\alias{report.cross}
\title{Create a report with cross-tables}
\usage{
  report.cross(variables, p.test = 0.05)
}
\arguments{
  \item{variables}{is a data.frame of factors}
}
\value{
  several plotted tables
}
\description{
  Takes a data.frame of factors and makes all possible
  two-dimensional crosstables (\link{tab.cross}). Only the
  tables that pass a \link{chisq.test} are printed.
}

