\name{kill.the.small}
\alias{kill.the.small}
\title{Recode very small levels into Missing values}
\usage{
kill.the.small(variables, threshold = 0.05, new.level = "Missing")
}
\arguments{
  \item{variables}{is a data.frame of factors}

  \item{threshold}{is the proportion below which a level is
  recoded}

  \item{new.level}{is the new label for the recoded levels}
}
\value{
a data.frame
}
\description{
Kill.the.small recodes all levels with proportions below a
certain threshold into "Missing".  This is useful for a
very crude first look at a lot of variables.
}

