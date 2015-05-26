#' freq.tab
#'
#' Creates a simple frequency table and store it in a data.frame.
#' In additiion it adds attributes (header, sub.header, answer, type) to the output data.frame.
#' Attributes are usefull when writing tables (data.frames) to excel (with the tabout function)
#'
#'@param x a vector
#'@param weighs a numeric vector of weights
#'@param transpose transposes the table if set as TRUE
#'@return a data.frame
#'@export freq.tab

freq.tab <- function(x, weights=NULL, transpose=FALSE){
  
  count <- wtd.table(x, weights, type='table')
  count <- addmargins(count)
  names(count)[length(count)] <- "Total"
  
  pct <- wpct(x, weights)
  pct[(length(pct)+1)] <- sum(pct)
  names(pct)[length(pct)] <- "Total"
  
  out <- cbind(count,pct)
  dimnames(out)[[2]] <- c("Antal svar", "Andel (%)")
  
  if (transpose)
    out <- t(out)
  
  out <- data.frame(out, check.names=FALSE)
  
  attributes(out)$header <- attributes(x)$header
  attributes(out)$sub.header <- attributes(x)$sub.header
  attributes(out)$answer <- attributes(x)$answer
  attributes(out)$type <- "freq.tab"
  out
}
