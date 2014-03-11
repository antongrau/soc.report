#' Cross-table
#'
#' \code{tab.cross} performs a simple two-way cross tabulation of two factor vectors.
#' Easy printing and plotting is helped by simple formatting.
#' 
#' @param x a factor
#' @param y a factor
#' @param freq if TRUE frequencies placed within ()
#' @param prop.margin index, or vector of indices to generate margin for, \link{prop.table}
#' @param digits the maximum number of digits, \link{round}
#' @param sum.margin vector of dimensions over which to form margins. Margins are formed in the order in which dimensions are specified in margin. \link{addmargins}
#' @param sum.label is the label of the margins
#' @return a table with 2 dimensions
#' @export tab.cross

# The values now set as -- could be the sum proportions this should be simple

tab.cross <-  function(x, y, frequencies = TRUE, prop.margin = 1, digits = 2, sum.margin = 1:2, sum.label = "Total"){
  
  freq  <- table(x,y)
  prop  <- round(prop.table(freq, prop.margin), digits)
  
  pm    <- seq(1,2)[(seq(1,2) %in% prop.margin)==FALSE]
  
  freq  <- addmargins(freq, sum.margin)
  prop  <- addmargins(prop, sum.margin)
    
  cols  <- ncol(freq)
  pfreq <- paste("(", freq, ")", sep="")
  s     <- seq(from=dim(freq)[prop.margin], by=dim(freq)[prop.margin], to=length(prop))
  prop[s]  <- freq[s]/(sum(freq[s])/2)
  prop  <- paste(format(prop*100, justify="right"), "%")
  
  
  if (identical(frequencies, TRUE)){
  out.tab <- matrix(paste(format(prop, justify="centre"), format(pfreq, justify="right"), sep=" "), ncol=cols)
  }
  if (identical(frequencies, FALSE)){
  out.tab <- matrix(paste(format(prop, justify="centre"), sep=" "), ncol=cols)
  }
  rownames(out.tab) <- rownames(freq)
  colnames(out.tab) <- colnames(freq)

  rownames(out.tab)[length(rownames(out.tab))] <- sum.label
  colnames(out.tab)[length(colnames(out.tab))] <- sum.label
  out.tab
}

#' Frequency-table
#' 
#' @param x is a factor or character vector
#' @digits integer indicating the number of decimal places
#' @return a table of frequencies and proportions
#' @export tab.freq

tab.freq <- function(x, digits=2){
  
  freq <- table(x)  
  prop <- round(prop.table(freq), digits)
  
  prop.p <- format(paste(prop*100, "%", sep=" "), justify="right")
  freq.f <- format(freq, justify="right")
  
  out.tab <- cbind(prop.p, freq.f)
  colnames(out.tab) <- c("Procent", "Antal")
  out.tab
}

#' Standard width table
#' 
#' Formats a table to a standard width
#' @param tab is a table
#' @standard is the width of the table in number of characters
#' @return a reformatted tabel
#' @export standard.width.table

standard.width.table <- function(tab, standard=120){
  
  nc          <- ncol(tab) + 1
  each        <- standard / nc
  
  colnames(tab) <- format(colnames(tab), width=each, justify="centre")
  tab[]         <- format(tab, width=unit(each, "mm"), justify="centre")
  tab
}
