#' freq.tab
#'
#' Creates a simple frequency table and store it in a data.frame.
#' In additiion it adds attributes (header, sub.header, answer, type) to the output data.frame.
#'
#'@param x a vector
#'@param weighs a numeric vector of weights
#'@param transpose transposes the table if set as TRUE
#'@param cells a vector indicating which cells to export.Default is c("count", "pct")
#'@return a data.frame
#'@export freq.tab

freq.tab <- function(x, weights=NULL, transpose=FALSE, cells=c("count","pct")){
  
  count <- wtd.table(x, weights, type='table')
  count <- addmargins(count)
  names(count)[length(count)] <- "Total"
  
  pct <- wpct(x, weights)
  pct[(length(pct)+1)] <- sum(pct)
  names(pct)[length(pct)] <- "Total"
  
  out <- cbind(count,pct)
  dimnames(out)[[2]] <- c("Antal svar", "Andel (%)")
  
  out <- data.frame(out, check.names=FALSE)
  
  if (!"count" %in% cells){
    out <- data.frame(pct)
    names(out) <- "Andel (%)"
  }
  
  if (!"pct" %in% cells){
    out <- data.frame(count)
    names(out) <- "Antal svar"
  }
  
  if (transpose)
    out <- data.frame(t(out), check.names=FALSE)
  
  attributes(out)$header <- attributes(x)$header
  attributes(out)$sub.header <- attributes(x)$sub.header
  attributes(out)$answer <- attributes(x)$answer
  attributes(out)$type <- "freq.tab"
  out
}

#' freq.mc
#'
#' Creates a multiple choice frequency table from a data.frame and store it in a data.frame
#' 
#' When working with survey data one often finds oneself with a single variable for each element of a multiple choice question. 
#' This function performs a frequency table on each variable in x and compounds the yes.answer into a single frequency table 
#' and stores it in a data.frame.
#'
#' If a column vector within x does not have an answer attribute acribed to it, then yes.answer and row.number can be used
#' to match which element of the vector the function should use instead to create the multiple choice frequency table.
#'
#'@param x a data.frame
#'@param yes.answer a character value that can be used to match which element of a column vector to use in creating 
#'the multiple choice frequency table.
#'@param row.number a numerical value that can be used to match which element of a column vector to use in creating 
#'the multiple choice frequency table.
#'
#'@return a data.frame
#'@export freq.mc

freq.mc  <- function(x, yes.answer=NULL, row.number=1){
  
  if (!is.data.frame(x))
    warning("x is not a data.frame. If x is a vector consider using freq.tab function instead")
  
  tab.list <- llply(x,freq.tab)
  
  row.out <- headers <- answers <- has.header <- has.answer <- vector(length=length(tab.list))
  for (i in 1:length(tab.list)){
    
    has.answer[i]        <- !is.null(attributes(tab.list[[i]])$answer)
    has.header[i]        <- !is.null(attributes(tab.list[[i]])$header)
    
    if (has.header[i])
      headers[i] <- attributes(tab.list[[i]])$header
    else
      headers[i] <- names(tab.list)[i]
    
    if (has.answer[i])
      answers[i] <- attributes(tab.list[[i]])$answer
    
    else{    
      if (!is.null(yes.answer)){
        if (yes.answer %in% rownames(tab.list[[i]]))
          answers[i] <- rownames(tab.list[[i]])[rownames(tab.list[[i]])==yes.answer]
      }
      else
        answers[i] <- rownames(tab.list[[i]])[row.number]
    }
  }
    
  dup.headers <-  duplicated(headers)
  
  if (unique(dup.headers[2:length(dup.headers)]))
    header <- headers[1]
  else
    header <- "" 
    
  no.cols <- vector(length=length(tab.list))
  for (i in 1:length(tab.list)) 
    no.cols[i] <- unlist(ncol(tab.list[[i]]))
  
  n.col           <- max(no.cols)
  n.rows          <- length(tab.list)
  out             <- data.frame(matrix(nrow=n.rows, ncol=n.col))
  names(out)      <- c("Antal svar", "Andel (%)")
  
  dup.answers <-  duplicated(answers)
  if (unique(dup.answers[2:length(dup.answers)]))
    if (!unique(dup.headers[2:length(dup.headers)]))
      row.names(out) <- headers  
  else
    row.names(out) <- answers

  
  for (i in 1:length(tab.list)){
    row.out[i] <- which(rownames(tab.list[[i]]) %in% answers[i])
    out[i,] <- tab.list[[i]][row.out[i],]
  }
  
  attributes(out)$header          <- header
  attributes(out)$type            <- "freq.mc"
  out
}          



