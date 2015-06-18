#' freq.tab
#'
#' Creates a simple frequency table and stores it in a data.frame.
#' In additiion it adds attributes (header, sub.header, answer, type) to the output data.frame.
#'
#'@param x a vector
#'@param weighs a numeric vector of weights
#'@param transpose transposes the table if set as TRUE
#'@param cells a vector indicating which cells to export.Default is c("count", "pct")
#'@return a data.frame
#'@export freq.tab

freq.tab <- function(x, transpose=FALSE, cells=c("count","pct"), weights=NULL, digits=3){
  
  count <- wtd.table(x, weights, type='table')
  count[(length(count)+1)] <- sum(count) # addmargins fucker med mig ved named numericals
  names(count)[length(count)] <- "Total"
  count <- round(count, digits)
  
  pct <- wpct(x, weights)
  pct[(length(pct)+1)] <- sum(pct)
  names(pct)[length(pct)] <- "Total"
  pct <- round(pct, digits)
  
  out <- cbind(count,pct)
  dimnames(out)[[2]] <- c("Antal", "Procent")
  
  out <- data.frame(out, check.names=FALSE)
  
  if (!"count" %in% cells){
    out <- data.frame(pct)
    names(out) <- "Procent"
  }
  
  if (!"pct" %in% cells){
    out <- data.frame(count)
    names(out) <- "Antal"
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
#' Creates a multiple choice frequency table from the column vectors in a data.frame.
#' 
#' When working with survey data one often finds oneself with a single variable for each element of a multiple choice question. 
#' This function performs a frequency table on each variable in x and compounds the yes.answer into a single frequency table 
#' and stores it in a data.frame.
#'
#' If a column vector within x does not have an answer attribute acribed to it, then yes.answer and row.number can be used
#' to match which element of the vector the function should use instead.
#'@param x a data.frame
#'@param yes.answer a character value that can be used to match which element of a column vector to use in creating 
#'the multiple choice frequency table.
#'@param row.number a numerical value that can be used to match which element of a column vector to use in creating 
#'the multiple choice frequency table.
#'
#'@return a data.frame
#'@export freq.mc

freq.mc  <- function(x, cells=c("count", "pct"), yes.answer=NULL, row.number=1,weight=NULL){
  
  if (!is.data.frame(x))
    warning("x is not a data.frame. If x is a vector consider using freq.tab function instead")
  
  tab.list <- llply(x,freq.tab, cells=cells, weight=weight)
  
  row.out <- headers <- sub.headers <- answers <- has.header <- has.sub.header <- has.answer <- vector(length=length(tab.list))
  for (i in 1:length(tab.list)){
    
    has.header[i]            <- !is.null(attributes(tab.list[[i]])$header)
    has.sub.header[i]        <- !is.null(attributes(tab.list[[i]])$sub.header)
    has.answer[i]            <- !is.null(attributes(tab.list[[i]])$answer)
    
    if (has.header[i])
      headers[i] <- attributes(tab.list[[i]])$header
    else
      headers[i] <- names(tab.list)[i]
    
    if (has.sub.header[i])
      sub.headers[i] <- attributes(tab.list[[i]])$sub.header
    
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
  
  if (!"count" %in% cells)
    names(out) <- "Procent"

  if (!"pct" %in% cells)
    names(out) <- "Antal"
  
  if ("count" %in% cells && "pct" %in% cells)
    names(out)      <- c("Antal", "Procent")
  
  dup.answers <-  duplicated(answers)
  dup.sub.headers <-  duplicated(sub.headers)
  
  if (unique(dup.answers[2:length(dup.answers)])){
    
    if (!unique(dup.headers[2:length(dup.headers)]))
      row.names(out) <- headers
    
    if (!unique(dup.sub.headers[2:length(dup.sub.headers)]))
      row.names(out) <- sub.headers
  }
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


#' freq.scale
#'
#' Creates a frequency table from the column vectors in a data.frame.
#' @param x a data.frame
#' @param weighs a numeric vector of weights
#' @param header add a header (for tabout)
#' @param cells a vector indicating which cells to export.Default is c("count", "pct")
#' @return a data.frame
#' @export freq.scale


freq.scale <- function(x,cells=c("count", "pct"), weight=NULL, header=NULL){
  
  if (is.data.frame(x)){
    tab.list.count <- llply(x,freq.tab, cells="count", weight=weight, transpose=T)
    tab.list.pct   <- llply(x,freq.tab, cells="pct", weight=weight, transpose=T)
  }
  else{
    tab.list.count <- list(freq.tab(x, cells="count", weight=weight, transpose=T))
    tab.list.pct <- list(freq.tab(x, cells="pct", weight=weight, transpose=T))
  }
  if (is.data.frame(x)){
    x.numeric       <- llply(x, as.numeric)
    Gennemsnit      <- laply(x.numeric, mean, na.rm=T)
  }
  else{
    x.numeric       <- laply(x, as.numeric)
    Gennemsnit      <- mean(x.numeric, na.rm=T)
  }
  
  Gennemsnit      <- round(Gennemsnit,2)
  
  
  Antal <- vector(length=length(tab.list.pct))
  
  for (i in 1:length(tab.list.count))
    Antal[i] <- unlist(tab.list.count[[i]][length(tab.list.count[[i]])])
  
  no.rows <- vector(length=length(tab.list.pct))
  no.col  <- ncol(tab.list.pct[[1]])+2
  out.pct <- data.frame(matrix(nrow=length(no.rows), ncol=no.col))
  
  for (i in 1:length(tab.list.pct)){
    out.pct[i,] <- cbind(tab.list.pct[[i]], Antal[i],Gennemsnit[i])
  }
  
  col.names <- vector("list",length=length(tab.list.pct))
  has.header <- has.sub.header <- headers <- sub.headers <- vector(length=length(tab.list.pct))
  
  for (i in 1:length(tab.list.pct)){
    col.names[[i]]        <- attributes(tab.list.pct[[i]])$names
    has.header[i]         <- !is.null(attributes(tab.list.pct[[i]])$header)
    has.sub.header[i]     <- !is.null(attributes(tab.list.pct[[i]])$sub.header)
    
    if (has.header[i])
      headers[i]    <- unlist(attributes(tab.list.pct[[i]])$header)
    #                     else 
    #                               headers[i]    <- attributes(tab.list.pct)$names[i]
    
    if (has.sub.header[i])
      sub.headers[i]    <- unlist(attributes(tab.list.pct[[i]])$sub.header)  
    
    else 
      sub.headers[i]    <- attributes(tab.list.pct)$names[i]
  }
  
  if (!is.null(header))
    header <- header
  else{
    
    if(has.header[1])
      header <- headers[1]
  }
  
  row.names(out.pct) <- sub.headers
  names(out.pct)     <- c(col.names[[1]],"Antal", "Gennemsnit")
  
  attributes(out.pct)$header <- header
  attributes(out.pct)$answer <- sub.headers
  attributes(out.pct)$type <- "frec.scale"
  out.pct
}



