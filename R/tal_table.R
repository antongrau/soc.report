#' Crosstable in the &tal style
#'
#' @param x a factor
#' @param y a factor
#' @param freq if TRUE frequencies placed within ()
#' @param prop.margin index, or vector of indices to generate margin for, \link{prop.table}
#' @param digits the maximum number of digits, \link{round}
#' @param vector of dimensions over which to form margins. Margins are formed in the order in which dimensions are specified in margin. \link{addmargins}
#' @param stat if TRUE returns various statistical tests
#' @param color sets the color of the main lines
#' @param min.width is the minimum width of the table, see \link{standard.with.table}
#' @param title sets the title
#' @export tal.cross

tal.cross <- function(x, y, prop.margin=1, digits=2, margin=1:2, stat=TRUE,
           color=rgb(144,26,30, maxColorValue =255), title="", min.width=120){
  
    require("vcd")
    
    freq           <- table(x,y)
    tab.cross.out  <- tab.cross(x,y, prop.margin=prop.margin, digits=digits, margin=margin)
        
    ## statistical testing for the footnote
    chi            <- suppressWarnings(chisq.test(x,y))
    assoc          <- assocstats(freq)
    cramer         <- round(assoc$cramer,5)
    phi            <- round(assoc$phi,5)
    chi            <- round(chi$p.value,5)
    stat.test      <- paste("Pearsons p-værdi =",chi,",", "phi =",phi, ",", "cramer's V =",cramer, sep=" ")
    if(identical(stat, FALSE)) stat.test <- ""
    
    ## Standard.width
    st.tab         <- standard.width.table(tab.cross.out, min.width) 
    
    ## stat.grob 
    grob.tab(st.tab, style="academic", footnote=stat.test, title=title, main.line.color=color)
}

#' Frequency table in the &tal style
#' 
#' @param x is a factor or character vector
#' @param digits integer indicating the number of decimal places
#' @param title sets the title
#' @param min.width is the minimum width of the table, see \link{standard.width.table}
#' @param footnote is the text displayed below the table
#' @export tal.freq

tal.freq <- function(x, digits=2, color=rgb(144,26,30, maxColorValue =255), title="", min.width=120, footnote=""){
  
  
  tab.freq.out  <- tab.freq(x, digits=digits)
  
  ## Standard.width
  st.tab         <- standard.width.table(tab.freq.out, min.width) 
  
  ## stat.grob 
  grob.tab(st.tab, style="academic", footnote=footnote, title=title, main.line.color=color)
}

