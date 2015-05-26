#' prepare.lime.data
#' 
#' 
#' @param x a data.frame
#' @return a data.frame
#' @export

prepare.lime.data <- function(x){

          if (!is.data.frame(x))
                    stop("x is not a data.frame")
          
          var.lab <- attributes(x)$variable.labels
          headers <- answers <- vector("list", length=length(x))
          
          for (i in 1:length(x)){
                    headers[i] <- gsub("(.*?)\\[.*?\\]", "\\1",var.lab[i])
                    answers[i] <- gsub(".*\\[(.*)\\].*", "\\1", var.lab[i])
                    
                    attributes(x[,i])$header <- headers[i]
                    #attributes(df[,i])$answer <- answers[i]
                    
                    if (is.factor(x[,i]) && c("Not selected") %in% levels(x[,i])){
                              x[,i] <- revalue(x[,i], c("Yes" = answers[[i]]))
                              attributes(x[,i])$answer <- answers[i]
                    }
                    
                    if (is.factor(x[,i]) && !headers[i] %in% answers[i] && !c("Not selected") %in% levels(x[,i])){
                              attributes(x[,i])$sub.header <- answers[i]
                    }
                    
          }        
          x
}