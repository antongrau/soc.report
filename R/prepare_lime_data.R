#' prepare.lime.data
#' 
#' Adds a number of attributes (header, sub.header, answer) to each variable in a data.frame. 
#' In addition it revalue the level "Yes" in multiple choice variables from Limesurvey writh the question text.
#' 
#' @param x a data.frame
#' @return a data.frame
#' @export

prepare.lime.data <- function(x){

          if (!is.data.frame(x))
            stop("x is not a data.frame")
        
          if (is.null(attributes(x)$variable.labels))
            stop("x lacks variable.labels")
          
          var.lab <- attributes(x)$variable.labels
          headers <- answers <- vector("list", length=length(x))
          
          for (i in 1:length(x)){
                    headers[i] <- gsub("(.*?)\\[.*?\\]", "\\1",var.lab[i])
                    answers[i] <- gsub(".*\\[(.*)\\].*", "\\1", var.lab[i])
                    
                    headers[i] <- str_trim(headers[[i]])
                    answers[i] <- str_trim(answers[[i]])
                    
                    attributes(x[,i])$header <- headers[i]
                    
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