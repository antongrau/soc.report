###############################
### Cleaning functions

#'  Recode very small levels into Missing values
#'  
#'  Kill.the.small recodes all levels with proportions below a certain threshold into "Missing".
#'  This is useful for a very crude first look at a lot of variables.
#'  @param variables is a data.frame of factors
#'  @param threshold is the proportion below which a level is recoded
#'  @param new.level is the new label for the recoded levels
#'  @return a data.frame
#'  @export kill.the.small

kill.the.small <- function(variables, threshold=0.05, new.level="Missing"){

  for (i in 1:ncol(variables)){
prop.var      <- prop.table(table(variables[,i]))
kill.levels   <- names(prop.var)[prop.var < threshold]  
kl            <- levels(variables[,i]) %in% kill.levels
levels(variables[,i])[kl]  <- new.level
 }
variables
}


#' NA to Missing
#' 
#' Recode all NA values into missing in a data.frame of factors
#' 
#' @param x is a data.frame of factors
#' @param missing.value is the new value given to the NA values
#' @export
#' @examples
#'
#' x <- as.data.frame(matrix(rep(c("a", "b", "c", NA), 3), ncol=3))
#' x
#' na.to.missing(x)

na.to.missing <- function(x, missing.value="MISSING"){

 if (all(unlist(lapply(x, is.factor)))==FALSE) stop("Not all columns in x are factors")
  
  
missing.cols  <- which(colSums(is.na(x)) > 0)
missing.x     <- x[,missing.cols] 

for ( i in 1:ncol(missing.x)){
  levels(missing.x[,i])             <- c(levels(missing.x[,i]), missing.value)
  missing.x[is.na(missing.x[,i]),i] <- missing.value 
}
x[,missing.cols]                    <- missing.x
x
}
  
  