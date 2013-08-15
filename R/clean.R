###############################
### Cleaning functions

#'  Recode very small levels into Missing values
#'  
#'  Kill.the.small recodes all levels with proportions below a certain threshold into "Missing".
#'  This is useful for a very crude first look at a lot of variables.
#'  @param variables is a data.frame of factors
#'  @threshold is the proportion below which a level is recoded
#'  @new.level is the new label for the recoded levels
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


