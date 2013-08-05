######################################
### Report functions


####################################
#### Crosstable report
# Todo: Custom titles
# Todo: Custom footnotes
# Todo: Remove tables with the same variable as x and y
# Todo: Remove duplicate tables - tables with the same variables

#' Create a report with cross-tables
#' 
#' Takes a data.frame of factors and makes all possible two-dimensional crosstables (\link{tab.cross}). Only the tables that pass a \link{chisq.test} are printed.
#' @param variables is a data.frame of factors
#' @p.test is the threshold above which all tables are rejected.
#' @return several plotted tables
#' @export report.cross

report.cross <- function(variables, p.test=0.05){

  out.list    <- list()
  chi.list    <- list()
  titles.list <- list()
  
for (i in 1:ncol(variables)){
    out.list[[length(out.list)+1]]                  <- apply(variables, 2, tab.cross, y=variables[,i])
    suppressWarnings(chi.list[[length(chi.list)+1]] <- apply(variables, 2, chisq.test, y=variables[,i]))
    titles.list[[length(chi.list)+1]]               <- paste(colnames(variables)[i], "by", colnames(variables), sep=" ")
    
  }  

unlisted         <- unlist(out.list, recursive=FALSE, use.names=FALSE)
unlisted.chi     <- unlist(chi.list, recursive=FALSE, use.names=FALSE)
unlisted.titles  <- unlist(titles.list)

  #subset matrix
  subset.matrix <- matrix(1:length(unlisted), ncol=ncol(variables))
  subset.tables <- subset.matrix[lower.tri(subset.matrix)]
  
  
p.values <- vector()  
for ( i in 1: length(unlisted.chi)){
p.values <- c(p.values, unlisted.chi[[i]]$p.value)
}

significant            <- which(p.values  <= p.test)
subset                 <- subset.tables[subset.tables %in% significant]
significant.tables     <- unlisted[subset]
significant.titles     <- unlisted.titles[subset] 
  
# Plotting
for ( i in 1:length(significant.tables)){
grob.tab(significant.tables[[i]], title=significant.titles[i])
}
}

########################################
#### Report frequencies

#' Create a report with frequency tables
#' 
#' Takes a data.frame of factors and returns plotted frequency tables created by \link{tab.freq}
#' @param variables is a data.frame of factors
#' @return Several plotted tables
#' @export report.freq

# Todo: Add sum
# Todo: Custom titles
# Todo: Custom footnotes

report.freq <- function(variables){
  
out.list <- apply(variables, 2, tab.freq)
titles   <- colnames(variables) 

out.titles <- paste("Frekvenstabel for", titles)


# Plotting
for ( i in 1:length(out.list)){
  grob.tab(out.list[[i]], title=out.titles[i])
}
}

########################################
#### Report correlations
# Den er på en eller anden måde kontra intuitiv fordi en stor korrelation giver en lile streg.

library(ellipse)
# library(RColorBrewer)






# report.cor <- function(variables){
# 
# n          <- ncol(variables)  
# brew.col   <- brewer.pal(n, "Set3")
#   
# cor.mat    <- cor(variables, use="pairwise.complete.obs")
#   
# 
# 
# plotcorr(cor.mat, col= brew.col, numbers=FALSE)  
#   
# }
# 
# 
# 
# # variables <- with(directors, data.frame(phd, mba, hd, member_scientific, member_top_boards, member_america))
# # report.freq(variables)
# 
# 
# 
# 
# # report.cross(variables)
# 
# 
# 
