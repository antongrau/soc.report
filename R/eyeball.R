#' eyeball
#'
#' multiple crosstables & tabout
#' 
#'
#'@param dep a dataframe of dependent variables
#'@param vars a dataframe of independent variables
#'@param weight 
#'@param cell cell.proportions (only supports one form at the moment)
#'@param p.value 
#'@return an xlsx file with all tables with significant levels above p.value 
#'@export eyeball

eyeball  <- function(dep,
                     vars,
                     weight = NULL,
                     cell = "prop.c",
                     p.value = 0.05,
                     file = "eyeball.xlsx"){
          
          out.list <- out.count <- chi.list <- subtitles  <- list()
          
          for (i in 1:ncol(dep)){
                    out.list[[i]]                     <- apply(vars,2, cross.tab, cells = cell, weight = weight, dep[,i])
                    out.count[[i]]                    <- apply(vars,2, cross.tab, cells = "count", weight = weight, dep[,i])
                    suppressWarnings(chi.list[[i]]    <- apply(vars,2, chisq.test, dep[,i]))
                    subtitles[[i]]                    <- rep(names(dep[i]),length(vars))
          }
          titles              <- rep(names(vars),length(dep))
          unlisted            <- unlist(out.list, recursive=FALSE, use.names=FALSE)
          unlisted.count      <- unlist(out.count, recursive=FALSE, use.names=FALSE)  
          unlisted.chi        <- unlist(chi.list, recursive=FALSE, use.names=FALSE)  
          unlisted.sub        <- unlist(subtitles, recursive=FALSE, use.names=FALSE)  
          
          p.values <- vector()  
          for ( i in 1: length(unlisted.chi))
                    p.values <- c(p.values, unlisted.chi[[i]]$p.value)
          
          
          significant            <- which(p.values  <= p.value)
          significant.p.values   <- p.values[significant]
          significant.tables     <- unlisted[significant]
          significant.count      <- unlisted.count[significant]
          significant.titles     <- titles[significant]
          signifianct.subs       <- unlisted.sub[significant]
          
          for (i in 1:length(significant)){
                    attributes(significant.tables[[i]])$sub.header  <- signifianct.subs[i]
                    attributes(significant.count[[i]])$sub.header  <- signifianct.subs[i]
                    attributes(significant.tables[[i]])$footnote    <- significant.p.values[i]         
          }         
          
          tabout(significant.tables, headers = significant.titles, header.pos = 2, sheet.name ="prop.c", file=file)
          tabout(significant.count, headers = significant.titles, header.pos = 2, sheet.name ="freq", overwrite=F, file=file)
          
          
}
