# OGTAL.FUNKTIONER

# Indeholder pt. 4 funktioner og 3 themes: 

# FUNKTIONER
# Frekvens
# Kryds.row
# Kryds.col
# plot.bar

# THEMES
# theme.plot.bar
# theme.plot.bar.flip
# theme.plot.bar.3      <- plot af multiple answers, flip=T

library(gridExtra)
library(car)
library(ggplot2)
library(reshape)
library(scales)

################
#   THEMES     #
################
theme.plot.bar <- function(){
  
  theme(
    text                   = element_text(face = "plain", colour = "black", size = 12, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
    axis.text.x            = element_text(size = 11, colour = "black"),
    axis.text.y            = element_text(size = 11, colour = "black", hjust = 1),
    strip.text             = element_text(size = rel(0.8)),
    axis.ticks             = element_blank(),
    axis.title.x           = element_text(), 
    axis.title.y           = element_text(angle = 90),
    panel.background       = element_rect(fill = NA ,colour = NA),
    panel.grid.major.x     = element_blank(),
    panel.grid.minor       = element_blank(),
    panel.grid.major.y     = element_line(colour = "grey40", linetype= "dotted", size =0.6),
    strip.text.x           = element_text(),
    #strip.text.y           = element_text(angle = -90),
    #plot.background        = element_rect(colour = "grey40", size = 0.6, linetype= 1, fill="white"), 
    plot.margin            = unit(c(0, 0, 0, 0), "lines"),
    plot.title             = element_text(size = 10, hjust=1),
    complete               = FALSE)
  
}


theme.plot.bar.flip <- function(){
  
  theme(
    text                   = element_text(face = "plain", colour = "black", size = 12, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
    axis.text.y            = element_text(size = 11, colour = "black", vjust=0.5, hjust=0),
    axis.text.x            = element_text(size = 11, colour = "black", hjust = 0.5),
    strip.text             = element_text(size = rel(0.8)),
    axis.ticks             = element_blank(),
    axis.title.x           = element_text(), 
    axis.title.y           = element_text(angle = 90),
    panel.background       = element_rect(fill = NA ,colour = NA),
    panel.border           = element_blank(),
    panel.grid.major.y     = element_blank(),
    panel.grid.minor       = element_blank(),
    panel.grid.major.x     = element_line(colour = "grey40", linetype= "dotted", size =0.6),
    strip.text.x           = element_text(),
    strip.text.y           = element_text(angle = -90),
    #plot.background        = element_rect(colour = "grey40", size = 0.8, linetype= 1, fill="white"), 
    plot.margin            = unit(c(0, 0, 0, 0), "lines"),
    plot.title             = element_text(size = 10, hjust=1),
    complete               = FALSE)
  
}

theme.plot.bar.flip.3 <- function(){
  
  theme(
    text                   = element_text(face = "plain", colour = "black", size = 12, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
    axis.text.y            = element_text(size = 11, colour = "black", vjust=0.5, hjust=0),
    axis.text.x            = element_text(size = 11, colour = "black", hjust = 0.5),
    strip.text             = element_text(size = rel(0.8)),
    axis.ticks             = element_blank(),
    axis.title.x           = element_text(), 
    axis.title.y           = element_text(angle = 90),
    panel.background       = element_rect(fill = NA ,colour = NA),
    panel.border           = element_blank(),
    panel.grid.major.y     = element_blank(),
    panel.grid.minor       = element_blank(),
    panel.grid.major.x     = element_line(colour = "grey40", linetype= "dotted", size =0.6),
    strip.text.x           = element_text(),
    strip.text.y           = element_text(angle = -90),
    #plot.background        = element_rect(colour = "grey40", size = 0.8, linetype= 1, fill="white"), 
    plot.margin            = unit(c(0.5, 0, 0, 0), "lines"),
    plot.title             = element_text(size = 10, hjust=1),
    legend.position        = c(0.5, 1),
    legend.direction       = "horizontal",
    legend.title           = element_blank(),
    legend.key.size        = unit(0.7, "lines"),
    complete               = FALSE)
  
}


theme.plot.bar.3 <- function(){
  
  theme(
    line                   = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
    rect                   = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text                   = element_text(family = "sans", face = "plain", colour = "black", size = 12, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
    axis.text.y            = element_text(size = 11, colour = "black", hjust=0, vjust=0.5),
    axis.text.x            = element_text(size = 11, colour = "black", hjust=0.5),
    strip.text             = element_text(size = rel(0.8)),
    axis.text.x            = element_text(vjust = 1),
    axis.text.y            = element_text(hjust = 1), 
    axis.ticks             = element_blank(),
    axis.title.x           = element_text(), 
    axis.title.y           = element_text(angle = 90),
    panel.background       = element_rect(fill = NA ,colour = NA),
    panel.border           = element_blank(),
    panel.grid.major.x     = element_blank(),
    panel.grid.minor       = element_blank(),
    panel.grid.major.y     = element_line(colour = "grey40", linetype= "dotted", size =0.6),
    strip.text.x           = element_text(),
    strip.text.y           = element_text(angle = -90),
    plot.margin            = unit(c(0.5, 0, 0, 0), "cm"),
    panel.margin           = unit(0, "lines"),
    legend.position        = c(0.5, 1),
    legend.direction       = "horizontal",
    legend.title           = element_blank(),
    legend.key.size        = unit(0.7, "lines"),
    complete               = FALSE)
  
}

#################
#  FUNKTIONER   #
#################


frekvens <- function(x, digits=3, p=100, decor="%", sort=FALSE,
                     red=144, green=26, blue= 30, titel="", note=""){

  x <- factor(x)
  
  longest       <- 0
  longestnumber <- 0
  
  for (i in 1:length(levels(x))){
    
    if (longest < nchar(levels(x))[i]){
      longest <- nchar(levels(x))[i] 
      longestnumber <- i
    } 
  }
  
  for (i in 1:(100-longest)){
    levels(x)[longestnumber] <- paste(levels(x)[longestnumber], " ", sep="")
  }
  
  g <- table(x)
  s <- as.character(sum(g))
  s <- paste("Antal besvarelser:", s, sep=" ")
  j <- prop.table(g)  
  #k <- c(rownames(j), "Total")
  
  z   <- sort(j, decreasing=T)
  gnu <- if (sort == TRUE) (z) else (j)
  
  j <- addmargins(gnu, FUN = sum)
  j <- round(j, digits)
  j[]<-paste(j*p, decor, sep=" ")
  
  j <- as.matrix(j)
  
  farve <- rgb(red,green,blue, maxColorValue =255)
  
  table <-   tableGrob(j,
                       #rows = k,
                       gpar.coretext = gpar(fontsize = 12),            
                       gpar.coltext  = gpar(fontsize = 12,col="white"),            
                       gpar.rowtext  = gpar(fontsize = 12, fontface="bold"),            
                       gpar.corefill = gpar(fill = rgb(255,255,255, maxColorValue =255), alpha = 1, col = NA),
                       gpar.rowfill  = gpar(fill = rgb(255,255,255, maxColorValue =255), alpha = 1, col = NA),           
                       gpar.colfill  = gpar(fill = farve, alpha = 1, col= "white"),                       
                       equal.width   = FALSE,            
                       show.rownames = TRUE,            
                       show.rsep     = TRUE, 
                       show.hlines   = TRUE,                               
                       show.csep     = FALSE, 
                       show.vlines   = FALSE,
                       show.box      = FALSE,
                       padding.h     = unit(15, "mm"),            
                       padding.v     = unit(8, "mm"),
                       core.just     = "center", 
                       row.just      = "left",
                       separator     = farve)
  
  hh <- grobHeight(table)
  ww <- grobWidth(table)
  
  svarpersoner       <- textGrob(s, 
                                 x=unit(0.5,"npc") + 0.5*ww -unit(5, "mm"),
                                 y=unit(0.5,"npc") + 0.5*hh + unit(2, "mm"), 
                                 vjust=0, hjust=1,gp=gpar(fontsize=10))
  
  border <- roundrectGrob(x=0.5, y=0.5, width=ww, height=hh,
                          default.units="npc",
                          r=unit(1.2, "char"),
                          just="center",
                          name=NULL, gp=gpar(col="white", fill=farve, vp=NULL)) 
  
  border2 <- roundrectGrob(x=0.5, y=0.5, width=ww, height=hh,
                           default.units="npc",
                           r=unit(1.2, "char"),
                           just="center",
                           name=NULL, gp=gpar(fill=NA, col=farve, vp=NULL))
  
  title <- textGrob(titel,
                    x=unit(0.5,"npc"), 
                    y=unit(0.5,"npc") +0.5*hh + unit(8, "mm"), 
                    vjust=0,hjust=0.5, gp=gpar(fontsize=15, fontface="bold"))
  
  footnote <- textGrob(note, 
                       x=unit(0.5,"npc") - 0.5*ww + unit(5,"mm"),
                       y=unit(0.5,"npc") - 0.5*hh, 
                       vjust=1, hjust=0,gp=gpar( fontsize=10))
  
  grid.newpage()
  gt <- gTree(children=gList(border, table,border2, svarpersoner, title, footnote))
  grid.draw(gt)
  
}


kryds.row <- function(x,y, p=100, decor="%", digits=3,
                      titel="", note="", red=144, green=26, blue= 30){
  
  c <- table(x, y)
  s <- as.character(sum(c))
  s <- paste("Antal besvarelser:", s, sep=" ")
  j <- prop.table(c,1)
  r <- c(rownames(j),"Total")
  k <- c(colnames(j), "Total")
  j <- addmargins(j, margin =2, FUN = sum)
  j <- round(j, digits)
  j[]<-paste(j*p, decor, sep=" ")
  
  farve <- rgb(red,green,blue, maxColorValue =255)
  
  table     <-   tableGrob(j,
                       cols = k,
                       gpar.coretext = gpar(fontsize = 12),            
                       gpar.coltext  = gpar(fontsize = 12,col="white"),            
                       gpar.rowtext  = gpar(fontsize = 12, fontface="bold"),            
                       gpar.corefill = gpar(fill = rgb(255,255,255, maxColorValue =255), alpha = 1, col = NA),
                       gpar.rowfill  = gpar(fill = rgb(255,255,255, maxColorValue =255), alpha = 1, col = NA),           
                       gpar.colfill  = gpar(fill = 0, alpha = 1 ,col= "white"),                       
                       equal.width   = TRUE,            
                       show.rownames = TRUE,            
                       show.rsep     = TRUE, 
                       show.hlines   = TRUE,                               
                       show.csep     = FALSE, 
                       show.vlines   = FALSE,
                       show.box      = FALSE,
                       padding.h     = unit(10, "mm"),            
                       padding.v     = unit(8, "mm"),
                       core.just     = "center", 
                       row.just      = "left",
                       separator     = farve)
  
  
  hh <- grobHeight(table)
  ww <- grobWidth(table)
  
  border <- roundrectGrob(x=0.5, y=0.5, width=ww, height=hh,
                          default.units="npc",
                          r=unit(1.2, "char"),
                          just="center",
                          name=NULL, gp=gpar(col="white", fill=farve, vp=NULL)) 
  
  border2 <- roundrectGrob(x=0.5, y=0.5, width=ww, height=hh,
                           default.units="npc",
                           r=unit(1.2, "char"),
                           just="center",
                           name=NULL, gp=gpar(fill=NA, col=farve, vp=NULL))
  
  title <- textGrob(titel,
                    x=unit(0.5,"npc") -0.5*ww + unit(5, "mm"), 
                    y=unit(0.5,"npc") +0.5*hh + unit(2, "mm"), 
                    vjust=0,hjust=0, gp=gpar(fontsize=12, fontface="bold"))
  
  footnote <- textGrob(note, 
                       x=unit(0.5,"npc") - 0.5*ww + unit(5,"mm"),
                       y=unit(0.5,"npc") - 0.5*hh, 
                       vjust=1, hjust=0,gp=gpar( fontsize=10))
  
  svarpersoner       <- textGrob(s, 
                                 x=unit(0.5,"npc") + 0.5*ww -unit(5, "mm"),
                                 y=unit(0.5,"npc") + 0.5*hh + unit(2, "mm"), 
                                 vjust=0, hjust=1,gp=gpar( fontsize=10))
  grid.newpage()
  gt <- gTree(children=gList(border,table,border2, title, footnote, svarpersoner))
  grid.draw(gt)
  
}


kryds.col <- function(x,y, p=100, decor="%", digits=3,
                      titel="", note="", red=144, green=26, blue= 30){
  
  g <- table(x, y)
  s <- as.character(sum(g))
  s <- paste("Antal besvarelser:", s, sep=" ")
  j <- prop.table(g,2)
  r <- c(rownames(j),"Total")
  k <- c(colnames(j), "Total")
  j <- addmargins(j, margin =1, FUN = sum)
  j <- round(j, digits)
  j[]<-paste(j*p, decor, sep=" ")
  
  farve <- rgb(red,green,blue, maxColorValue =255)
  
  table <-   tableGrob(j,
                       rows=r,
                       gpar.coretext = gpar(fontsize = 12),            
                       gpar.coltext  = gpar(fontsize = 12,col="white"),            
                       gpar.rowtext  = gpar(fontsize = 12, fontface="bold"),            
                       gpar.corefill = gpar(fill = rgb(255,255,255, maxColorValue =255), alpha = 1, col = NA),
                       gpar.rowfill  = gpar(fill = rgb(255,255,255, maxColorValue =255), alpha = 1, col = NA),           
                       gpar.colfill  = gpar(fill = 0, alpha = 1, col= "white"),                       
                       equal.width   = TRUE,            
                       show.rownames = TRUE,            
                       show.rsep     = FALSE, 
                       show.hlines   = FALSE,                               
                       show.csep     = TRUE, 
                       show.vlines   = FALSE,
                       show.box      = FALSE,
                       padding.h     = unit(25, "mm"),            
                       padding.v     = unit(8, "mm"),
                       core.just     = "center", 
                       row.just      = "left",
                       separator     = farve)
  
  hh <- grobHeight(table)
  ww <- grobWidth(table)
  
  border <- roundrectGrob(x=0.5, y=0.5, width=ww, height=hh,
                          default.units="npc",
                          r=unit(1.2, "char"),
                          just="center",
                          name=NULL, gp=gpar(col="white", fill=farve, vp=NULL)) 
  
  border2 <- roundrectGrob(x=0.5, y=0.5, width=ww, height=hh,
                           default.units="npc",
                           r=unit(1.2, "char"),
                           just="center",
                           name=NULL, gp=gpar(fill=NA, col=farve, vp=NULL))
  
  title <- textGrob(titel,
                    x=unit(0.5,"npc") -0.5*ww + unit(5, "mm"), 
                    y=unit(0.5,"npc") +0.5*hh + unit(2, "mm"), 
                    vjust=0,hjust=0, gp=gpar(fontsize=12, fontface="bold"))
  
  footnote <- textGrob(note, 
                       x=unit(0.5,"npc") - 0.5*ww + unit(5,"mm"),
                       y=unit(0.5,"npc") - 0.5*hh, 
                       vjust=1, hjust=0,gp=gpar( fontsize=10))
  
  svarpersoner       <- textGrob(s, 
                                 x=unit(0.5,"npc") + 0.5*ww -unit(5, "mm"),
                                 y=unit(0.5,"npc") + 0.5*hh + unit(2, "mm"), 
                                 vjust=0, hjust=1,gp=gpar( fontsize=10))
  
  
  grid.newpage()
  gt <- gTree(children=gList(border,table,border2, title, footnote, svarpersoner))
  grid.draw(gt)
}


plot.bar <- function(var, var.lab=FALSE, flip=TRUE, sort=TRUE, lim=100, 
                     red=144, blue=26, green=30, set.na=FALSE, na="Nej", andet=FALSE){
  
  b       <- c(0,10,20,30,40,50,60,70,80,90,100)
  l       <- c("0%","", "20%","", "40%","", "60%","", "80%","", "100%")
  
  if (is.data.frame(var) == F){
    
    df            <- data.frame(var)
    df            <- data.frame(lapply(df, factor))
    
    df            <- na.omit(df)
    
    if (length(set.na) == 1){
      df  <- data.frame(df[df[,1] != set.na[1],])
    }
    
    if (length(set.na) == 2){
      df  <- data.frame(df[df[,1] != set.na[1],])
      df  <- data.frame(df[df[,1] != set.na[2],])
    }
    
    if (length(set.na) == 3){
      df  <- data.frame(df[df[,1] != set.na[1],])
      df  <- data.frame(df[df[,1] != set.na[2],])
      df  <- data.frame(df[df[,1] != set.na[3],])
    }
    
    if (length(set.na) == 4){
      df  <- data.frame(df[df[,1] != set.na[1],])
      df  <- data.frame(df[df[,1] != set.na[2],])
      df  <- data.frame(df[df[,1] != set.na[3],])
      df  <- data.frame(df[df[,1] != set.na[4],])
    }
    
    df$id         <- c(1:as.numeric(length(df[,1])))
    
    dfm           <- melt(df, id="id")
    df2           <- cast(dfm, value~variable, fun = length)
    
    s             <- as.character(sum(df2[,2]))
    s             <- paste("Antal besvarelser:", s, sep=" ")
    
    df2[,2]       <- round(df2[,2] / sum(df2[,2]) * 100, 1)
    df2           <- as.data.frame(df2)
    names(df2)    <- c("v1","v2")
    df2$v1        <- factor(df2$v1)
    
    if (flip == TRUE){
      
      levels(df2$v1)  <- gsub('(.{25})\\s+', '\\1\n\\2',levels(df2$v1))
      
      if (sort == TRUE){
        nif            <- order(df2$v2)
        df2$v1         <- factor(df2$v1,levels(df2$v1)[c(nif)])
      }
      
      if (sort == FALSE){
        nif            <- length(levels(df2$v1))
        df2$v1         <- factor(df2$v1,levels(df2$v1)[c(nif:1)])
      }
      
      out <- ggplot(df2, aes(x=v1, y=v2))+
        geom_bar(stat="identity", fill=rgb(red,green,blue, maxColorValue=255), width=0.4)+
        scale_y_continuous(name="", limits=c(0,lim), breaks=b, labels=l)+
        xlab("")+
        geom_text(aes(label = v2),hjust=-0.3, size=4, color="black")+
        coord_flip()+
        ggtitle(s)+
        theme.plot.bar.flip()
    }
    
    if (flip == FALSE){
      
      levels(df2$v1)  <- gsub('(.{10})\\s+', '\\1\n\\2',levels(df2$v1))
      
      if (sort == TRUE){
        nif            <- order(-df2$v2)
        df2$v1         <- factor(df2$v1,levels(df2$v1)[c(nif)])
      }
      
      out <- ggplot(df2, aes(x=v1, y=v2))+
        geom_bar(stat="identity", fill=rgb(red,green,blue, maxColorValue=255), width=0.4)+
        scale_y_continuous(name="", limits=c(0,lim), breaks=b, labels=l)+
        xlab("")+
        geom_text(aes(label = v2),vjust=-0.3, size=4, color="black")+
        ggtitle(s)+
        theme.plot.bar()
    }}
  
  else {
    
    df        <- data.frame(lapply(var, factor))
    
    if (var.lab != FALSE){
      names(df)   <- var.lab
    }
    
    if (ncol(df) == 1){
      
      df1            <- na.omit(df)
      
      if (length(set.na) == 1){
        df  <- data.frame(df[df[,1] != set.na[1],])
      }
      
      if (length(set.na) == 2){
        df  <- data.frame(df[df[,1] != set.na[1],])
        df  <- data.frame(df[df[,1] != set.na[2],])
      }
      
      if (length(set.na) == 3){
        df  <- data.frame(df[df[,1] != set.na[1],])
        df  <- data.frame(df[df[,1] != set.na[2],])
        df  <- data.frame(df[df[,1] != set.na[3],])
      }
      
      if (length(set.na) == 4){
        df  <- data.frame(df[df[,1] != set.na[1],])
        df  <- data.frame(df[df[,1] != set.na[2],])
        df  <- data.frame(df[df[,1] != set.na[3],])
        df  <- data.frame(df[df[,1] != set.na[4],])
      }
      
      df1$id         <- c(1:as.numeric(length(df1[,1])))
      
      dfm           <- melt(df1, id="id")
      df2           <- cast(dfm, value~variable, fun = length)
      
      s             <- as.character(sum(df2[,2]))
      s             <- paste("Antal besvarelser:", s, sep=" ")
      
      df2[,2]       <- round(df2[,2] / sum(df2[,2]) * 100, 1)
      df2           <- as.data.frame(df2)
      names(df2)    <- c("v1","v2")
      df2$v1        <- factor(df2$v1)
      
      if (flip == TRUE){
        
        levels(df2$v1)  <- gsub('(.{25})\\s+', '\\1\n\\2',levels(df2$v1))
        
        if (sort == TRUE){
          nif            <- order(df2$v2)
          df2$v1         <- factor(df2$v1,levels(df2$v1)[c(nif)])
        }
        
        if (sort == FALSE){
          nif            <- length(levels(df2$v1))
          df2$v1         <- factor(df2$v1,levels(df2$v1)[c(nif:1)])
        }
        
        out <- ggplot(df2, aes(x=v1, y=v2))+
          geom_bar(stat="identity", fill=rgb(red,green,blue, maxColorValue=255), width=0.4)+
          scale_y_continuous(name="", limits=c(0,lim), breaks=b, labels=l)+
          xlab("")+
          geom_text(aes(label = v2),hjust=-0.3, size=4, color="black")+
          coord_flip()+
          ggtitle(s)+
          theme.plot.bar.flip()
      }
      
      if (flip == FALSE){
        
        levels(df2$v1)  <- gsub('(.{10})\\s+', '\\1\n\\2',levels(df2$v1))
        
        if (sort == TRUE){
          nif            <- order(-df2$v2)
          df2$v1         <- factor(df2$v1,levels(df2$v1)[c(nif)])
        }
        
        out <- ggplot(df2, aes(x=v1, y=v2))+
          geom_bar(stat="identity", fill=rgb(red,green,blue, maxColorValue=255), width=0.4)+
          scale_y_continuous(name="", limits=c(0,lim), breaks=b, labels=l)+
          xlab("")+
          geom_text(aes(label = v2),vjust=-0.3, size=4, color="black")+
          ggtitle(s)+
          theme.plot.bar()
      }
      
    }
    
    if (ncol(df) >=2){
      
      if (andet == TRUE){        
        df[,ncol(df)]  <- is.na(df[,ncol(df)])
        df[,ncol(df)] <- ifelse(df[,ncol(df)] == TRUE, na, "Ja")
      }
      
      n.mat     <- df == na
      df        <- df[rowSums(n.mat) != ncol(df),]  # fjern dem som har svaret nej til alle spm(de har formodenlig ikke svaret)
      
      df$id     <- c(1:as.numeric(length(df[,1])))
      
      s         <- as.character(length(df[,1]))
      s         <- paste("Antal besvarelser:", s, sep=" ")
      
      dfm       <- melt(df, id="id")
      df2       <- cast(dfm, value~variable, fun = length)
      
      df2[, 2:ncol(df2)]      <- round(df2[, 2:ncol(df2)] / colSums(df2[, 2:ncol(df2)]) * 100, 1)
      df2                     <- melt(df2)
      df2                     <- df2[df2$value != na,]
      
      df2                     <- as.data.frame(df2)
      names(df2)              <- c("v1","v2","v3")
      
      if (flip == TRUE){
        
        levels(df2$v3)  <- gsub('(.{25})\\s+', '\\1\n\\2',levels(df2$v3))
        
        if (sort == TRUE){
          nif            <- order(df2$v2)
          df2$v3         <- factor(df2$v3,levels(df2$v3)[c(nif)])
        }
        
        if (sort == FALSE){
          nif            <- c(ncol(df):1)
          df2$v3         <- factor(df2$v3,levels(df2$v3)[c(nif)])
        }
        
        out <- ggplot(df2, aes(x=v3, y=v2))+
          geom_bar(stat="identity", fill=rgb(red,blue,green, maxColorValue=255), width=0.4)+
          scale_y_continuous(name="", limits=c(0,lim), breaks=b, labels=l)+
          xlab("")+
          geom_text(aes(label = v2),hjust=-0.3, size=4, color="black")+
          coord_flip()+
          ggtitle(s)+
          theme.plot.bar.flip()
        
      }
      
      if (flip == FALSE){
        
        levels(df2$v3)  <- gsub('(.{10})\\s+', '\\1\n\\2',levels(df2$v3))
        
        if (sort == TRUE){
          nif            <- order(-df2$v2)
          df2$v3         <- factor(df2$v3,levels(df2$v3)[c(nif)])
        }
        
        out <- ggplot(df2, aes(x=v3, y=v2))+
          geom_bar(stat="identity", fill=rgb(red,blue,green, maxColorValue=255), width=0.4)+
          scale_y_continuous(name="", limits=c(0,lim), breaks=b, labels=l)+
          xlab("")+
          geom_text(aes(label = v2),vjust=-0.3, size=4, color="black")+
          ggtitle(s)+
          theme.plot.bar()
      }}}
  out
}



plot.fill <- function(var,var.lab=FALSE, set.na=FALSE, flip=TRUE, cutoff=5, type="stack"){
  
  df        <- data.frame(lapply(var, factor))
  
  if (var.lab != FALSE){
    names(df)   <- var.lab
  }
  
  df        <- na.omit(df)
  df$id     <- c(1:as.numeric(length(df[,1])))
  
  dfm       <- melt(df, id="id")
  df2       <- cast(dfm, value~variable, fun = length)
  
  if(set.na != FALSE){
    
    l            <- length(set.na)
    
    if(l == 1){
      df2 <- df2[df2$value != set.na[1],]
    }
    
    if(l == 2){
      df2 <- df2[df2$value != set.na[1],]
      df2 <- df2[df2$value != set.na[2],]
    }
    
    if(l == 3){
      df2 <- df2[df2$value != set.na[1],]
      df2 <- df2[df2$value != set.na[2],]
      df2 <- df2[df2$value != set.na[3],]
      
    }}
  #################
  
  df2[, 2:ncol(df2)]      <- round(df2[, 2:ncol(df2)] / colSums(df2[, 2:ncol(df2)]) * 100, 1)
  df2                     <- melt(df2)
  
  if(flip == TRUE){
    levels(df2$variable)    <- gsub('(.{15})\\s+','\\1\n\\2',levels(df2$variable))}
  else{
    levels(df2$variable)    <- gsub('(.{10})\\s+','\\1\n\\2',levels(df2$variable))
  }
  
  df2        <- ddply(df2, .(variable), transform, pos = cumsum(value.1) - 0.5*value.1)
  df2        <- as.data.frame(df2)
  names(df2) <- c("v1","v2","v3", "pos")
  
  for(i in nrow(df2)){
    df2$pos[df2$v2 < cutoff] <- NA
  }
  
  
  #df2$v3 <- factor(df2$v3, levels(df2$v3)[order(df2$v2)])
  
  if(type=="dodge"){
    
    q <- ggplot(df2, aes(x=v3, fill=v1, y=v2))+
      geom_bar(aes(y=v2), stat="identity", color="grey40", position = position_dodge(), width=0.8)+
      #scale_fill_manual(values=c("#2E8B57","#3CB371","#DB2929","#8B0000"))+
      scale_y_continuous(name="", breaks=c(0,20,40,60,80,100), labels=c("0%", "20%", "40%", "60%", "80%", "100%"))+
      xlab("")+
      #geom_text(aes(label = v2), size = 4.5, color="black", hjust=-0.3, position = position_dodge())+
      coord_flip()+
      theme.plot.bar.flip.3()
    
    
    r <- ggplot(df2, aes(x=v3, fill=v1, y=v2))+
      geom_bar(aes(y=v2), stat="identity", color="grey40", position = position_dodge(), width=0.8)+
      #scale_fill_manual(values=c("#2E8B57","#3CB371","#DB2929","#8B0000"))+
      scale_y_continuous(name="", breaks=c(0,20,40,60,80,100), labels=c("0%", "20%", "40%", "60%", "80%", "100%"))+
      xlab("")+
      #geom_text(aes(label = v2), size = 4.5, color="black", vjust=-0.3, position = position_dodge())+
      theme.plot.bar.flip.3()
    
    out <- if (flip == TRUE) (q) else (r)
    
  }
  
  if(type=="stack"){
    
    q <- ggplot(df2, aes(x=v3, fill=v1, y=v2))+
      geom_bar(aes(y=v2), stat="identity", color="grey40", position = position_stack(), width=0.45)+
      #scale_fill_manual(values=c("#2E8B57","#3CB371","#DB2929","#8B0000"))+
      scale_y_continuous(name="", breaks=c(0,20,40,60,80,100), labels=c("0%", "20%", "40%", "60%", "80%", "100%"))+
      xlab("")+
      geom_text(aes(label = v2, y = pos), size = 4.5, color="White")+
      coord_flip()+
      theme.plot.bar.flip.3()
    
    r <- ggplot(df2, aes(x=v3, fill=v1, y=v2))+
      geom_bar(aes(y=v2), stat="identity", color="grey40", position = position_stack(), width=0.45)+
      #scale_fill_manual(values=c("#2E8B57","#3CB371","#DB2929","#8B0000"))+
      scale_y_continuous(name="", breaks=c(0,20,40,60,80,100), labels=c("0%", "20%", "40%", "60%", "80%", "100%"))+
      xlab("")+
      geom_text(aes(label = v2, y = pos), size = 4.5, color="White")+
      theme.plot.bar.flip.3()
    
    out <- if (flip == TRUE) (q) else (r)
    
  }
  
  out
  
}

freq <- function(x, flip=T, sort=T,lim=100,titel="titel",
                 red=144, blue=26, green=30){
  
  df   <- x
  s    <- titel
  b       <- c(0,10,20,30,40,50,60,70,80,90,100)
  l       <- c("0%","", "20%","", "40%","", "60%","", "80%","", "100%")
  
  df[,3]            <- round(df[,2] / sum(df[,2]) * 100, 1)
  
  if (flip == TRUE){
    
    levels(df$v1)  <- gsub('(.{20})\\s+', '\\1\n\\2',levels(df$v1))
    
    if (sort == TRUE){
      nif               <- order(df$V3)
      df$v1             <- factor(df$v1,levels(df$v1)[c(nif)])}
    
    if (sort == FALSE){
      nif            <- length(levels(df$v1))
      df$v1          <- factor(df$v1,levels(df$v1)[c(nif:1)])
    }
    
    out <- ggplot(df, aes(x=v1, y=V3))+
      geom_bar(stat="identity", fill=rgb(red,blue,green, maxColorValue=255), width=0.4)+
      scale_y_continuous(name="", limits=c(0,lim), breaks=b, labels=l)+
      xlab("")+
      geom_text(aes(label = V3),hjust=-0.3, size=4, color="black")+
      coord_flip()+
      ggtitle(s)+
      theme.plot.bar.flip()
  }
  
  if (flip == FALSE){
    
    levels(df$v1)  <- gsub('(.{10})\\s+', '\\1\n\\2',levels(df$v1))
    
    if (sort == TRUE){
      nif            <- order(-df$V3)
      df$v1         <- factor(df$v1,levels(df$v1)[c(nif)])
    }
    
    out <- ggplot(df, aes(x=v1, y=V3))+
      geom_bar(stat="identity", fill=rgb(red,green,blue, maxColorValue=255), width=0.4)+
      scale_y_continuous(name="", limits=c(0,lim), breaks=b, labels=l)+
      xlab("")+
      geom_text(aes(label = V3),vjust=-0.3, size=4, color="black")+
      ggtitle(s)+
      theme.plot.bar()
  }
  out  
}

