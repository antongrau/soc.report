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
    text                   = element_text(face = "plain", colour = "black", size = 12, hjust=0, vjust = 0.5, angle = 0, lineheight = 0.9),
    axis.text.x            = element_text(size = 11, colour = "grey50"),
    axis.text.y            = element_text(size = 11, colour = "grey50", hjust = 1),
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
    axis.text.y            = element_text(size = 11, colour = "grey50", vjust=0.5, hjust=0),
    axis.text.x            = element_text(size = 11, colour = "grey50", hjust = 0.5),
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
    axis.line = element_line(colour = "grey70", size = 0.2, linetype = 1, 
                             lineend = "butt"),
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


theme.plot.bar.likert <- function(){
  
  theme(
    text                   = element_text(face = "plain", colour = "black", size = 12, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = 0.9),
    axis.text.y            = element_text(size = 11, colour = "grey50", vjust=0.5, hjust=0),
    axis.text.x            = element_text(size = 11, colour = "grey50", hjust = 0.5),
    strip.text             = element_text(size = rel(0.8)),
    axis.ticks             = element_blank(),
    axis.title.x           = element_text(), 
    axis.title.y           = element_text(angle = 90),
    panel.background       = element_rect(fill = NA ,colour = NA),
    panel.border           = element_blank(),
    panel.grid.major.y     = element_blank(),
    panel.grid.minor       = element_blank(),
    panel.grid.major.x     = element_line(colour = "grey70", linetype= "dotted", size =0.6),
    strip.text.x           = element_text(),
    strip.text.y           = element_text(angle = -90),
    #plot.background        = element_rect(colour = "grey40", size = 0.8, linetype= 1, fill="white"), 
    plot.margin            = unit(c(1, 1, 1, 1), "mm"),
    plot.title             = element_text(size = 10, hjust=1),
    legend.title           = element_blank(),
    legend.key.size        = unit(1, "lines"),
    legend.position        = "bottom",
    legend.direction       = "horizontal",
    axis.line = element_line(colour = "grey70", size = 0.2, linetype = 1, 
                        lineend = "butt"),
    complete               = FALSE)
  
}



#################
#  FUNKTIONER   #
#################

plot.bar <- function(var, var.lab=FALSE, flip=TRUE, sort=TRUE, lim=100, 
                      farve = '#5AB4AC', set.na=FALSE, na="Nej", andet=FALSE){
  
  b       <- c(0,25,50,75,100)
  l       <- c("0%","25%","50%","75%", "100%")
  
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
    
    df2[,2]       <- round(df2[,2] / sum(df2[,2]) * 100)
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
        geom_bar(stat="identity", fill=farve, width=0.4)+
        scale_y_continuous(name="", limits=c(-15,lim), breaks=b, labels=l)+
        xlab("")+
        geom_text(aes(label = paste(v2, "%", sep="")),y=-14, size=4, color="black", just="left")+
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
        geom_bar(stat="identity", fill=farve, width=0.4)+
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
      
      df2[,2]       <- round(df2[,2] / sum(df2[,2]) * 100)
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
          geom_bar(stat="identity", fill=farve, width=0.4)+
          scale_y_continuous(name="", limits=c(-15,lim), breaks=b, labels=l)+
          xlab("")+
          geom_text(aes(label = paste(v2, "%", sep="")),y=-14, size=4, color="black")+
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
          geom_bar(stat="identity", fill=farve, width=0.4)+
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
        df[,ncol(df)]  <- ifelse(df[,ncol(df)] == TRUE, na, "Ja")
      }
      
      n.mat     <- df == na
      df        <- df[rowSums(n.mat) != ncol(df),]  # fjern dem som har svaret nej til alle spm(de har formodenlig ikke svaret)
      
      df$id     <- c(1:as.numeric(length(df[,1])))
      
      s         <- as.character(length(df[,1]))
      s         <- paste("Antal besvarelser:", s, sep=" ")
      
      dfm       <- melt(df, id="id")
      df2       <- cast(dfm, value~variable, fun = length)
      
      df2[, 2:ncol(df2)]      <- round(df2[, 2:ncol(df2)] / colSums(df2[, 2:ncol(df2)]) * 100)
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
          geom_bar(stat="identity", fill=farve, width=0.4)+
          scale_y_continuous(name="", limits=c(-15,lim), breaks=b, labels=l)+
          xlab("")+
          geom_text(aes(label = paste(v2, "%", sep="")),y=-14, size=4, color="black")+
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
          geom_bar(stat="identity", fill=farve, width=0.4)+
          scale_y_continuous(name="", limits=c(0,lim), breaks=b, labels=l)+
          xlab("")+
          geom_text(aes(label = v2),vjust=-0.3, size=4, color="black")+
          ggtitle(s)+
          theme.plot.bar()
      }}}
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


#### Krydstabel med GridExtra-pakken ###
relevel <- function(items, mylevels){
  
  if (is.data.frame(items)){
    for(i in seq_along(items)) {
      items[,i] <- factor(items[,i], levels=mylevels)
    }}
  
  if (!is.data.frame(items)){
      items <- factor(items, levels=mylevels)   
  }
  items
}

temp.plot <- function(item){
  
  require(reshape2)
  
  if (!is.data.frame(item)){
    stop("Det er ikke en data.frame")
  }
  
  item$id     <- c(1:as.numeric(length(item[,1])))
  dfm       <- melt(item, id="id")
  df2       <- dcast(dfm, value~variable, fun = length)
  
  df2[, 2:ncol(df2)]      <- round(df2[, 2:ncol(df2)] / colSums(df2[, 2:ncol(df2)]) * 100, 1)
  df2                     <- melt(df2)
  
  names(df2)              <- c("v1","v2","v3")
  
  df2  <- na.omit(df2)
  
  breaks                   <- c(0,25,50,75,100)
  axis.labels              <- c("0%","25%","50%","75%", "100%")
  
  out <- ggplot(df2, aes(x = v1, y = v3, fill = v2)) + 
    geom_bar(stat = "identity", position = "dodge") + 
    coord_flip()+
    scale_y_continuous(name="", limits=c(-15,100), breaks=breaks, labels=axis.labels)+
    geom_text(aes(label = v3, ymax=100), y=-14, 
              size = 4, color="black", position = position_dodge(width=0.7))+
    theme.plot.bar.likert()
    
  
  out
}


bar.cross <- function(item, 
                      color = '#5AB4AC', 
                      legend.row = 1, 
                      lim=100, 
                      relevel.fill = NULL,
                      relevel.x = NULL){
  
  breaks                   <- c(0,25,50,75,100)
  axis.labels              <- c("0%","25%","50%","75%", "100%")
  
  freq       <- table(item)
  prop       <- round(prop.table(freq,1),2)*100
  df         <- melt(prop)
  names(df)  <- c("x","fill","value")
  df$label   <- paste(df$value, "%", sep='')
  
  s             <- as.character(sum(freq))
  s             <- paste("Antal besvarelser:", s, sep=" ")
  
  bamp                     <- colorRamp(c('grey90', color))
  bamp                     <- rgb(bamp(seq(0, 1, length=nlevels(df$fill))), maxColorValue=255)
  
  if (!is.null(relevel.fill)){
    df$fill  <- relevel(df$fill, relevel.fill)
  }
  
  if (!is.null(relevel.x)){
    df$x  <- relevel(df$x, relevel.x)
  }
  
  levels(df$x)  <- gsub('(.{18})\\s+', '\\1\n\\2',levels(df$x))
  
  out <- ggplot(df, aes(x=x, fill=fill, y=value))+
    geom_bar(stat="identity", position = position_dodge(),width=0.7)+
    scale_y_continuous(name="", limits=c(-15,lim), breaks=breaks, labels=axis.labels)+
    geom_text(aes(label = label, ymax=100), y=-14, 
              size = 4, color="black", position = position_dodge(width=0.7))+
    scale_fill_manual(values=bamp, guide = guide_legend(nrow=legend.row, reverse = T))+
    coord_flip()+
    xlab("")+
    ggtitle(s)+
    theme.plot.bar.likert()
  
  out
  
}

multiple.freq   <- function(item, 
                            set.as.na="Nej",
                            relevel.x = NULL,
                            andet= FALSE, 
                            sort = TRUE){
  
  
  if (!is.data.frame(item)){
    stop("Data must be in a data.frame format")
  }
  
  if (andet == TRUE){        
    item[,ncol(item)]  <- is.na(item[,ncol(item)])
    item[,ncol(item)]  <- ifelse(item[,ncol(item)] == TRUE, set.as.na, "Ja")
  }
  
  n.mat       <- item == set.as.na
  item        <- item[rowSums(n.mat) != ncol(item),]  
  item$id     <- c(1:as.numeric(length(item[,1])))
  item        <- melt(item, id="id")
  item        <- cast(item, value~variable, fun = length)
  
  item[, 2:ncol(item)]       <- round(item[, 2:ncol(item)] / colSums(item[, 2:ncol(item)]) * 100)
  item                       <- melt(item)
  item                       <- item[item$value != set.as.na,]
  item                       <- as.data.frame(item[,2:ncol(item)])
  names(item)                <- c("value","x")
  item$label                 <- paste(item$value, "%", sep='')
  item$fill                  <- vector(length=length(item$label))
  item                       <- na.omit(item)
  
  if (!is.null(relevel.x)){
    levels(item$x)             <- relevel.x 
  }
  
  if (sort == TRUE){
    sort.x                <- order(item$value)
    item$x                <- factor(item$x,levels(item$x)[c(sort.x)])
  }
  
  levels(item$x)  <- gsub('(.{18})\\s+', '\\1\n\\2',levels(item$x))
  item
}


fill  <- function(items, 
                  names,
                  reverse = FALSE){
  for (i in seq_along(items)){
    items[[i]]$fill <- as.factor(names)[i]
  }
  items  <- ldply(items)
  
  if (reverse == TRUE){
    items$fill  <- factor(items$fill, levels =rev(levels(items$fill)))  
  }
  items
}


plot.freq2  <- function(items,
                        color = '#5AB4AC', 
                        legend.row = 1, 
                        lim=100){
  breaks                   <- c(0,25,50,75,100)
  axis.labels              <- c("0%","25%","50%","75%", "100%")
  bamp                     <- colorRamp(c('grey80', color))
  bamp                    <- rgb(bamp(seq(0, 1, length=nlevels(items$fill))), maxColorValue=255)

  
  out <- ggplot(items, aes(x=x, fill=factor(fill), y=value))+
    geom_bar(stat="identity", position = position_dodge(),width=0.7)+
    scale_y_continuous(name="", limits=c(-15,lim), breaks=breaks, labels=axis.labels)+
    geom_text(aes(label = label, ymax=100), y=-14, 
              size = 4, color="black", position = position_dodge(width=0.7))+
    scale_fill_manual(values=bamp, guide = guide_legend(nrow=legend.row, reverse=T))+
    coord_flip()+
    xlab("")+
    theme.plot.bar.likert()
  
  out
  
}