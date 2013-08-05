#### Krydstabel med GridExtra-pakken ###

tab.cross <-  function(x, y, prop.margin=1, digits=2, margin=1:2, stat=T,
                       red=144, green=26, blue= 30, titel=""){
  
  require("vcd")
  
  freq  <- table(x,y)
  prop  <- round(prop.table(freq, prop.margin), digits)
  
  ## statistical testing ##
  chi   <- chisq.test(x,y)
  assoc <- assocstats(freq)
  
  cramer  <- round(assoc$cramer,5)
  phi     <- round(assoc$phi,5)
  chi     <- round(chi$p.value,5)
  
  stat.test <- paste("Pearsons p-værdi =",chi,",", "phi =",phi, ",", "cramer's V =",cramer, sep=" ")
  
# Standard width
  
  want.width  <- 120 
  nc          <- ncol(out.tab) + 1
  each        <- want.width / nc
  
  colnames(out.tab) <- format(colnames(out.tab), width=each, justify="centre")
  out.tab[]         <- format(out.tab, width=unit(each, "mm"), justify="centre")

  farve <- rgb(red,green,blue, maxColorValue =255)
  
  tg <-   tableGrob(out.tab,
                    rows=rownames(out.tab),
                    gpar.coretext = gpar(fontsize = 12),            
                    gpar.coltext  = gpar(fontsize = 14,col="black", fontface=2),            
                    gpar.rowtext  = gpar(fontsize = 14, col="black", fontface=4),            
                    gpar.corefill = gpar(fill = rgb(255,255,255, maxColorValue =255), alpha = 1, col = "white"),
                    gpar.rowfill  = gpar(fill = rgb(255,255,255, maxColorValue =255), alpha = 1, col = "white"),           
                    gpar.colfill  = gpar(fill = NA, alpha = 1, col= NA),                       
                    equal.width   = FALSE,            
                    show.rownames = TRUE,            
                    show.rsep     = TRUE, 
                    show.hlines   = TRUE,                               
                    show.csep     = FALSE, 
                    show.vlines   = FALSE,
                    show.box      = FALSE,
                    padding.h     = unit(8, "mm"),            
                    padding.v     = unit(8, "mm"),
                    core.just     = "center", 
                    row.just      = "left",
                    separator     = "grey90")
  
  
  hh <- grobHeight(tg)
  ww <- grobWidth(tg)
  
  
  border <- rectGrob(x =unit(0.5,"npc"), y = unit(0.5,"npc") + 0.5*hh - unit(6, "mm"),
                     width =ww, height = unit(8, "mm"),
                     just = "centre", hjust = NULL, vjust = NULL, 
                     default.units = "npc", name = NULL,
                     gp=gpar(fill=farve, col="white"), vp = NULL)
  
  xx        <- unit(0.5,"npc") - 0.5*ww
  xy        <- unit(0.5,"npc") + 0.5*ww
  yy.top    <- unit(0.5,"npc") + 0.5*hh-unit(12, "mm")
  yy.bot    <- unit(0.5,"npc") - 0.52*hh
  yy.head   <- unit(0.5,"npc") + 0.5*hh
  
  
  border.head <- linesGrob(x = unit.c(xx, xy),
                           y = unit.c(yy.head, yy.head),
                           default.units = "npc",
                           arrow = NULL, name = NULL,
                           gp=gpar(col=farve, lwd=2), vp = NULL)
  
  
  border.top <- linesGrob(x = unit.c(xx, xy),
                          y = unit.c(yy.top, yy.top),
                          default.units = "npc",
                          arrow = NULL, name = NULL,
                          gp=gpar(col=farve, lwd=1), vp = NULL)
  
  
  border.bot <- linesGrob(x = unit.c(xx, xy),
                          y = unit.c(yy.bot,yy.bot),
                          default.units = "npc",
                          arrow = NULL, name = NULL,
                          gp=gpar(col=farve, lwd=2), vp = NULL)
  
  
  title <- textGrob(titel,
                    x=unit(0.5,"npc") -0.5*ww + unit(5, "mm"), 
                    y=unit(0.5,"npc") +0.5*hh + unit(8, "mm"), 
                    vjust=1, hjust=0, just="center", gp=gpar(fontsize=13, fontface="bold"))
  
  
  test  <- textGrob("", 
                    x=unit(0.5,"npc") - 0.5*ww,
                    y=unit(0.5,"npc") - 0.5*hh, 
                    vjust=1, hjust=0,gp=gpar(fontsize=11))
  
  if (stat == TRUE){
    
    test  <- textGrob(stat.test, 
                      x=unit(0.5,"npc") - 0.5*ww,
                      y=unit(0.5,"npc") - 0.5*hh - unit(4, "mm"), 
                      vjust=1, hjust=0,gp=gpar(fontsize=11))
  }
  
  grid.newpage()
  gt <- gTree(children=gList(tg,border.head, border.top,border.bot, title, test))
  grid.draw(gt)
}

tab.freq <- function(x, digits=2,
                     red=144, green=26, blue= 30, titel=""){
  
  
  freq <- table(x)  
  prop <- round(prop.table(freq), 2)
  
  freq  <- addmargins(freq, 1)
  prop  <- addmargins(prop, 1)
  
  prop.p <- format(paste(prop*100, "%", sep=" "), justify="right")
  freq.f <- format(freq, justify="right")
  
  out.tab <- cbind(freq.f, prop.p)
  
  rownames(out.tab)[nrow(out.tab)] <- "Total"
  colnames(out.tab) <- c("Antal", "Procent")
  
  want.width  <- 120 
  nc          <- ncol(out.tab) + 1
  each        <- want.width / nc
  
  colnames(out.tab) <- format(colnames(out.tab), width=each, justify="centre")
  out.tab[]         <- format(out.tab, width=unit(each, "mm"), justify="centre")
  
  farve <- rgb(red,green,blue, maxColorValue =255)
  
  tg <-   tableGrob(out.tab,
                    rows=rownames(out.tab),
                    gpar.coretext = gpar(fontsize = 12),            
                    gpar.coltext  = gpar(fontsize = 14,col="black", fontface=2),            
                    gpar.rowtext  = gpar(fontsize = 14, col="black", fontface=4),            
                    gpar.corefill = gpar(fill = rgb(255,255,255, maxColorValue =255), alpha = 1, col = NA),
                    gpar.rowfill  = gpar(fill = rgb(255,255,255, maxColorValue =255), alpha = 1, col = "white"),           
                    gpar.colfill  = gpar(fill = NA, alpha = 1, col= NA),                       
                    equal.width   = FALSE,            
                    show.rownames = TRUE,            
                    show.rsep     = FALSE, 
                    show.hlines   = FALSE,                               
                    show.csep     = FALSE, 
                    show.vlines   = FALSE,
                    show.box      = FALSE,
                    #padding.h     = unit(8, "mm"),            
                    padding.v     = unit(8, "mm"),
                    core.just     = "center", 
                    row.just      = "left",
                    separator     = "grey80")
  
  
  hh <- grobHeight(tg)
  ww <- grobWidth(tg)
  
  
  border <- rectGrob(x =unit(0.5,"npc"), y = unit(0.5,"npc") + 0.5*hh - unit(6, "mm"),
                     width =ww, height = unit(8, "mm"),
                     just = "centre", hjust = NULL, vjust = NULL, 
                     default.units = "npc", name = NULL,
                     gp=gpar(fill=farve, col="white"), vp = NULL)
  
  xx        <- unit(0.5,"npc") - 0.5*ww
  xy        <- unit(0.5,"npc") + 0.5*ww
  yy.top    <- unit(0.5,"npc") + 0.5*hh-unit(12, "mm")
  yy.bot    <- unit(0.5,"npc") - 0.52*hh
  yy.head   <- unit(0.5,"npc") + 0.5*hh
  
  
  border.head <- linesGrob(x = unit.c(xx, xy),
                           y = unit.c(yy.head, yy.head),
                           default.units = "npc",
                           arrow = NULL, name = NULL,
                           gp=gpar(col=farve, lwd=2), vp = NULL)
  
  
  border.top <- linesGrob(x = unit.c(xx, xy),
                          y = unit.c(yy.top, yy.top),
                          default.units = "npc",
                          arrow = NULL, name = NULL,
                          gp=gpar(col=farve, lwd=1), vp = NULL)
  
  
  border.bot <- linesGrob(x = unit.c(xx, xy),
                          y = unit.c(yy.bot,yy.bot),
                          default.units = "npc",
                          arrow = NULL, name = NULL,
                          gp=gpar(col=farve, lwd=2), vp = NULL)
  
  
  title <- textGrob(titel,
                    x=unit(0.5,"npc") -0.5*ww + unit(5, "mm"), 
                    y=unit(0.5,"npc") +0.5*hh + unit(8, "mm"), 
                    vjust=1, hjust=0, just="center", gp=gpar(fontsize=13, fontface="bold"))
  
  grid.newpage()
  gt <- gTree(children=gList(tg,border.head, border.top,border.bot, title))
  grid.draw(gt)
}

levels(x)  <- gsub('(.{35})\\s+', '\\1\n\\2',levels(x))

tab.freq(pol$nujob_sektor_v1)
levels(pol$job_sektor_v1)[8] <- "International organisation eller institution"
levels(pol$job_sektor_v1)[4] <- "Centraladministrationen"


x <- pol$job_sektor_v1
levels(x)  <- gsub('(.{35})\\s+', '\\1\n\\2',levels(x)) # Virker ikke
levels(x)  <- gsub('(.{20})\\s+', '\\1\n\\2',levels(x)) # Virker 

tab.cross(x, pol$agegr)
tab.freq(x)

