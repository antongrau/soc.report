
#' Create style and plot a tableGrob object on the basis of any table.
#' 
#' grob.tab is mainly used for plotting and styling. Styling includes colors and seperator lines but not width and alignment.
#' Alignment, width etc. should be handled by \link{paste} and \link{format}.
#' @param tab is a table
#' @param style is one of the predefined styles : "academic"
#' @param footnote is the text displayed below the table
#' @param title is the text displayed above the table
#' @return a plot version of the table
#' @export grob.tab

grob.tab <- function(tab, style="academic",  footnote="", title="", main.line.color="black"){

#####################################
# Style academic
if(identical(style, "academic")){
tg <-   tableGrob(tab,
                    rows=rownames(tab),
                    gpar.coretext = gpar(fontsize = 12),            
                    gpar.coltext  = gpar(fontsize = 12,col="black", fontface="italic"),            
                    gpar.rowtext  = gpar(fontsize = 12, fontface="italic"),            
                    gpar.corefill = gpar(fill = rgb(255,255,255, maxColorValue =255), alpha = 1, col = NA),
                    gpar.rowfill  = gpar(fill = rgb(255,255,255, maxColorValue =255), alpha = 1, col = NA),           
                    gpar.colfill  = gpar(fill = rgb(255,255,255, maxColorValue =255), alpha = 1, col= "white"),                       
                    equal.width   = FALSE,            
                    show.rownames = TRUE,            
                    show.rsep     = TRUE, 
                    show.hlines   = TRUE,                               
                    show.csep     = FALSE, 
                    show.vlines   = FALSE,
                    show.box      = FALSE,
                    padding.h     = unit(20, "mm"),            
                    padding.v     = unit(8, "mm"),
                    core.just     = "center", 
                    row.just      = "left",
                    separator     = "grey85")
  
  
  hh <- grobHeight(tg)
  ww <- grobWidth(tg)
  
  
  # The empty cell between row and col labels
  border <- rectGrob(x =unit(0.5,"npc"), y = unit(0.5,"npc"),
                     width =ww, height = hh,
                     just = "centre", hjust = NULL, vjust = NULL, 
                     default.units = "npc", name = NULL,
                     gp=gpar(fill="white", col="white"), vp = NULL)
  
  xx        <- unit(0.5,"npc") - 0.5*ww
  xy        <- unit(0.5,"npc") + 0.5*ww
  yy.top    <- unit(0.5,"npc") + 0.5*hh-unit(12, "mm")
  yy.bot    <- unit(0.5,"npc") - 0.52*hh
  yy.head   <- unit(0.5,"npc") + 0.5*hh
  
  
  border.head <- linesGrob(x = unit.c(xx, xy),
                          y = unit.c(yy.head, yy.head),
                          default.units = "npc",
                          arrow = NULL, name = NULL,
                          gp=gpar(col=main.line.color, lwd=2), vp = NULL)
  
  
  border.top <- linesGrob(x = unit.c(xx, xy),
                       y = unit.c(yy.top, yy.top),
                       default.units = "npc",
                       arrow = NULL, name = NULL,
                       gp=gpar(col=main.line.color, lwd=1), vp = NULL)
  
  
  border.bot <- linesGrob(x = unit.c(xx, xy),
                       y = unit.c(yy.bot,yy.bot),
                       default.units = "npc",
                       arrow = NULL, name = NULL,
                       gp=gpar(col=main.line.color, lwd=2), vp = NULL)
  
  
  title.g <- textGrob(title,
                    x=unit(0.5,"npc") -0.5*ww + unit(5, "mm"), 
                    y=unit(0.5,"npc") +0.5*hh + unit(8, "mm"), 
                    vjust=1, hjust=0, just="center", gp=gpar(fontsize=13, fontface="bold"))
  
  footnote.g <- textGrob(footnote, 
                       x=unit(0.5,"npc") - 0.5*ww,
                       y=unit(0.5,"npc") - 0.5*hh - unit(4, "mm"), 
                       vjust=1, hjust=0,gp=gpar(fontsize=11))
  
  out.grob.list  <- gList(border,tg, border.head, border.top, border.bot, title.g, footnote.g)

  grid.newpage()
  gt <- gTree(children=out.grob.list)
  grid.draw(gt)
}

##########################################################################
#### Insert some other style

}

##################################################################################
### Fix the width of a table:

# tab <- tab.cross(x,y)
# tab
# 
# want.width  <- 140
# 
# nc      <- ncol(tab) + 1
# each    <- want.width / nc
# 
# colnames(tab) <- format(colnames(tab), width=each, justify="centre")
# tab[]         <- format(tab, width=each, justify="centre")
# tab
# 
# grob.tab(tab)


