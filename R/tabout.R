#' tabout
#' 
#' takes a list of dataframes and write them to a sheet
#' @param x a list of dataframes or a single data.frame
#' @param sheet.name name your sheet
#' @param overwrite 
#' @param file filename
#' @param row.break.length number of empty rows between dataframes in sheet
#' @param headers takes the name of x unless something else is specificed
#' @param header.pos which column to paste headers too. 1 or 2
#' @param write if false returns workbook
#' @param style chose how to style your frames (currently you can only  style=1)
#' @export tabout


tabout       <- function (
          x, 
          sheet.name = NULL,
          overwrite = TRUE,
          file = "My_tables.xlsx", 
          row.break.length = 2, 
          write = TRUE, 
          style = 1,
          dropif = 40,
          headers = names(x),
          header.pos = 1) 
{
          
          # load or write my_workbook
          if (overwrite)
                    my_workbook    <- createWorkbook()
          else {
                    if ("try-error" %in% class(try(loadWorkbook(file), silent = T))) 
                              my_workbook    <- createWorkbook()
                    else {
                              my_workbook   <- loadWorkbook(file)
                              my_sheets     <- getSheets(my_workbook)
                              
                              if (sheet.name %in% names(my_sheets))
                                        removeSheet(my_workbook, sheet.name)
                    }
          }

          new.sheet <- createSheet(my_workbook, sheetName = sheet.name)
          
          ##
          
          if (is.data.frame(x)) 
                    x <- list(x)          
          
          
          # identificer hvilke rækker tabeller og headers skal smides ud i
          n.rows       <- sapply(x,nrow)
          n.cols       <- sapply(x,ncol)
          
          n.cols       <- n.cols[which(n.rows < dropif)]
          x            <- x[which(n.rows < dropif)]
          my.headers   <- headers[which(n.rows < dropif)]
          n.rows       <- n.rows[which(n.rows < dropif)]
          
          
          row.breaks   <- cumsum(rep(row.break.length, length(x))) - row.break.length
          start.rows   <- cumsum(n.rows) + 1
          start.rows   <- start.rows - n.rows
          
          plus         <- cumsum(rep(2, length(x))) - 1          
          start.rows   <- start.rows + plus + row.breaks
          end.rows     <- start.rows + n.rows
          
          footnote.rows <- end.rows + 1 
          header.rows   <- start.rows - 1  
          
          ###
          cs           <- style(my_workbook, style = style)  
          
          cb.rows      <- createRow(new.sheet, 1:tail(footnote.rows,1))
          cb.cells     <- createCell(cb.rows, 1:(max(n.cols)+1))
          
          
          l_ply(seq_along(x), function(i){
                    
                    df.col.index          <- rep(list(cs$col.style), dim(x[[i]])[2])
                    names(df.col.index)   <- seq(1, dim(x[[i]])[2], by = 1)
                    
                    #addMergedRegion(new.sheet, header.rows[i], header.rows[i], 1, n.cols[i] + 1)
                    
                    addDataFrame(x[[i]], new.sheet, 
                                 startRow = start.rows[i],
                                 colnamesStyle = cs$col.name.style, 
                                 rownamesStyle = cs$row.name.style, 
                                 colStyle = df.col.index)
                    
          })
          
          row.headers          <- getRows(new.sheet, header.rows)
          row.sub.headers      <- getRows(new.sheet, start.rows)
          row.footnote         <- getRows(new.sheet, footnote.rows )
          cell.footnote        <- getCells(row.footnote, colIndex = 1)
          cell.headers         <- getCells(row.headers, colIndex = header.pos)
          cell.sub.headers     <- getCells(row.sub.headers, colIndex = 1)
          
          lapply(cell.sub.headers, setCellStyle, cs$sub.header.style)
          lapply(cell.headers, setCellStyle, cs$header.style)
          #lapply(cell.sub.headers, setCellValue, attributes(x)$CST)
          
          l_ply(seq_along(x), function(i){
                    
                    if (!is.null(attributes(x[[i]])$sub.header))
                              setCellValue(cell.sub.headers[[i]], attributes(x[[i]])$sub.header)
                    if (!is.null(attributes(x[[i]])$footnote))
                              setCellValue(cell.footnote[[i]], attributes(x[[i]])$footnote)
          })
          
          if (!is.null(headers)){
                    
                    l_ply(seq_along(x), function(i){
                              setCellValue(cell.headers[[i]], my.headers[i])
                              })
          }
          
          #setColumnWidth(new.sheet, colIndex = 1, colWidth = 15)
          all.rows  <- getRows(new.sheet)
          setRowHeight(all.rows, 15)
          

          ###
          if (write) 
                    saveWorkbook(my_workbook, file)
          else {
                    invisible(my_workbook)
          }
}


# tabout.lime <- function(x, sheet.name=NULL, file="My_tables.xlsx", row.break.length=2, write=TRUE, style=1){
#           
#           try(library(xlsx), silent=TRUE)   
#           
#           if ("try-error" %in% class(try(loadWorkbook(file), silent=T)))
#                     my_workbook <- createWorkbook()
#           else {
#                     my_workbook <- loadWorkbook(file)
#                     my_sheets <- getSheets(my_workbook)
#                     if (!is.null(sheet.name) && sheet.name %in% names(my_sheets))
#                               removeSheet(my_workbook, sheet.name)    
#           }          
#           new.sheet <- createSheet(my_workbook, sheetName = sheet.name)
# 
#           # data
#           if (is.data.frame(x)) 
#                     x <- list(x)
#    
#           # attributes
#           has.header <- has.sub.header <- headers <- sub.headers <- vector(length=length(x))
#           
#           for (i in 1:length(x)){
#                     has.header[i] <- !is.null(attributes(x[[i]])$header)
#                     has.sub.header[i] <- !is.null(attributes(x[[i]])$sub.header)
#                     
#                     if(has.header[i]){headers[i] <- unlist(attributes(x[[i]])$header)}
#                     else {headers[i] <- paste(attributes(x)$names[i],attributes(x)$variable.labels[i])}
#                     
#                     if (has.sub.header[i]){sub.headers[i] <- unlist(attributes(x[[i]])$sub.header)}
#           }
#           
#           sub.headers <- sub.headers[!is.element(sub.headers, "FALSE")]
#          
#           # row indexes, row.break, col.length
#           row.breaks <- cumsum(rep(row.break.length, length(x)))-row.break.length
#           
#           n.rows <- n.cols <- vector(length=length(x))
#           for (i in 1:length(x)){
#                     n.rows[i] <- unlist(nrow(x[[i]]))
#                     n.cols[i] <- unlist(ncol(x[[i]]))
#           }
#           
#           start.rows <- cumsum(n.rows)+1
#           for (i in 1:length(x))
#                     start.rows[i] <-start.rows[i]-n.rows[i]
#                     
#           plus.h <- cumsum(rep(2, length(x)))-1 
#           start.rows <-start.rows+plus.h+row.breaks
#           end.rows <-start.rows+n.rows
#           
#           row.h <- start.rows-1
#                   
#           # CellStyles (turn into function)
#           cs <- style(my_workbook, style=style)
# 
#           ### add data and cellStyles
# 
#           if (end.rows[length(end.rows)] < 50)
#                     no.rows <- 75
#           else (no.rows <- end.rows[length(end.rows)]+25)
#           
#           cb.rows <- createRow(new.sheet, 1:no.rows)
#           cb.cells <- createCell(cb.rows, 1:50)
#           
# #           for (i in 1:length(cb.cells))
# #                     setCellStyle(cb.cells[[i]], cs$bg.style)
# 
#           for (i in 1:length(x)){
#                     df.col.index <- rep(list(cs$col.style), dim(x[[i]])[2]) 
#                     names(df.col.index) <- seq(1, dim(x[[i]])[2], by = 1)  #seq(start.cols[1],start.cols[1]+dim(x[[1]])[2], by = 1)
#                     
#                     addMergedRegion(new.sheet,  row.h[i], row.h[i], 1, n.cols[i]+1)
#                     
#                     addDataFrame(x[[i]], new.sheet, startRow=start.rows[i], 
#                                  colnamesStyle= cs$col.name.style, rownamesStyle=cs$row.name.style,colStyle=df.col.index)         
#           }
#           
#           row.sh <- getRows(new.sheet, start.rows)
#           cell.sh <- getCells(row.sh, colIndex=1)
#           
#           for (i in 1:length(row.sh)){
#                     setCellStyle(cell.sh[[i]], cs$sub.header.style)
#           }
# 
#           if (!length(sub.headers)==FALSE){
#                     cell.sh <- cell.sh[has.sub.header]
#                     for (i in 1:length(cell.sh))
#                               setCellValue(cell.sh[[i]], sub.headers[i])
#           }
#           
#           r.h <- getRows(new.sheet, row.h)
#           cell.h <- vector("list", length=length(r.h))
#           for (i in 1:length(row.h)){
#                     cell.h[[i]] <- getCells(r.h[i], 1:(n.cols[i]+1))          
#           }          
#           c.h <- lapply(cell.h, `[[`, 1)
#           
#           for (i in 1:length(c.h)){
#                     setCellValue(c.h[[i]], headers[i])
#           }
#           
#           cell.h <- unlist(cell.h)
#           for (i in 1:length(cell.h)){
#                     setCellStyle(cell.h[[i]], cs$header.style)   
#           }
#           
#           setColumnWidth(new.sheet,colIndex=1,colWidth=15)
#           setRowHeight(row.sh, 15)
# 
#           
#           # return
#           if (write)
#                     saveWorkbook(my_workbook, file)
#           else{
#                     invisible(my_workbook)
#           }
# }


#' style
#'
#' for tabout
#'

style <- function(wb = my_workbook, style=1){
  r.name.style <- c.name.style <- c.style <- h.style <- sh.style <- CellStyle(wb) 
  
  #1 style
  if (style == 1){
    font1      <- Font(wb,heightInPoints=9, name="Helvetica")
    font2      <- Font(wb,heightInPoints=10, name="Helvetica", isBold=T)
    l.align    <- Alignment(h="ALIGN_LEFT", wrapText=T)
    l.align2   <- Alignment(h="ALIGN_LEFT", wrapText=F)
    c.align    <- Alignment(h="ALIGN_CENTER", wrapText=T)
    r.align    <- Alignment(h="ALIGN_RIGHT", wrapText=T)
    fill1      <- Fill(backgroundColor="#AAAAAA",foregroundColor="#AAAAAA")
    border1    <- Border(position=c("TOP", "BOTTOM", "LEFT", "RIGHT"))
    
    row.name.style   <- r.name.style + font1 + l.align + border1
    col.name.style   <- c.name.style + font1 + r.align + border1
    col.style        <- c.style + font1 + r.align + border1
    header.style     <- h.style + font2 + l.align2
    sub.header.style <- sh.style + font2 + l.align  + border1
    
  }
  
  if (style == 2){
    font1      <- Font(wb,heightInPoints=9, name="Helvetica")
    font2      <- Font(wb,heightInPoints=10, name="Helvetica", isBold=T)
    l.align    <- Alignment(h="ALIGN_LEFT", wrapText=F)
    l.align2   <- Alignment(h="ALIGN_LEFT", wrapText=F)
    c.align    <- Alignment(h="ALIGN_CENTER", wrapText=F)
    r.align    <- Alignment(h="ALIGN_RIGHT", wrapText=F)
    fill1      <- Fill(backgroundColor="#AAAAAA",foregroundColor="#AAAAAA")
    border1    <- Border(position=c("TOP", "BOTTOM", "LEFT", "RIGHT"))
    
    row.name.style   <- r.name.style + font1 + l.align + border1
    col.name.style   <- c.name.style + font1 + r.align + border1 + fill1
    col.style        <- c.style + font1 + r.align + border1
    header.style     <- h.style + font2 + l.align2
    sub.header.style <- sh.style + font1 + l.align  + border1 + fill1
  }
  
  styles <- list(row.name.style, col.name.style,col.style,header.style, sub.header.style)
  names(styles) <- c("row.name.style", "col.name.style","col.style","header.style", "sub.header.style")
  styles
}


