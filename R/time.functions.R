Hour   <- function(x,tz="BST") as.numeric(format(as.POSIXct(x, tz=tz), format="%H"))

Month  <- function(x,tz="BST") as.numeric(format(as.POSIXct(x, tz=tz), format="%m"))

Year   <- function(x,tz="BST") as.numeric(format(as.POSIXct(x, tz=tz), format="%Y"))

Ymd    <- function(x,tz="BST") as.numeric(format(as.POSIXct(x, tz=tz), format="%Y%m%d"))

Ym     <- function(x,tz="BST") as.numeric(format(as.POSIXct(x, tz=tz), format="%Y%m"))

YQ     <- function(x,tz="BST") paste(Year(x,tz=tz),quarters(x),sep="")

Ndays  <- function(x,tz="BST") length(unique(Ymd(x,tz=tz)))

Nmonths<- function(x,tz="BST") length(unique(Ym(x,tz=tz)))

squeeze<- function(x) {
  if(length(x)==0) {
    out <- "-"
  } else {
    x <- sort(x,decreasing=F)
    xb <- c(NA,x)[1:length(x)]
    xa <- c(x,NA)[2:(1+length(x))]
    ext <- (xa-x)>1 | (x-xb)>1
    sel <- which(is.na(ext)|ext)
    x.sel <- x[sel]
    sela <- c(sel,NA)[2:(1+length(sel))]
    sym <- c("-",",")[as.numeric(sela-sel==1)+1]
    sym[is.na(sym)] <- ""    
    out <- paste(rbind(x.sel,sym),collapse="")
  }
  return(out)
}
