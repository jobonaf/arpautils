# contiene funzioni utili per statistiche di dati QA
# con valenza legale o meno

stat.period <- function(x,period,necess,FUN=mean) {
  xmean  <- tapply(X=x, INDEX=period, FUN=FUN, na.rm=T)
  if(necess<1) {
    Fcheck <- mean
  }else{
    Fcheck <- sum
  }
  xvalid <- tapply(X=!is.na(x),
                     INDEX=period, FUN=Fcheck, na.rm=T)
  xmean[xvalid < necess] <- NA
  return(xmean)
}

shift <- function(x,k) {
  lx <- length(x)
  if(k>0) {
    out <- c(rep(NA,k), x)[1:lx]
  }
  if(k<0) {
    out <- c(x, rep(NA,-k))[(1-k):(lx-k)]
  }
  if(k==0) {
    out <- x
  }
  return(out)
}

stat.window <- function(x,window,necess,FUN=mean) {
  X <- NULL
  for(i in window[1]:window[2]) {
    xx <- shift(x,-i)
    X <- rbind(X,xx)
  }
  xmean <- apply(X, MARGIN=2, FUN=FUN, na.rm=T)
  if(necess<1) {
    Fcheck <- mean
  }else{
    Fcheck <- sum
  }
  xvalid <- apply(X=!(is.na(X)), MARGIN=2,
                  FUN=Fcheck, na.rm=T)
  xmean[xvalid < necess] <- NA
  return(xmean)
}


exc.period <- function(x,period,necess,threshold){
  xx <- x > threshold
  out <- stat.period(xx,period=period,
                     necess=necess,FUN=sum)
  return(out)
}
