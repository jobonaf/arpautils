## produce le elaborazioni annuali per un generico inquinante

## Operazioni:
## 1) estrazione
## 2) calcolo indicatori e loro validità
## 3) scrittura su DB

prepare.annual_report <- function(con,
                                  id.staz,
                                  id.param=7,
                                  year=NULL,
                                  tstep="d",
                                  ...){
  ## 0) operazioni preliminari
  if(is.null(year)) {
    year <- format(as.POSIXct(paste(Sys.Date(),"00:00"), tz="BST")-60*60*24*30*5,"%Y") # l'anno di 5 mesi fa
  }
  f.date <- as.POSIXct(paste(year,"-12-31 23:59",sep=""), tz="BST") # fine anno
  if(tstep=="H") {
    i.date <- as.POSIXct(paste(Year(f.date)-1,"-12-31 16:00",sep=""), tz="BST") # margine per media mobile    
  } else {
    i.date <- as.POSIXct(paste(Year(f.date),"-01-01 00:00",sep=""), tz="BST")
  }
  
  ## 1) estrazione
  if((as.POSIXct(Sys.time()) - i.date) > 365*2) {
    tab <- "storico"
  } else {
    tab <- "recente"
  }
  if(tstep=="H") {
    bulk_read <- 28  # valore ottimale per estrazione dati orari
  }else{
    bulk_read <- 7  # valore ottimale per estrazione dati giornalieri
  }
  Dat <- dbqa.get.datastaz(con=con,ts.range=c(i.date,f.date),
                           tstep=tstep,
                           id.staz=id.staz,
                           id.param=id.param,
                           #flg=c(0,1),
                           flg=1,
                           bulk_read=bulk_read,
                           table=tab,
                           ...)
  
  out <- list(Dat=Dat, id.staz=id.staz)
  return(out)
}

calculate.annual_report <- function(data,
                                    thr.daily.ave=NULL,
                                    thr.ave8h.max=NULL){
  
  Dat <- data$Dat
  
  if(!is.null(Dat)){
    ## 2) calcolo indicatori e loro validità
    Time  <- index(Dat)
    day   <- Ymd(Time)
    yDat  <- last(Dat,'1 year') # solo dati ultimo anno
    yTime <- index(yDat)
    yday  <- Ymd(yTime)
    yDatR <- round(as.vector(yDat)) # solo dati ultimo anno, arrotondati
    
    ## distingue dati orari-giornalieri
    hourly <- difftime(Time[2],Time[1],units="hours")==1
    daily  <- difftime(Time[2],Time[1],units="days")==1
    if(!hourly & !daily) stop("cannot manage timestep!")
    
    ## calcola media annua senza arrotondamenti
    annual.mean      <- mean(yDat, na.rm=T)
    annual.nValid    <- sum(as.numeric(!is.na(yDat)))
    annual.percValid <- annual.nValid/length(yDat)*100
    if(hourly) annual.nExpected <- length(yDatR)/24*23
    if(daily)  annual.nExpected <- length(yDatR)-4
    annual.efficiency <- annual.nValid/annual.nExpected*100
    
    annual.report <- data.frame(annual.mean      =annual.mean,
                                annual.nValid    =annual.nValid,
                                annual.percValid =annual.percValid,     
                                annual.nExpected =annual.nExpected,
                                annual.efficiency=annual.efficiency)
  } else {
    annual.report <- data.frame(annual.mean      =NA,
                                annual.nValid    =NA,
                                annual.percValid =NA,     
                                annual.nExpected =NA,
                                annual.efficiency=NA)
  }
  
  ## calcola superamenti giornalieri senza arrotondare
  if(!is.null(thr.daily.ave)){
    if(!is.null(Dat)){
      if(hourly) dDat <- stat.period(x=yDat,period=yday,necess=18,FUN=mean)
      if(daily)  dDat <- Dat
      daily.nexc      <- sum(as.numeric(dDat>thr.daily.ave), na.rm=T)
      daily.nValid    <- sum(as.numeric(!is.na(dDat)))
      daily.percValid <- daily.nValid/length(dDat)*100
      
      annual.report <- data.frame(annual.report,
                                  daily.nexc     =daily.nexc,
                                  daily.nValid   =daily.nValid,
                                  daily.percValid=daily.percValid)
    }else{
      annual.report <- data.frame(annual.report,
                                  daily.nexc     =NA,
                                  daily.nValid   =NA,
                                  daily.percValid=NA)
    }
  }
  
  ## calcola superamenti giornalieri senza arrotondare
  if(!is.null(thr.ave8h.max)){
    if(!is.null(Dat)){
      if(hourly) ave.8h <- round(mean.window(x=as.vector(Dat),k=8,necess=6))
      if(daily)  stop("cannot calculate 8h moving average for daily data!")
      max.ave.8h <- stat.period(x=ave.8h,period=day,necess=18,FUN=max)[-1]
      ave8h.nexc      <- sum(as.numeric(max.ave.8h>thr.ave8h.max), na.rm=T)
      ave8h.nValid    <- sum(as.numeric(!is.na(max.ave.8h)))
      ave8h.percValid <- ave8h.nValid/length(max.ave.8h)*100
      
      annual.report <- data.frame(annual.report,
                                  ave8h.nexc     =ave8h.nexc,
                                  ave8h.nValid   =ave8h.nValid,
                                  ave8h.percValid=ave8h.percValid)
    }else{
      annual.report <- data.frame(annual.report,
                                  ave8h.nexc     =NA,
                                  ave8h.nValid   =NA,
                                  ave8h.percValid=NA)
    }
  }
    
  Out <- list(annual.report=annual.report,
              id.staz=data$id.staz,
              first.time=yTime[1],
              last.time=yTime[length(yTime)])
  return(Out)
}


## DA SCRIVERE DOPO PREDISPOSIZIONE TABELLA SU DB
##write.annual_report <- function()