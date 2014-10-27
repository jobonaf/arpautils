## produce le elaborazioni annuali sui dati di ozono

## Operazioni:
## 1) estrazione
## 2) calcolo indicatori e loro validità
## 3) identificazione eventi
## 4) scrittura su DB

prepare.ozone_annual_report <- function(con,
                                       id.staz,
                                       year=NULL,
                                       ...){
  ## 0) operazioni preliminari
  if(is.null(year)) {
    year <- format(as.POSIXct(paste(Sys.Date(),"00:00"), tz="BST")-60*60*24*30*5,"%Y") # l'anno di 5 mesi fa
  }
  f.date <- as.POSIXct(paste(year,"-12-31 23:59",sep=""), tz="BST") # fine anno
  i.date <- as.POSIXct(paste(Year(f.date)-1,"-12-31 16:00",sep=""), tz="BST") # margine per media mobile
  
  
  ## 1) estrazione
  if((as.POSIXct(Sys.time()) - i.date) > 365*2) {
    tab <- "storico"
  } else {
    tab <- "recente"
  }
  Dat <- dbqa.get.datastaz(con=con,ts.range=c(i.date,f.date),
                           tstep="H",
                           id.staz=id.staz,
                           id.param=7,
                           #flg=c(0,1),
                           flg=1,
                           bulk_read=28,  # valore ottimale
                           table=tab,
                           ...)
  
  out <- list(Dat=Dat, id.staz=id.staz)
  return(out)
}

calculate.ozone_annual_report <- function(data){
  
  Dat <- data$Dat
  
  if(!is.null(Dat)){
    ## 2) calcolo indicatori e loro validità
    Time <- index(Dat)
    day <- Ymd(Time)
    yDat <- last(Dat,'1 year') # solo dati ultimo anno
    yTime <- index(yDat)
    yday <- Ymd(yTime)
    yDatR <- round(as.vector(yDat)) # solo dati ultimo anno, arrotondati
    # - max media mobile 8h (con arrotondamento)
    ave.8h <- round(mean.window(x=as.vector(Dat),k=8,necess=6))
    max.ave.8h <- stat.period(x=ave.8h,period=day,necess=18,FUN=max)[-1]
    ## - no. sup. orari soglia 180 da inizio anno (valori arrotondati)
    cumul.nexc.180 <- sum(as.numeric(yDatR>180), na.rm=T)
    ## - no. sup. orari soglia 240 da inizio anno (valori arrotondati)
    cumul.nexc.240 <- sum(as.numeric(yDatR>240), na.rm=T)
    ## - no. sup. giorn. soglia 120 da inizio anno
    cumul.nexc.120 <- sum(as.numeric(max.ave.8h>120), na.rm=T)
    ## - AOT40 annuale vegetazione
    vegetIdx <- as.numeric(format(Time, format="%m")) %in% 5:7
    vegetDat <- Dat[vegetIdx]
    vegetTime <- Time[vegetIdx]
    vegetHour <- as.numeric(format(vegetTime, format="%H"))
    dum <- aot(vegetDat, vegetHour, threshold=80, estimate=T, hr.min=8, hr.max=19)
    aot40.veget <- round(dum$Aot)
    aot40.veget.PercValid <- dum$PercValid
    ## - AOT40 annuale foreste
    forestIdx <- as.numeric(format(Time, format="%m")) %in% 4:9
    forestDat <- Dat[forestIdx]
    forestTime <- Time[forestIdx]
    forestHour <- as.numeric(format(forestTime, format="%H"))
    dum <- aot(forestDat, forestHour, threshold=80, estimate=T, hr.min=8, hr.max=19)
    aot40.forest <- round(dum$Aot)
    aot40.forest.PercValid <- dum$PercValid
    
    annual.report <- data.frame(cumul.nexc.180=cumul.nexc.180,
                                cumul.nexc.240=cumul.nexc.240,
                                cumul.nexc.120=cumul.nexc.120,
                                aot40.veget=           aot40.veget,
                                aot40.veget.PercValid= aot40.veget.PercValid,
                                aot40.forest=          aot40.forest,
                                aot40.forest.PercValid=aot40.forest.PercValid)
    
  } else {
    annual.report <- data.frame(cumul.nexc.180=        NA,
                                cumul.nexc.240=        NA,
                                cumul.nexc.120=        NA,
                                aot40.veget=           NA,
                                aot40.veget.PercValid= NA,
                                aot40.forest=          NA,
                                aot40.forest.PercValid=NA)
  }
  
  ## 3) identificazione eventi
  if(!is.null(Dat)){
    Ev180 <- detect.event(yDatR,180)
    if(is.data.frame(Ev180)) {
      Time180 <- index(yDat)[Ev180$index]
      Max180 <- Ev180$max
      events.180 <- data.frame(start.time=Time180,
                               duration=Ev180$duration,
                               max=Max180)
    } else {
      events.180 <- NULL
    }
    Ev240 <- detect.event(yDatR,240)
    if(is.data.frame(Ev240)) {
      Time240 <- index(yDat)[Ev240$index]
      Max240 <- Ev240$max
      events.240 <- data.frame(start.time=Time240,
                               duration=Ev240$duration,
                               max=Max240)      
    } else {
      events.240 <- NULL
    }
    events <- list(exc.180=events.180,
                   exc.240=events.240)
  } else {
    events <- NULL
  }
  
  Out <- list(annual.report=annual.report,
              events=events,
              id.staz=data$id.staz,
              first.time=yTime[1],
              last.time=yTime[length(yTime)])
  return(Out)
}


## DA SCRIVERE DOPO PREDISPOSIZIONE TABELLA SU DB
##write.ozone_annual_report <- function()