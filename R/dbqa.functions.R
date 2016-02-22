## Funzioni di accesso e interrogazione al DB QA

## credenziali accesso DB
dbqa.config <- function(db_usr=NULL, 
                        db_pwd=NULL, 
                        db_name=NULL, 
                        db_tz="Africa/Algiers",
                        config.file="~/util/R/dbqa_keys.R",
                        global=TRUE) {
  if(is.null(db_usr)) {
    if(!file.exists(config.file)) stop(paste("Cannot find file",config.file))
    cat(paste("Reading dbqa configuration from file",config.file),sep = "\n")
    source(config.file,local = TRUE)
  }
  if(global) {
    cat("Setting dbqa configuration globally",sep = "\n")
    db_usr  <<- db_usr
    db_pwd  <<- db_pwd
    db_name <<- db_name
    db_tz   <<- db_tz
    Sys.setenv(TZ=db_tz)
  }
  cfg <- list(db_usr=db_usr, db_pwd=db_pwd, db_name=db_name, db_tz=db_tz)
  return(cfg)
}

## funzione per connettersi al DB di qualita' dell'aria Arpa ER
## (Africa/Algiers = ora locale italiana senza DST)
dbqa.connect <- function(...) { 
  ## Carichiamo la libreria Oracle
  ## Carichiamo il driver
  drv <- dbDriver("Oracle")
  ## credenziali accesso DB
  cfg <- dbqa.config(...)
  ## Creiamo la connessione al DB
  con <- dbConnect(drv,
                   username = cfg$db_usr,
                   password = cfg$db_pwd,
                   dbname   = cfg$db_name)
  return(con)
}


## scrive in una stringa una semplice query della forma
## "SELECT what FROM tab [WHERE crit]"
simple.query <- function(tab,what,crit) {
  out <- paste("SELECT",paste(what,collapse=","),
               "FROM",paste(tab,collapse=","),
               ("WHERE")[!is.null(crit)],paste(crit,collapse=" AND "))
  return(out)
}


## dato un ID parametro e sensore,
## scarica i dati disponibili cosi' come sono
dbqa.get.datasens <- function(con,
                              ts.range,
                              id.cfgsens,
                              id.param,
                              flg=1,
                              flg.excl=NULL,
                              flg.null=FALSE,
                              verbose=FALSE,
                              table="storico",
                              ...) {
  
  ## costruisce query
  if(table=="storico") {
    tab <- c("AA_ARIA.T$01$DATI_STORICO d",
             "AA_ARIA.T$01$CONFIG_SENSORI cs")    
  } else {
    tab <- c("AA_ARIA.T$01$DATI d",
             "AA_ARIA.T$01$CONFIG_SENSORI cs")    
    
  }
  what <- c("TO_CHAR(d.TS_INIZIO_RIL,'YYYY-MM-DD HH24:MI') as TS_INIZIO_RIL",          ## OCCHIO CON LA DATA, CONTROLLARE
            "d.VALORE * cs.COEFF_CONV + cs.VAL_OFFSET as VALORE",
            "d.FLG_A as FLG_A")
  crit <- c(paste("d.ID_PARAMETRO=",id.param,sep=""),
            paste("d.ID_CONFIG_SENSORE=",id.cfgsens,sep=""),
            "d.ID_CONFIG_SENSORE=cs.ID_CONFIG_SENSORE",
            paste("d.TS_INIZIO_RIL between to_date('",
                  format(as.POSIXct(ts.range[1]),"%Y-%m-%d %H:%M"),
                  "','YYYY-MM-DD HH24:MI') and to_date('",
                  format(as.POSIXct(ts.range[2]),"%Y-%m-%d %H:%M"),
                  "','YYYY-MM-DD HH24:MI')",sep=""))
  
  ## gestisce flag
  if(!is.null(flg) | flg.null) {
    CF <- NULL
    if(!is.null(flg)) {
      cf <- paste("(",
                  paste(paste("FLG_A=",flg,sep=""),collapse=" OR "),
                  ")",sep="")
      CF <- c(CF,cf)
    }        
    if(flg.null) {
      cf <- "(FLG_4 IS NULL)"
      CF <- c(CF,cf)
    }
    crit <- c(crit,paste("(",paste(CF, collapse=" OR "),")",sep=""))
  }
  if(!is.null(flg.excl)) {
    crit <- c(crit,
              paste("(",
                    paste(paste("FLG_A!=",flg.excl,sep=""),collapse=" AND "),
                    ")",sep=""))
  }        
  
  ## estrae dati
  query <- simple.query(tab,what,crit)
  if(verbose)  print(query)
  data <- dbGetQuery(con, query, ...)
  
  # arrotondamento in visualizzazione
  data$VALORE <- dbqa.round(x=data$VALORE, id.param=id.param)
  return(data)
}

## dato ID parametro, stazione, data inizio e fine,
## ottiene tutti gli ID sensore corrispondenti
dbqa.get.idcfgsens <- function(con,
                               id.param,
                               i.date=NULL,
                               f.date=NULL,
                               id.staz) {
  tab <- "AA_ARIA.ANG_CONFIG_SENSORI"
  crit <- c(paste("ID_PARAMETRO=",id.param,sep=""),
            paste("ID_STAZIONE=",id.staz,sep=""))
  if(!is.null(i.date)) {
    crit <- c(crit,
              paste("(DATA_FINE is null OR to_char(DATA_FINE,'YYYYMMDD')>='",
                    format(as.POSIXct(i.date),format="%Y%m%d%H"),
                    "')", sep=""))
  }
  if(!is.null(f.date)) {
    crit <- c(crit,
              paste("(DATA_INIZIO is null OR to_char(DATA_INIZIO,'YYYYMMDD')<='",
                    format(as.POSIXct(f.date),format="%Y%m%d%H"),
                    "')", sep=""))
  }
  #print(crit)
  query.idcfgsens <- simple.query(tab,what="ID_CONFIG_SENSORE",crit)
  #print(query.idcfgsens)
  idcfgsens <- dbGetQuery(con, query.idcfgsens)[,1]
  if(length(idcfgsens)>0) {
    query.idate <- simple.query(tab,what="DATA_INIZIO",crit)
    idate <- dbGetQuery(con, query.idate)[,1]
    idate <- pmax(as.POSIXct(idate),as.POSIXct(i.date))
    query.fdate <- simple.query(tab,what="DATA_FINE",crit)
    fdate <- dbGetQuery(con, query.fdate)[,1]
    fdate <- pmin(as.POSIXct(fdate),as.POSIXct(f.date))
    idx <- order(idate,decreasing=F)
    idate <- idate[idx]
    fdate <- fdate[idx]
    idcfgsens <- idcfgsens[idx]
  } else {
    idcfgsens=NULL
    idate=NULL
    fdate=NULL
  }
  
  out <- list(idcfgsens=idcfgsens, idate=idate, fdate=fdate)
  return(out)

}


## elenca le tabelle disponibili
dbqa.list.tables <- function(con) {
  tr <- dbSendQuery(con, "select table_name from all_tables")
  tables <- paste("AA_ARIA.",
                  sort(fetch(tr)[,1]),sep="")
  return(tables)
}

## elenca i campi disponibili in una tabella
dbqa.list.fields <- function(con,tab) {
  query <- paste("select * from ",tab," where rownum=0")
#  print(query)
  data <- dbGetQuery(con, query)
  out <- colnames(data)
  return(out)
}

## visualizza in tabella i parametri (inquinanti)
## con i relativi ID
dbqa.view.param <- function(con,FUN=View) {
  query <- "select ID_PARAMETRO,NOME from AA_ARIA.T$00$PARAMETRI"
  data <- fetch(ds <- dbSendQuery(con, query))
  idx <- order(data[,2])
  Data <- data[idx,]
  rownames(Data) <- rownames(data)
  FUN(Data)
}

dbqa.view.staz <- function(con,FUN=View) {
  query <- "select distinct ID_STAZIONE, NOME_STAZIONE, COMUNE, PROVINCIA, TIPOSTAZ, ZONA from AA_ARIA.ANG_CONFIG_SENSORI"
  data <- fetch(ds <- dbSendQuery(con, query))
  idx <- order(data[, 2])
  Data <- data[idx, ]
  rownames(Data) <- rownames(data)
  FUN(Data)
}

dbqa.isrrqa <- function(con,Id) {
  isr <- function(id) substr(dbGetQuery(con,paste("select aa_web.pk$99$zone.isRRQA_NEW (",
                                                  id,
                                                  ") from dual",
                                                  sep="")),
                             1,4)=="RRQA"
  mapply(FUN=isr, Id)
}

dbqa.list.active.staz <- function(con,
                                  prov=c("PC","PR","RE","MO","BO","FE","RA","FC","RN"),
                                  Day=Sys.Date(),
                                  mobile=FALSE) {
  day <- format(Day,format="%Y-%m-%d")
  query <- paste("SELECT ID_CONFIG_STAZ ",
                 "FROM aa_aria.VO$01$CONFIG_STAZIONI cst ",
                 "WHERE TRUNC (to_date('",
                 day,
                 "','YYYY-MM-DD')) BETWEEN cst.dth_i_vld ",
                 "AND NVL (cst.dth_f_vld, to_date('",
                 day,
                 "','YYYY-MM-DD')) ",
                 if(!mobile) "AND NVL (cst.flg_mobile, 0) = 0 ",
                 "AND COD_PRV IN (",
                 paste0("'",prov,"'",collapse=","),
                 ")",sep="")
  data <- dbGetQuery(con, query)
  Data <- as.character(sort(as.numeric(unlist(data))))
  names(Data) <- NULL
  return(Data)
}

## dato un ID parametro e stazione,
## scarica i dati disponibili dai sensori
## corrispondenti, così come sono
dbqa.get.datastaz <- function(con,
                              ts.range,
                              id.staz,
                              id.param,
                              flg=1,
                              tstep,
                              lod.manage=ifelse({id.param %in% c(12,14,15,18,29)},  #metalli e BaP
                                                "half",
                                                "keep"),
                              ...) {
  cfgsens <- dbqa.get.idcfgsens(con,
                                id.param,
                                i.date=ts.range[1],
                                f.date=ts.range[2],
                                id.staz)
  DATA <- NULL
  nsens <- length(cfgsens$idcfgsens)
  #print(lod.manage) 
  if(nsens>0) for (i in 1:nsens) {
    idate <- cfgsens$idate[i]
    fdate <- cfgsens$fdate[i]
    if(is.na(idate)) idate <- as.POSIXct(ts.range[1])
    if(is.na(fdate)) fdate <- as.POSIXct(ts.range[2])
    dat <- dbqa.get.datasens (con=con,
                              ts.range=c(idate,fdate),
                              id.cfgsens=cfgsens$idcfgsens[i],
                              id.param=id.param,
                              flg=flg,
                              ...)
    Dat <- dbqa.data2xts(dat)
    Dreg <- xts.regolarize(tstep,Dat,
                           f.time=as.POSIXct(ts.range[1], TZ="Africa/Algiers"),
                           l.time=as.POSIXct(ts.range[2], TZ="Africa/Algiers"))
    if(i>1) {
      DATA <- xts.blend(tstep, TZ="Africa/Algiers", DATA, Dreg)
    } else {
      DATA <- Dreg
    }
    #DATA <- data.frame(rbind(DATA,dat))
  }
  
  ## gestisce il LOD (metalli e BaP)
  if(lod.manage=="half") {
    lod <- dbqa.lod(con=con, id.param=id.param, days=index(DATA))
    DATA <- pmax(DATA,lod/2)
  }
  return(DATA)
}

## scrive o aggiorna valori nei campi di una tabella
dbqa.insert <- function(con, tab, values, columns=colnames(values), 
                        to_date=NULL,
                        update=F, verbose=F) {
  nv <- ncol(values)
  if(update) {
    action <- "update"
  } else {
    action <- "insert"    
  }
  vv <- paste(":",1:nv,sep="")
  vv[to_date] <- paste("TO_DATE(",vv[to_date],",'YYYY-MM-DD HH24:MI')",sep="")
  vvv <- paste(vv,collapse=", ")
  insStr <- paste(action," into ",
                  tab,
                  " (",
                  paste(columns,collapse=", "),
                  ") values (",
                  vvv,
                  ")",sep="")
  if(verbose)print(insStr)
  if(verbose)print(values)
  dbGetQuery(conn=con, statement=insStr, values)
}

## elimina record da una tabella
dbqa.delete <- function(con, tab, keys, values, verbose=F) {
  chkStr <- paste("select * from ",
                  tab,
                  " where ",
                  paste(paste(keys,values,sep="="),collapse=" AND "),
                  sep="")
  check <- dbGetQuery(conn=con, statement=chkStr)
  dbCommit(con)
  if(verbose) print(check)
  if(nrow(check)>0) {
    delStr <- paste("delete from ",
                    tab,
                    " where ",
                    paste(paste(keys,values,sep="="),collapse=" AND "),
                    sep="")
    if(verbose)print(delStr)
    dbGetQuery(conn=con, statement=delStr)    
  }
}

# arrotondamenti in visualizzazione, secondo indicazioni GdL
round_awayfromzero <- function(x,digits=0) trunc(x*10^digits+sign(x)*0.5)*10^-digits
dbqa.round <- function(x,id.param) {
  #                              O3 PM10 PM2.5 NO2  NOx SO2 C6H6   CO  BaP   As   Cd   Ni   Pb
  dat <- data.frame(idparam=c(   7,   5, 111,   8,   9,   1,  20,  10,  29,  18,  14,  15,  12),
                    digits= c(   0,   0,   0,   0,   0,   0,   1,   1,   4,   3,   3,   3,   6))
  idx <- which(id.param==dat$idparam)
  if(length(idx)==1) {
    dig <- dat$digits[idx]
    out <- round_awayfromzero(x,dig)    
  } else {
    out <- x
  }
  return(out)
}

# restituisce limite di quantificazione
dbqa.lod <- function(con, id.param, days=Sys.Date()) {
  days <- as.POSIXct(days)
  qqq <- paste0("select * from DETECTION_LIMIT where",
                " ID_PARAMETRO=",id.param)
  ddd <- dbGetQuery(con,qqq)
  matchRow <- function(day) {
    idx <- which(day >= ddd$DT_INIZIO_VALID & 
                   (is.na(ddd$DT_FINE_VALID) || day <= ddd$DT_FINE_VALID))
    if(length(idx)==0) idx <- NA
    return(idx[1])
  }
  idx <- unlist(lapply(days, matchRow))
  out <- ddd$DL[idx]
  return(out)
}


## converte nome inquinante in ID numerico
dbqa.get.idparam <- function(poll, con=NULL) {
  id.param <- NULL
  id.param <- switch(poll,
                     "SO2"=1,
                     "PM10"=5,
                     "PM2.5"=111,
                     "NO2"=8,
                     "NO"=38,
                     "NOx"=9,
                     "NOX"=9,
                     "CO"=10,
                     "O3"=7,
                     "BaP"=29,
                     "Ni"=15,
                     "As"=18,
                     "Cd"=14,
                     "Pb"=12)
  
  ## se non ? predefinito, lo cerca nei nomi del DB
  if(is.null(id.param)) {
    if(is.null(con)) {
      cat("Cannot search in the DB without 'con' argument",sep="\n")
    } else {
      ppp <- dbqa.view.param(con, FUN = return)
      fff <- agrep(poll, x = ppp$NOME, ignore.case = T)
      if(length(fff)==1) {
        cat(paste0("Selected '",ppp$NOME[fff],"'"),sep="\n")
        id.param <- ppp$ID_PARAMETRO[fff]
      } else if(length(fff)==0) {
        cat(paste("Cannot find",poll), sep="\n")
      } else if(length(fff)>1) {
        cat(paste0("Sorry, string '",poll,"' may refer to different IDs:"), sep="\n")
        cat(paste(paste0(ppp$ID_PARAMETRO,": '",ppp$NOME,"'")[fff],collapse="\n"),sep="\n")
        cat("",sep="\n")
      }
    }
  }
  if(is.null(id.param)) cat("Pollutant not managed",cat="\n")
  return(id.param)
}


## funzione di estrazione di un'elaborazione annuale
dbqa.get.elab <- function(con, 
                          year, 
                          id.param,
                          id.elab, 
                          type.elab="F",
                          only.rrqa=T, 
                          only.valid=T,
                          keep.all=F) {
  qqq <- paste("select * from WEB_STAT where ",
               "TO_CHAR(GIORNO,'YYYY')='",year,"' ",
               " and ID_PARAMETRO=",id.param,
               sep="")
  if(!is.null(id.elab)) qqq <- paste(qqq,"and ID_ELABORAZIONE=",id.elab)
  if(only.valid) qqq <- paste(qqq,"and FLG_ELAB=1")
  #print(qqq)
  dat <- dbGetQuery(con,qqq)
  
  # tiene solo RRQA se richiesto
  if(only.rrqa) {
    idx <- dbqa.isrrqa(con,dat$ID_CONFIG_STAZ)
    idx[is.na(idx)] <- FALSE
    if(length(idx)==0) return(NULL)
    dat <- dat[which(idx),]
  }
  # esclude doppioni piu' vecchi
  dat <- dat[order(dat$TS_UPD, decreasing=T),]
  dat <- dat[order(dat$TS_INS, decreasing=T),]
  idx <- paste(dat$ID_CONFIG_STAZ,dat$ID_ELABORAZIONE,dat$ID_EVENTO)
  dat <- dat[which(!duplicated(idx)),]
  
  
  if(keep.all) { # output originale completo
    out <- dat  
  } else {  # oppure colonne selezionate (pi? leggibile)
    out <- data.frame(ID_CONFIG_STAZ=dat$ID_CONFIG_STAZ, 
                      V_ELAB=dat[,paste("V_ELAB_",type.elab,sep="")],
                      ID_ELAB=dat$ID_ELABORAZIONE,
                      VERSION=dat$TS_INS,
                      FLAG_ELAB=dat$FLG_ELAB)
  }
  return(out)
}

## restituisce descrizione di una o pi? elaborazioni statistiche
dbqa.descr.elab <- function(con, id.elab=NULL) {
  qqq <- "select ID_ELABORAZIONE,DES_ELABORAZIONE from WEB_ELAB"
  dat <- dbGetQuery(conn = con,qqq)
  dat <- dat[match(id.elab,dat$ID_ELABORAZIONE), ,drop=F] ## riordina in base alla richiesta
  return(dat)
}


