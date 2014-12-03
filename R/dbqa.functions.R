## Funzioni di accesso e interrogazione al DB QA

## credenziali accesso DB
dbqa.config <- function(db_usr, db_pwd, db_name, db_tz) {
  Sys.setenv(TZ=db_tz)
  cfg <- list(db_usr=db_usr, db_pwd=db_pwd, db_name=db_name)
}

## funzione per connettersi al DB di qualità dell'aria Arpa ER
## (BST = ora locale italiana senza DST)
dbqa.connect <- function(db_usr, db_pwd, db_name, db_tz="BST") { 
  ## Carichiamo la libreria Oracle
  ## Carichiamo il driver
  drv <- dbDriver("Oracle")
  ## credenziali accesso DB
  cfg <- dbqa.config(db_usr, db_pwd, db_name, db_tz)
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
  if(table=="storico") {
    tab <- c("AA_ARIA.T$01$DATI_STORICO d",
             "AA_ARIA.VO$01$CONFIG_SENSORI cs")    
  } else {
    tab <- c("AA_ARIA.T$01$DATI d",
             "AA_ARIA.VO$01$CONFIG_SENSORI cs")    
    
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
  query <- "select distinct ID_STAZIONE, NOME_STAZIONE, COMUNE, PROVINCIA from AA_ARIA.ANG_CONFIG_SENSORI"
  data <- fetch(ds <- dbSendQuery(con, query))
  idx <- order(data[, 2])
  Data <- data[idx, ]
  rownames(Data) <- rownames(data)
  FUN(Data)
}

dbqa.list.active.staz <- function(con,
                                  prov,
                                  Day=Sys.Date()) {
  day <- format(Day,format="%Y-%m-%d")
  query <- paste("SELECT ID_CONFIG_STAZ ",
                 "FROM aa_aria.VO$01$CONFIG_STAZIONI cst ",
                 "WHERE TRUNC (to_date('",
                 day,
                 "','YYYY-MM-DD')) BETWEEN cst.dth_i_vld ",
                 "AND NVL (cst.dth_f_vld, to_date('",
                 day,
                 "','YYYY-MM-DD')) ",
                 "AND NVL (cst.flg_mobile, 0) = 0 ",
                 "AND COD_PRV = '",prov,"'",sep="")
  data <- dbGetQuery(con, query)
  Data <- as.character(unlist(data))
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
                              ...) {
  cfgsens <- dbqa.get.idcfgsens(con,
                                id.param,
                                i.date=ts.range[1],
                                f.date=ts.range[2],
                                id.staz)
  DATA <- NULL
  nsens <- length(cfgsens$idcfgsens)
  #print(cfgsens$idcfgsens)
  if(nsens>0) for (i in 1:nsens) {
    idate <- cfgsens$idate[i]
    fdate <- cfgsens$fdate[i]
    if(is.na(idate)) idate <- as.POSIXct(ts.range[1])
    if(is.na(fdate)) fdate <- as.POSIXct(ts.range[2])
#     print(con)
#     print(c(idate,fdate))
#     print(cfgsens$idcfgsens[i])
#     print(id.param)
#     print(flg)
    dat <- dbqa.get.datasens (con=con,
                              ts.range=c(idate,fdate),
                              id.cfgsens=cfgsens$idcfgsens[i],
                              id.param=id.param,
                              flg=flg,
                              ...)
    Dat <- dbqa.data2xts(dat)
    Dreg <- xts.regolarize(tstep,Dat,
                           f.time=as.POSIXct(ts.range[1], TZ="BST"),
                           l.time=as.POSIXct(ts.range[2], TZ="BST"))
    if(i>1) {
      DATA <- xts.blend(tstep, TZ="BST", DATA, Dreg)
    } else {
      DATA <- Dreg
    }
    #DATA <- data.frame(rbind(DATA,dat))
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
dbqa.round <- function(x,id.param) {
  #                              O3 PM10 PM2.5 NO2  NOx SO2 C6H6   CO  BaP   As   Cd   Ni   Pb
  dat <- data.frame(idparam=c(   7,   5, 111,   8,   9,   1,  20,  10,  29,  18,  14,  15,  12),
                    digits= c(   0,   0,   0,   0,   0,   0,   1,   1,   4,   3,   3,   3,   6))
  idx <- which(id.param==dat$idparam)
  if(length(idx)==1) {
    dig <- dat$digits[idx]
    out <- round(x,dig)    
  } else {
    out <- x
  }
  return(out)
}