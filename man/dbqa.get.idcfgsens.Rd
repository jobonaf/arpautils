\name{dbqa.get.idcfgsens}
\alias{dbqa.get.idcfgsens}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Estrae l'elenco dei codici sensore corrispondenti ad una data stazione, per un dato periodo, per un dato parametro (inquinante)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Estrae l'elenco dei codici sensore corrispondenti ad una data stazione, per un dato periodo, per un dato parametro (inquinante)
}
\usage{
dbqa.get.idcfgsens(con, id.param, i.date = NULL, f.date = NULL, id.staz)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{con}{
%%     ~~Describe \code{con} here~~
connessione al DB
}
  \item{id.param}{
%%     ~~Describe \code{id.param} here~~
codice numerico dell'inquinante
}
  \item{i.date}{
%%     ~~Describe \code{i.date} here~~
data iniziale
}
  \item{f.date}{
%%     ~~Describe \code{f.date} here~~
data finale
}
  \item{id.staz}{
%%     ~~Describe \code{id.staz} here~~
codice numerico identificativo della stazione
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Giovanni Bonafe', Arpa Emilia-Romagna
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (con, id.param, i.date = NULL, f.date = NULL, id.staz) 
{
    tab <- "AA_ARIA.ANG_CONFIG_SENSORI"
    crit <- c(paste("ID_PARAMETRO=", id.param, sep = ""), paste("ID_STAZIONE=", 
        id.staz, sep = ""))
    if (!is.null(i.date)) {
        crit <- c(crit, paste("(DATA_FINE is null OR to_char(DATA_FINE,'yyyymmdd')>='", 
            format(as.POSIXct(i.date), format = "\%Y\%m\%d"), "')", 
            sep = ""))
    }
    if (!is.null(f.date)) {
        crit <- c(crit, paste("(DATA_INIZIO is null OR to_char(DATA_INIZIO,'yyyymmdd')<='", 
            format(as.POSIXct(f.date), format = "\%Y\%m\%d"), "')", 
            sep = ""))
    }
    library("ROracle")
    query.idcfgsens <- simple.query(tab, what = "ID_CONFIG_SENSORE", 
        crit)
    idcfgsens <- fetch(ds <- dbSendQuery(con, query.idcfgsens))[, 
        1]
    query.idate <- simple.query(tab, what = "DATA_INIZIO", crit)
    idate <- fetch(ds <- dbSendQuery(con, query.idate))[, 1]
    idate <- pmax(as.POSIXct(idate), as.POSIXct(i.date))
    query.fdate <- simple.query(tab, what = "DATA_FINE", crit)
    fdate <- fetch(ds <- dbSendQuery(con, query.fdate))[, 1]
    fdate <- pmin(as.POSIXct(fdate), as.POSIXct(f.date))
    idx <- order(idate, decreasing = F)
    idate <- idate[idx]
    fdate <- fdate[idx]
    idcfgsens <- idcfgsens[idx]
    out <- list(idcfgsens = idcfgsens, idate = idate, fdate = fdate)
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
