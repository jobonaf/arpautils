\name{dbqa.data2xts}
\alias{dbqa.data2xts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Converte i dati estratti da DB in un oggetto di tipo \code{xts}
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Converte i dati estratti da DB in un oggetto di tipo \code{xts}
}
\usage{
dbqa.data2xts(data, Date = "TS_INIZIO_RIL", Value = "VALORE", TZ="Africa/Algiers")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
matrice o dataframe che include i dati, cosi' come sono estratti dal DB
}
  \item{Date}{
%%     ~~Describe \code{Date} here~~
il nome del campo che contiene data (e ora) in \code{data}
}
  \item{Value}{
%%     ~~Describe \code{Value} here~~
il nome del campo che contiene le concentrazioni in \code{data}
}
  \item{TZ}{
%%     ~~Describe \code{Value} here~~
timezone (vedi Warning)
}
}
\section{Warning}{
  Si fa riferimento all'ora ``Africa/Algiers'' (British Summer Time) poiche' (assieme a ``Africa/Algiers'') e' l'unico standard codificato corrispondente all'ora del DB, nonche' all'ora a cui si riferisce la normativa: Central Europe Time senza Daylight Saving Time (ora legale).
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
Giovanni Bonafè, Arpa Emilia-Romagna
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
