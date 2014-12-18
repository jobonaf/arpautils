\name{dbqa.get.datastaz}
\alias{dbqa.get.datastaz}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Estrae i dati di una stazione per un dato parametro (inquinante)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Estrae i dati di una stazione per un dato parametro (inquinante)
}
\usage{
dbqa.get.datastaz(con, ts.range, id.staz, id.param, flg = 1, tstep, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{con}{
connessione al DB
}
  \item{ts.range}{
periodo di interesse, definito come vettore \code{c(datainizio,datafine)}
}
  \item{id.staz}{
codice numerico identificativo della stazione
}
  \item{id.param}{
codice numerico dell'inquinante
}
  \item{flg}{
valore (o valori) accettati per la flag di qualita'
}
  \item{tstep}{
stringa di carattere che identifica il timestep ("H" o "d", orario o giornaliero)
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
argomenti opzionali, passati a \code{dbqa.get.datasens}
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
