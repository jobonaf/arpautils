\name{aqstat.functions}
\alias{stat.period}
\alias{stat.period2}
\alias{which.period}
\alias{stat.window}
\alias{exc.period}
\alias{shift}
\alias{mean.window}
\alias{detect.event}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Funzioni base per il calcolo di statistiche di legge su dati QA.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Funzioni base per il calcolo di statistiche di legge su dati QA.
}
\usage{
stat.period(x, period, necess, FUN = mean)
stat.period2(x, period, nmax.missing, FUN = mean)
which.period(x, period, necess, FUN=which.max)
exc.period(x, period, necess, threshold)

stat.window(x, window, necess, FUN = mean)
mean.window(x, k, necess)

detect.event(x, threshold)

shift(x, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
vettore dei valori di concentrazione
}
  \item{period}{
%%     ~~Describe \code{period} here~~
vettore, della stessa lunghezza di \code{x}, che identifica i periodi
}
  \item{window}{
%%     ~~Describe \code{period} here~~
vettore numerico di due elementi; descrive l'ampiezza della finestra mobile, p.es. \code{c(-7,0)} per la media mobile su 8 ore
}
  \item{necess}{
%%     ~~Describe \code{necess} here~~
numero di dati validi necessari in ciascun periodo
}
  \item{nmax.missing}{
%%     ~~Describe \code{nmax.missing} here~~
numero massimo di dati mancanti accettabili in ciascun periodo
}
  \item{FUN}{
%%     ~~Describe \code{FUN} here~~
funzione da applicare
}
  \item{threshold}{
%%     ~~Describe \code{threshold} here~~
soglia
}
  \item{k}{
%%     ~~Describe \code{k} here~~
in \code{shift}, numero di passi di cui si vuole spostare \code{x}; in \code{mean.window}, ampiezza della finestra
}}
\details{
%%  ~~ If necessary, more details than the description above ~~
Le funzioni \code{stat.period} e \code{stat.period2} calcolano una statistica \code{FUN} su periodi definiti, con approcci diversi nella gestione dei mancanti. La funzione \code{which.period} lavora come \code{stat.period}, ma si puo' usare per funzioni (come \code{which.min} o \code{which.max}) che non accettano l'argomento \code{na.rm}.

Invece \code{stat.window} opera su una finestra mobile, e chiama \code{shift} che sposta la serie temporale in avanti o indietro nel tempo. Piu' efficiente, ma limitata alla media mobile, e' \code{mean.window}.

La funzione \code{exc.period} fa il conteggio dei superamenti di una data soglia.
Invece \code{detect.event} restituisce una matrice contenente la data e l'orario dei superamenti e la loro durata (espressa in numero di timestep).
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
G.Bonafè
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
