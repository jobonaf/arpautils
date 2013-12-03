\name{stat.period}
\alias{stat.period}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
statistiche con aggregazione temporale
}
\description{
calcola una statistica (media, mediana, dev.standard, ...) per ciascun livello di un vettore che identifica l'aggregazione temporale desiderata (mese, ora, ...)
}
\usage{
stat.period(x, period, necess, FUN = mean)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vettore numerico su cui si vuole calcolare la statistica
}
  \item{period}{
vettore (della stessa lunghezza di 'x') che identifica l'aggregazione
}
  \item{necess}{
se >1  numero di dati validi necessari perche' l'aggregazione sia considerata valida
se <1  frazione di dati validi necessari
}
  \item{FUN}{
funzione per calcolare la statistica (e.g. mean, median, sd, ...)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
vettore di lunghezza unique(period) contenente le statistiche
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
G.Bonafe'
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
values <- runif(n=150)
# metto qualche mancante
values[values<0.1] <- NA

month <- c(rep("Jan",50), rep("Feb",50), rep("Mar",50))

stat.period(values, month, necess=0.9, FUN=mean)
stat.period(values, month, necess=45, FUN=mean)
stat.period(values, month, necess=0.9, FUN=median)
stat.period(values, month, necess=0.9, FUN=sd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
