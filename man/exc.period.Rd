\name{exc.period}
\alias{exc.period}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
conteggio di superamenti di una soglia
}
\description{
conta i superamenti di una soglia per ciascun livello di un vettore che identifica l'aggregazione temporale desiderata (mese, ora, ...)
}
\usage{
exc.period(x, period, necess, threshold)
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
  \item{threshold}{
soglia rispetto alla quale si contano i superamenti
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
vettore di lunghezza unique(period) contenente i conteggi di superamenti
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

exc.period(values, month, necess=0.9, threshold=0.5)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
