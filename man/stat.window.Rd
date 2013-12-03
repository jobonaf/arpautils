\name{stat.window}
\alias{stat.window}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
statistiche su finestra mobile
}
\description{
calcola una statistica (media, mediana, dev.standard, ...) su una finestra mobile
}
\usage{
stat.window(x, window, necess, FUN = mean)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vettore numerico su cui si vuole calcolare la statistica
}
  \item{window}{
vettore di due elementi che identifica il range della finestra mobile
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
vettore della stessa lunghezza di 'x' contenente le statistiche
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

plot(values)
w1 <- stat.window(values, window=c(-10,10), necess=0.9, FUN=mean)
w2 <- stat.window(values, window=c(-20,0), necess=0.9, FUN=mean)
w3 <- stat.window(values, window=c(-50,50), necess=0.9, FUN=mean)
points(w1, col="red")
points(w2, col="green")
points(w3, col="blue")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
