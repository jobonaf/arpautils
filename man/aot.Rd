\name{aot}
\alias{aot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calcola AOT
}
\description{
Calcola una generica Accumulated exposure Over Threshold
}
\usage{
aot(x, hr, threshold = 80, estimate = T, hr.min = 8, hr.max = 19)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
vettore dei valori di concentrazione
}
  \item{hr}{
%%     ~~Describe \code{hr} here~~
vettore, della stessa lunghezza di \code{x}, che identifica l'orario
}
  \item{threshold}{
%%     ~~Describe \code{threshold} here~~
soglia
}
  \item{estimate}{
%%     ~~Describe \code{estimate} here~~
variabile logica, se \code{TRUE} viene eseguita la stima di AOT corretta per i dati mancanti, conformemente alla normativa
}
  \item{hr.min}{
%%     ~~Describe \code{hr.min} here~~
prima ora della fascia oraria da considerare
}
  \item{hr.max}{
%%     ~~Describe \code{hr.max} here~~
ultima ora della fascia oraria da considerare
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
G.Bonafè
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
