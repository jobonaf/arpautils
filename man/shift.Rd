\name{shift}
\alias{shift}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
shifta un vettore
}
\description{
shifta un vettore 
}
\usage{
shift(x, k)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vettore da shiftare
}
  \item{k}{
passi di shift
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
vettore shiftato
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
values <- runif(n=10)
values
shift(values, 2)
shift(values, -3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
