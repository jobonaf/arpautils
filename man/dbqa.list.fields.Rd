\name{dbqa.list.fields}
\alias{dbqa.list.fields}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Elenca i campi presenti in una tabella del DB
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Elenca i campi presenti in una tabella del DB
}
\usage{
dbqa.list.fields(con, tab)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{con}{
%%     ~~Describe \code{con} here~~
connessione al DB
}
  \item{tab}{
%%     ~~Describe \code{tab} here~~
nome della tabella del DB
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
function (con, tab) 
{
    query <- paste("select * from ", tab, " where rownum=0")
    library("ROracle")
    data <- fetch(ds <- dbSendQuery(con, query))
    out <- colnames(data)
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line