\name{xts.blend}
\alias{xts.blend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
fonde tra loro due oggetti \code{xts}
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
fonde tra loro due oggetti \code{xts}
}
\usage{
xts.blend(tstep, TZ = "BST", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tstep}{
stringa di carattere che identifica il timestep ("H" o "d", orario o giornaliero)
}
  \item{TZ}{
%%     ~~Describe \code{TZ} here~~
timezone
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (step, TZ = "BST", ...) 
{
    if (!(step \%in\% c("d", "H"))) 
        stop(paste("Invalid timestep value, only `d` or `H` allowed"))
    Xm <- merge.xts(..., tzone = TZ)
    idx.time <- index(Xm)
    f.time <- idx.time[1]
    l.time <- idx.time[length(idx.time)]
    timestring <- paste(format(f.time, "\%Y-\%m-\%d \%H:\%M"), format(l.time, 
        "\%Y-\%m-\%d \%H:\%M"), step, sep = "/")
    s0 <- timeBasedSeq(timestring)
    x0 <- xts(rep(NA, length(s0)), s0, tzone = TZ)
    Xm <- merge.xts(x0, Xm, join = "left", tzone = TZ)
    dm <- apply(Xm, MARGIN = 1, FUN = max, na.rm = T)
    dm[is.infinite(dm)] <- NA
    dm[is.nan(dm)] <- NA
    xm <- xts(dm, s0)
    return(xm)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
