\name{agreement}
\alias{agreement}
\alias{consensus}
\alias{polarization}
\title{Function to calculate van der Eijk's measure of agreement "A"}
\description{ This package calculates agreement in ordered rating scales. It implements van der Eijk's (2001) measure of agreement "A", which can be used to describe agreement, consensus, or polarization among respondents.}
\usage{
agreement(V, old = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{V}{A frequency vector.
%%     ~~Describe \code{V} here~~
}
  \item{old}{Optional argument if you wish to use the old algorithm for agreement "A", as outlined in endnote XXX of the van der Eijk article. There is normally no reason to set the \code{old} argument.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{ The function returns the measure of agreement "A"
}
\references{
van der Eijk's (2001)
}
\author{ Didier Ruedin
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
function (V, old = FALSE)
{
    if (length(V) < 3) {
        warning("Warning: length of vector < 3, agreement A is not defined.")
        return(NA)
    }
    if (min(V) < 0)
        stop("Error: negative values found in frequency vector.")
    AA <- 0
    k <- length(V)
    N <- sum(V)
    R <- V
    for (i in 1:k) {
        P <- pattern.vector(R)
        if (max(P) == 0)
            break
        A <- pattern.agreement(P, old)
        m <- min.nz(R)
        L <- P * m
        w <- sum(L)/N
        AA <- AA + w * A
        R <- R - L
    }
    return(AA)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
