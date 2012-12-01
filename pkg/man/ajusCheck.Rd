\name{ajusCheck}
\alias{ajusCheck}
\title{Sensitivity test for AJUS}
\description{Check sensitivity of AJUS to different tolerance parameters.}
\usage{ajusCheck(V, t=seq(from=0.05, to=0.2, by=0.05))}
\arguments{
  \item{V}{A frequency vector}
  \item{t}{A vector of tolerance parameters to check. Differences smaller than or equal to the tolerance are ignored.}
}
\details{This function runs the AJUS system with a range of tolerance parameters. You can easily check how sensitive the classification of the distribution is to the tolerance parameter.}
\value{The function returns a list. The \code{tolerance} returns the tolerance parameters tested. The \code{type} returns a series of strings corresponding to the pattern described by Galtung (A,J,U,S) or F for each tolerance parameter. The \code{skew} returns a number to describe the direction of the skew.}
\author{Didier Ruedin}
\seealso{\code{\link{ajus}}}
