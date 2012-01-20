\name{AJUS}
\alias{AJUS}
\title{Classify distributions}
\description{Classify distributions using the AJUS-system introduced by Galtung (1969).}
\usage{AJUS(V, tolerance=0.1)}
\arguments{
  \item{V}{A frequency vector}
  \item{tolerance}{Specify how similar values have to be to be treated as different.}
}
\details{This function implements the AJUS-system introduced by Galtung (1969). The input is a frequency vector. Distributions are classified as A if they are unimodal with a peak in the centre, as J if they are unimodal with a peak at either end, as U if they are bimodal with a peak at both ends, and as S if they are multimodal. In addition to Galtung's classification, the Function classifies distributions as F if there is no peak and all values are more or less the same. The skew is givens as +1 for a positive skew, as 0 for no skew, and -1 for a negative skew.}
\value{The function returns a list. The \code{type} returns a string corresponding to the pattern described by Galtung (A,J,U,S) or F. The \code{skew} returns a number to describe the direction of the skew.}
\references{Galtung, J. (1969) Theory and Methods of Social Research. Oslo: Universitetsforlaget.}
\author{Didier Ruedin}
\seealso{\code{\link{ISD}}}
