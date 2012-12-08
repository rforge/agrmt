\name{ajus}
\alias{ajus}
\title{Classify distributions}
\description{Classify distributions using the AJUS-system introduced by Galtung (1969).}
\usage{ajus(V, tolerance=0.1)}
\arguments{
  \item{V}{A frequency vector}
  \item{tolerance}{Specify how similar values have to be to be treated as different (optional). Differences smaller than or equal to the tolerance are ignored.}
}
\details{This function implements the AJUS-system introduced by Galtung (1969). The input is a frequency vector. Distributions are classified as A if they are unimodal with a peak in the centre, as J if they are unimodal with a peak at either end, as U if they are bimodal with a peak at both ends, and as S if they are multimodal. In addition to Galtung's classification, the Function classifies distributions as F if there is no peak and all values are more or less the same. The skew is given as +1 for a positive skew, as 0 for no skew, and -1 for a negative skew.

The skew is identified by comparing the sum of values left and right of the midpoint respectively. For J-type of distributions, the skew is identified on the basis of the changes between values. This way long tails cannot influence the skew, and a single peak at the left and right-hand end can be differentiated in all cases.

The aim of the AJUS system is to reduce complexity. Initially the intuition was to classify distributions on an ad-hoc basis (i.e. eye-balling). Using an algorithm is certainly more reliable, and useful if one is interested in classifying (and comparing) a large number of distributions. The argument \code{tolerance}, however is not a trivial choice and can affect results. Use the helper function \code{ajusCheck} to check sensitivity to different values of the tolerance parameter.}
\value{The function returns a list. The \code{type} returns a string corresponding to the pattern described by Galtung (A,J,U,S) or F. The \code{skew} returns a number to describe the direction of the skew.}
\references{Galtung, J. (1969) Theory and Methods of Social Research. Oslo: Universitetsforlaget.}
\author{Didier Ruedin}
\seealso{\code{\link{isd}}, \code{\link{ajusCheck}}, \code{\link{ajusPlot}}}
