\name{modes}
\alias{modes}
\title{Identify multiple modes}
\description{Identifies (multiple) modes for frequency vectors.}
\usage{modes(V, tolerance=0)}
\arguments{
  \item{V}{A frequency vector}
  \item{tolerance}{Specify how similar values have to be to be treated as different.}
}
\details{This function identifies which positions of a frequency vector correspond to the mode. If there are multiple modes of the same value, only the first one will be reported.}
\value{The function returns a list. The \code{vector} returns the frequency vector. The \code{mode} returns the value of the mode (number). \code{positions} returns the positions of the vector that correspond to the mode. \code{contiguous} returns TRUE of all modes are contiguous, and FALSE if there are different values in between.}
\author{Didier Ruedin}
\seealso{\code{\link{second.modes}}}
