\name{second.modes}
\alias{second.modes}
\title{Most common and second most common values}
\description{Identifies the most common (multiple) modes for frequency vectors as well as the second most common values.}
\usage{second.modes(V, tolerance=0)}
\arguments{
  \item{V}{A frequency vector}
  \item{tolerance}{Specify how similar values have to be to be treated as different.}
}
\details{This function identifies which positions of a frequency vector correspond to the mode as implemented in the \code{\link{modes}} function. It also reports the second most common position in the same manner.}
\value{The function returns a list for the most common and the second most common value. In each case, the \code{mode} returns the value of the mode (number). \code{positions} returns the positions of the vector that correspond to the mode. \code{contiguous} returns TRUE of all modes are contiguous, and FALSE if there are different values in between.}
\author{Didier Ruedin}
\seealso{\code{\link{modes}}}
