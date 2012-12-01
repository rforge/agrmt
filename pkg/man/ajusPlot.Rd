\name{ajusPlot}
\alias{ajusPlot}
\title{Plot vector with AJUS type}
\description{Plot a frequency vector aong with its AJUS type.}
\usage{ajusPlot(V, tolerance=0.1)}
\arguments{
  \item{V}{A frequency vector}
  \item{tolerance}{Specify how similar values have to be to be treated as different (optional). Differences smaller than or equal to the tolerance are ignored.}
}
\details{This helper function plots the frequency vector along with its AJUS classification and skew. Although this function works, it should be considered experimental.}
\author{Didier Ruedin}
\seealso{\code{\link{ajus}}}
