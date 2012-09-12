\name{modes}
\alias{modes}
\title{Identify multiple modes}
\description{Identifies (multiple) modes in a frequency vector.}
\usage{modes(V, pos=FALSE, tolerance=0.1)}
\arguments{
  \item{V}{A frequency vector}
  \item{pos}{Categories of frequency vector (optional)}
  \item{tolerance}{Specify how similar values have to be to be treated as different (optional)}
}
\details{This function identifies which positions of a frequency vector correspond to the mode. If there are multiple modes of the same value, all matching positions will be reported. Use the function \code{\link{collapse}} to create frequency vectors if necessary.}
\value{The function returns a list. The \code{at} returns the categories of the frequency vector. Either these categories were specified using the argument pos, or we assume it to be 1:k (with k the number of categories in the frequency vector). The \code{frequencies} returns the frequency vector. The \code{mode} returns the value of the mode(s). If there are multiple modes, they are listed. Similar frequencies are counted as equal, using the tolerance argument. To prevent similar frequencies to be considered the same, set \code{tolerance} to 0. \code{positions} returns the positions of the vector that correspond to the mode. This will differ from the \code{mode} if \code{pos} is provided. \code{contiguous} returns TRUE of all modes are contiguous, and FALSE if there are different values in between.}
\author{Didier Ruedin}
\seealso{\code{\link{second.modes}}}
