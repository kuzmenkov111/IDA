\name{closest.below}
\alias{closest.below}

\title{Find the closest value below a given threshold in a numeric vector}

\description{This function finds in an input numeric vector the closest (greatest) value below a specified threshold. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
closest.below(v1, v2)
}

\arguments{
  \item{v1}{a numeric vector of threshold values}
  \item{v2}{a numeric vector of input value}
}

\details{For each threshold value in \code{v1}, the greatest less input value
from \code{v2} is found and returned.
}

\value{A numeric vector of the same length as \code{v1}, containing for
each element of \code{v1} the greatest less value from \code{v2} or
\code{-Inf} if no such value exists.}

\references{
}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
\code{\link{closest.above}}
}

\examples{
closest.below(1:5, 1:10)
closest.below(6:10, 1:10)
closest.below(2*(1:4), 1:10)
}

\keyword{arith}
