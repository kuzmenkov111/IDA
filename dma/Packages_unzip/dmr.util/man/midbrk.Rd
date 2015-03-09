\name{midbrk}
\alias{midbrk}

\title{Find middle break points between consecutive values in a vector}

\description{This function finds break points in the middle between consecutive values contained in a numeric vector. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
midbrk(v)
}

\arguments{
  \item{v}{a numeric vector}
}

\details{
Breaks are identified as the middle points between every two consecutive
elements of \code{v} after sorting.
}

\value{A vector of middle break points for \code{v}.
}

\references{
}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
}

\examples{
midbrk(1:10)
midbrk(2*(1:10))

data(weatherr, package="dmr.data")
midbrk(sort(unique(weatherr$temperature)))
}

\keyword{arith}
