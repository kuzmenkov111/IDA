\name{cmm}
\alias{cmm}

\title{Multiply the columns of a matrix by the corresponding elements of a vector}

\description{This function performs column-wise matrix by vector multiplication, multiplying the columns of a matrix by the corresponding elements of a vector. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
cmm(m, v)
}

\arguments{
  \item{m}{a numeric matrix}
  \item{v}{a numeric vector}
}

\details{
For each column of \code{m} the element-wise multiplication by \code{v}
is performed, assuming \code{length(v)==nrow(m)}.
}

\value{A matrix of the same dimensions as \code{m}, with each column
obtained by multiplying the corresponding column of \code{m} by \code{v}.
}

\references{
}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
\code{\link{rmm}}
}

\examples{
cmm(matrix(1:9, ncol=3), 10^(0:2))
cmm(matrix(1:12, ncol=4), 10^(0:3))
}

\keyword{array}
