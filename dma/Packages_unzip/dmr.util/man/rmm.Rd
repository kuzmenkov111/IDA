\name{rmm}
\alias{rmm}

\title{Multiply the rows of a matrix by the corresponding elements of a vector}

\description{This function performs row-wise matrix by vector multiplication, multiplying the rows of a matrix by the corresponding elements of a vector. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
rmm(m, v)
}

\arguments{
  \item{m}{a numeric matrix}
  \item{v}{a numeric vector}
}

\details{
For each row of \code{m} the element-wise multiplication by \code{v}
is performed, assuming \code{length(v)==ncol(m)}.
}

\value{A matrix of the same dimensions as \code{m}, with each row
obtained by multiplying the corresponding row of \code{m} by \code{v}.
}

\references{
}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
\code{\link{cmm}}
}

\examples{
rmm(matrix(1:9, nrow=3), 10^(0:2))
rmm(matrix(1:12, nrow=4), 10^(0:3))
}

\keyword{array}
