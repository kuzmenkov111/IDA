\name{l2norm}
\alias{l2norm}

\title{Calculate the L2 (Euclidean) norm of a vector}

\description{This function calculates the L2 (Euclidean) norm of a numeric vector. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
l2norm(v)
}

\arguments{
  \item{v}{a numeric vector}
}

\details{The L2 (Euclidean) norm is calculated as the square root of the
sum of squared vector elements.
}

\value{\code{sqrt(sum(v^2))}
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
l2norm(3:4)
l2norm(rep(3, 4))
}

\keyword{arith}
