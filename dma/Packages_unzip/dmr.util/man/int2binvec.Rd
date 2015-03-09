\name{int2binvec}
\alias{int2binvec}

\title{Convert an integer to a binary vector representation}

\description{This function converts an integer to the corresponding binary vector using the natural binary code. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
int2binvec(v, max = 255)
}

\arguments{
  \item{v}{an integer value}
  \item{max}{a maximum integer value to be represented}
}

\details{The input value is converted to a binary code word, ordered
from the least significant bits to the most significant bits, of
length sufficient to represent the maximum value specified by \code{max}.
}

\value{An integer vector of 0's and 1's, representing \code{v} in
natural binary code.}

\references{
}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
\code{\link{binvec2int}}
}

\examples{
int2binvec(11, max=15)
int2binvec(11, max=63)
}

\keyword{arith}
