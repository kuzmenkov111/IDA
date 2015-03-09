\name{binvec2int}
\alias{binvec2int}

\title{Convert a binary vector to an integer representation}

\description{This function converts a binary vector to the corresponding integer number using the natural binary code. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
binvec2int(v)
}

\arguments{
  \item{v}{an integer vector of 0's and 1's}
}

\details{The input argument is considered a binary code word, ordered
from the least significant bits to the most significant bits, and
converted to the corresponding integer number according to the natural
binary code.
}

\value{An integer represented the input binary vector according to the
  natural binary code.
}

\references{
}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
\code{\link{int2binvec}}
}

\examples{
binvec2int(c(1, 1, 0, 1))
binvec2int(c(1, 1, 0, 1, 0))
binvec2int(c(1, 1, 0, 1, 1))
}

\keyword{arith}
