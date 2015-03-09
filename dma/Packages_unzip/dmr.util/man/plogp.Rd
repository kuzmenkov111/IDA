\name{plogp}
\alias{plogp}

\title{Calculate the product of a probability and its base-2 logarithm returning 0 for 0 probabilities}

\description{This function calculates the product of a probability value and its a base-2 logarithm, returning 0 for 0 probabilities. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
plogp(p)
}

\arguments{
  \item{p}{a non-negative value or vector of values}
}

\details{This avoids the \code{NaN} result produced by \code{0*log2(0)}
by forcing the result to be 0 for zero probabilities.
}

\value{If \code{p>0} then \code{p*log2(p)} and \code{0} otherwise.
}

\references{
}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
\code{\link{logp}}
}

\examples{
plogp(0)
plogp(1)
plogp(0.5)
}

\keyword{arith}
