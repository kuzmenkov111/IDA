\name{logp}
\alias{logp}

\title{Calculate a base-2 logarithm of a probability value, returning a finite value for 0}

\description{This function calculates a base-2 logarithm of a probability value, returning a finite value for probability 0. It is used in examples presented in the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.
}

\usage{
logp(p)
}

\arguments{
  \item{p}{a non-negative value or vector of values}
}

\details{To avoid infinite results and the associated numerical problems
for zero arguments, a finite result corresponding to the least possible positive
argument is used.
}

\value{If \code{p>0} then \code{log2(p)} and \code{.Machine$double.min.exp} otherwise.
}

\references{
}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
\code{\link{plogp}}
}

\examples{
logp(0)
logp(1)
logp(0.5)
}

\keyword{arith}
