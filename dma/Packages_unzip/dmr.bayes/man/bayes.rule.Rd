\name{bayes.rule}
\alias{bayes.rule}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 4 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 4.2, Example 4.2.1.}

\arguments{See Section 4.2, Example 4.2.1.}

\details{See Section 4.2, Example 4.2.1.}

\value{See Section 4.2, Example 4.2.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
}

\examples{
  # posterior probabilities
bayes.rule(c(0.2, 0.3, 0.5), c(0.9, 0.9, 0.5))

  # let P(burglery)=0.001,
  # P(alarm|burglery)=0.95,
  # P(alarm|not burglery)=0.005
  # calculate P(burglery|alarm)
bayes.rule(c(0.001, 0.999), c(0.95, 0.005))[1]
}

\keyword{arith}
