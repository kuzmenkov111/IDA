\name{cheb.dist}
\alias{cheb.dist}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 11 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 11.3, Example 11.3.5.}

\arguments{See Section 11.3, Example 11.3.5.}

\details{See Section 11.3, Example 11.3.5.}

\value{See Section 11.3, Example 11.3.5.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{euc.dist}}
\code{\link{mink.dist}}
\code{\link{man.dist}}
\code{\link{can.dist}}
\code{\link{ham.dist}}
\code{\link{gower.coef}}
\code{\link{dissmat}}
}

\examples{
data(weathercl, package="dmr.data")

  # Chebyshev distance dissimilarity matrix for the weathercl data
dissmat(weathercl, cheb.dist)
  # roughly the same as
dissmat(weathercl, function (x1, x2) mink.dist(x1, x2, 10))
}

\keyword{arith}
