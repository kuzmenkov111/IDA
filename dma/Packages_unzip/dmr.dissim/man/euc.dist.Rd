\name{euc.dist}
\alias{euc.dist}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 11 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 11.3, Example 11.3.1.}

\arguments{See Section 11.3, Example 11.3.1.}

\details{See Section 11.3, Example 11.3.1.}

\value{See Section 11.3, Example 11.3.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{mink.dist}}
\code{\link{cheb.dist}}
\code{\link{man.dist}}
\code{\link{can.dist}}
\code{\link{ham.dist}}
\code{\link{gower.coef}}
\code{\link{dissmat}}
}

\examples{
data(weathercl, package="dmr.data")

  # Euclidean distance dissimilarity matrix for the weathercl data
dissmat(weathercl, euc.dist)
}

\keyword{arith}
