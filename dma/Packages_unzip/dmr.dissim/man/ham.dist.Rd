\name{ham.dist}
\alias{ham.dist}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 11 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 11.3, Example 11.3.6.}

\arguments{See Section 11.3, Example 11.3.6.}

\details{See Section 11.3, Example 11.3.6.}

\value{See Section 11.3, Example 11.3.6.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{euc.dist}}
\code{\link{mink.dist}}
\code{\link{cheb.dist}}
\code{\link{man.dist}}
\code{\link{can.dist}}
\code{\link{gower.coef}}
\code{\link{dissmat}}
}

\examples{
data(weathercl, package="dmr.data")

  # Hamming distance dissimilarity matrix for the weathercl
dissmat(weathercl, ham.dist)
}

\keyword{arith}
