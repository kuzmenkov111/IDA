\name{gower.coef}
\alias{gower.coef}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 11 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 11.3, Example 11.3.7.}

\arguments{See Section 11.3, Example 11.3.7.}

\details{See Section 11.3, Example 11.3.7.}

\value{See Section 11.3, Example 11.3.7.}

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
\code{\link{ham.dist}}
\code{\link{dissmat}}
}

\examples{
library(dmr.util)
data(weathercl, package="dmr.data")

  # Gower's coefficient dissimilarity matrix for the weathercl data
dissmat(weathercl, function (x1, x2) gower.coef(x1, x2, ranges(weathercl)))

}

\keyword{arith}
