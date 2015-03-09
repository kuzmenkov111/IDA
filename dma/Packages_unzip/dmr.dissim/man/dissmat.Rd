\name{dissmat}
\alias{dissmat}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 11 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 11.2, Example 11.2.1.}

\arguments{See Section 11.2, Example 11.2.1.}

\details{See Section 11.2, Example 11.2.1.}

\value{See Section 11.2, Example 11.2.1.}

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
\code{\link{gower.coef}}
\code{\link{cos.sim}}
\code{\link{pearson.sim}}
\code{\link{spearman.sim}}
}

\examples{
data(weathercl, package="dmr.data")

  # dummy dissimilarity matrix for the weathercl data
dummy.diss <- function(x1, x2)
{ abs(as.integer(row.names(x1))-as.integer(row.names(x2))) }

dissmat(weathercl, dummy.diss)
}

\keyword{arith}
