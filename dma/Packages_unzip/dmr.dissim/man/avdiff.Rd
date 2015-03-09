\name{avdiff}
\alias{avdiff}

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
\code{\link{ravdiff}}
\code{\link{euc.dist}}
\code{\link{mink.dist}}
}

\examples{
data(weathercl, package="dmr.data")

avdiff(weathercl[1,], weathercl[2,])
}

\keyword{arith}
