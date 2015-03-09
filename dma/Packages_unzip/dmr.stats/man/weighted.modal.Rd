\name{weighted.modal}
\alias{weighted.modal}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.20.}

\arguments{See Section 2.4, Example 2.4.20.}

\details{See Section 2.4, Example 2.4.20.}

\value{See Section 2.4, Example 2.4.20.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{modal}}
\code{\link{bs.weighted.mean}}
\code{\link{weighted.median}}
\code{\link{weighted.var}}
}

\examples{
data(weather, package="dmr.data")

weighted.modal(weather$outlook)
weighted.modal(weather$outlook, w=ifelse(weather$play=="yes", 2, 1))
}

\keyword{univar}
