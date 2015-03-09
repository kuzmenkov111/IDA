\name{weighted.var}
\alias{weighted.var}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.10.}

\arguments{See Section 2.4, Example 2.4.10.}

\details{See Section 2.4, Example 2.4.10.}

\value{See Section 2.4, Example 2.4.10.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{bs.var}}
\code{\link{bs.weighted.mean}}
\code{\link{weighted.median}}
\code{\link{weighted.modal}}
\code{\link{var}}
}

\examples{
data(weatherr, package="dmr.data")

weighted.var(weatherr$playability)
weighted.var(weatherr$playability, ifelse(weatherr$outlook=="rainy", 2, 1))
}

\keyword{univar}
