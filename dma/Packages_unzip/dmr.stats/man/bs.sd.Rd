\name{bs.sd}
\alias{bs.sd}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.12.}

\arguments{See Section 2.4, Example 2.4.12.}

\details{See Section 2.4, Example 2.4.12.}

\value{See Section 2.4, Example 2.4.12.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{bs.var}}
\code{\link{bs.mean}}
\code{\link{sd}}
}

\examples{
data(weatherr, package="dmr.data")

bs.sd(weatherr$playability)
sd(weatherr$playability)
}

\keyword{univar}
