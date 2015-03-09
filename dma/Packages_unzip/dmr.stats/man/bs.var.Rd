\name{bs.var}
\alias{bs.var}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.8.}

\arguments{See Section 2.4, Example 2.4.8.}

\details{See Section 2.4, Example 2.4.8.}

\value{See Section 2.4, Example 2.4.8.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}

\seealso{
\code{\link{bs.sd}}
\code{\link{bs.mean}}
\code{\link{var1}}
\code{\link{var}}
}

\examples{
data(weatherr, package="dmr.data")

bs.var(weatherr$playability)
var(weatherr$playability)
}

\keyword{univar}
