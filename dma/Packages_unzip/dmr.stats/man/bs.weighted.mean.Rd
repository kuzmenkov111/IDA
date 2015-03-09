\name{bs.weighted.mean}
\alias{bs.weighted.mean}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.2.}

\arguments{See Section 2.4, Example 2.4.2.}

\details{See Section 2.4, Example 2.4.2.}

\value{See Section 2.4, Example 2.4.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{bs.mean}}
\code{\link{weighted.var}}
\code{\link{weighted.median}}
\code{\link{weighted.modal}}
\code{\link{weighted.mean}}
}

\examples{
data(weatherc, package="dmr.data")

bs.weighted.mean(weatherc$temperature, ifelse(weatherc$play=="yes", 5, 1))
weighted.mean(weatherc$temperature, ifelse(weatherc$play=="yes", 5, 1))
}

\keyword{univar}
