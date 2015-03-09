\name{bs.median}
\alias{bs.median}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.3.}

\arguments{See Section 2.4, Example 2.4.3.}

\details{See Section 2.4, Example 2.4.3.}

\value{See Section 2.4, Example 2.4.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{bs.mean}}
\code{\link{median}}
}

\examples{
data(weatherc, package="dmr.data")

bs.median(weatherc$temperature)
median(weatherc$temperature)
bs.median(weatherc$temperature[weatherc$play=="yes"])
median(weatherc$temperature[weatherc$play=="yes"])
}

\keyword{univar}
