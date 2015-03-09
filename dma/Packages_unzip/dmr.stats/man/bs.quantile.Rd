\name{bs.quantile}
\alias{bs.quantile}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.4, Example 2.4.7.}

\arguments{See Section 2.4, Example 2.4.7.}

\details{See Section 2.4, Example 2.4.7.}

\value{See Section 2.4, Example 2.4.7.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{quantile}}
\code{\link{bs.median}}
}

\examples{
data(weatherc, package="dmr.data")

bs.quantile(weatherc$temperature)
quantile(weatherc$temperature)
bs.quantile(weatherc$temperature[weatherc$play=="yes"])
quantile(weatherc$temperature[weatherc$play=="yes"])
}

\keyword{univar}
