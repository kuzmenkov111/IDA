\name{imp}
\alias{imp}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 17 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 17.3, Example 17.3.4.}

\arguments{See Section 17.3, Example 17.3.4.}

\details{See Section 17.3, Example 17.3.4.}

\value{See Section 17.3, Example 17.3.4.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{imp.all}
}

\examples{
data(weatherc, package="dmr.data")

weathercm <- weatherc
weathercm$outlook[c(1, 3)] <- NA
weathercm$temperature[c(2, 4)] <- NA
weathercm$humidity[c(3, 5)] <- NA
weathercm$wind[c(4, 6)] <- NA

imp(weatherc$outlook)
imp(weatherc$temperature)
imp(weatherc$humidity)
imp(weatherc$wind)

}

\keyword{arith}
