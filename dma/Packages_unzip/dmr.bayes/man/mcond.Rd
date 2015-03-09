\name{mcond}
\alias{mcond}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 4 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 4.4, Example 4.4.2.}

\arguments{See Section 4.4, Example 4.4.2.}

\details{See Section 4.4, Example 4.4.2.}

\value{See Section 4.4, Example 4.4.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{vcond}}
}

\examples{
data(weatherc, package="dmr.data")

mcond(weatherc$temperature, weatherc$play)
mcond(weatherc$humidity, weatherc$play)
}

\keyword{multivariate}
