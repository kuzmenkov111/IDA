\name{symunc}
\alias{symunc}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.5, Example 2.5.7.}

\arguments{See Section 2.5, Example 2.5.7.}

\details{See Section 2.5, Example 2.5.7.}

\value{See Section 2.5, Example 2.5.7.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{mutinfo}}
\code{\link{entropy.cond}}
}

\examples{
data(weather, package="dmr.data")

symunc(weather$outlook, weather$temperature)
symunc(weather$outlook, weather$play)
}

\keyword{multivariate}
