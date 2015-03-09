\name{mutinfo}
\alias{mutinfo}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 2 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 2.5, Example 2.5.6.}

\arguments{See Section 2.5, Example 2.5.6.}

\details{See Section 2.5, Example 2.5.6.}

\value{See Section 2.5, Example 2.5.6.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{entropy}}
\code{\link{entropy.cond}}
\code{\link{symunc}}
\code{\link{g.test}}
}

\examples{
data(weather, package="dmr.data")

mutinfo(weather$outlook, weather$play)
  # this should be the same
entropy(weather$play)-entropy.cond(weather$play, weather$outlook)
entropy(weather$outlook)-entropy.cond(weather$outlook, weather$play)
g.test(weather$outlook, weather$play)$statistic/(2*log(2)*nrow(weather))
}

\keyword{multivariate}
