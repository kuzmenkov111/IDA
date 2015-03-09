\name{weighted.impurity}
\alias{weighted.impurity}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 3 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 3.3, Example 3.3.5.}

\arguments{See Section 3.3, Example 3.3.5.}

\details{See Section 3.3, Example 3.3.5.}

\value{See Section 3.3, Example 3.3.5.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{grow.dectree}}
\code{\link{entropy.p}}
\code{\link{gini.p}}
}

\examples{
library(dmr.stats)
data(weather, package="dmr.data")

weighted.impurity(pdisc(weather$play[weather$outlook=="overcast"]),
                  sum(weather$outlook=="overcast"),
                  pdisc(weather$play[weather$outlook!="overcast"]),
                  sum(weather$outlook!="overcast"))

}

\keyword{models}
