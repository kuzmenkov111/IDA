\name{asel.search.filter}
\alias{asel.search.filter}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 19 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 19.4, Example 19.4.9.}

\arguments{See Section 19.4, Example 19.4.9.}

\details{See Section 19.4, Example 19.4.9.}

\value{See Section 19.4, Example 19.4.9.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{asel.search.greedy}}
\code{\link{simple.filter}}
\code{\link{relief.filter}}
\code{\link{rrelief.filter}}
\code{\link{rf.filter}}
}

\examples{
library(dmr.stats)
data(weather, package="dmr.data")
data(weatherc, package="dmr.data")
data(weatherr, package="dmr.data")
data(Vehicle, package="mlbench")
data(Soybean, package="mlbench")
data(BostonHousing, package="mlbench")

  # filter attribute selection search for the weather data
  # using mutual information-based utility estimates
asel.search.filter(names(weather)[-5], "play",
                   simple.filter(play~., weather, dd=symunc),
                   evalf=function(subset, target) 1)
asel.search.filter(names(weather)[-5], "play",
                   simple.filter(play~., weather, dd=symunc),
                   evalf=function(subset, target) 1, penalty=0)
asel.search.filter(names(weather)[-5], "play",
                   simple.filter(play~., weather, dd=symunc),
                   evalf=function(subset, target) 1)
asel.search.filter(names(weather)[-5], "play",
                   simple.filter(play~., weather, dd=symunc),
                   evalf=function(subset, target) 1, penalty=0)
}

\keyword{models}
