\name{cfs.filter}
\alias{cfs.filter}
\alias{cfs.eval}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 19 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 19.4, Example 19.4.3.}

\arguments{See Section 19.4, Example 19.4.3.}

\details{See Section 19.4, Example 19.4.3.}

\value{See Section 19.4, Example 19.4.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{discor}}
\code{\link{simple.filter}}
\code{\link{cons.filter}}
\code{\link{relief.filter}}
\code{\link{rrelief.filter}}
\code{\link{rf.filter}}
}

\examples{
library(dmr.disc)
data(weather, package="dmr.data")
data(weatherc, package="dmr.data")
data(weatherr, package="dmr.data")
data(Vehicle, package="mlbench")
data(Soybean, package="mlbench")
data(BostonHousing, package="mlbench")

set.seed(12)

rv <- runif(nrow(Vehicle))
v.train <- Vehicle[rv>=0.33,]
v.test <- Vehicle[rv<0.33,]

rs <- runif(nrow(Soybean))
s.train <- Soybean[rs>=0.33,]
s.test <- Soybean[rs<0.33,]

rbh <- runif(nrow(BostonHousing))
bh.train <- BostonHousing[rbh>=0.33,]
bh.test <- BostonHousing[rbh<0.33,]

  # correlation-based filter for the weather data
cfs.filter(play~., weather)
cfs.filter(play~., weather, initf=asel.init.none, nextf=asel.next.forward)
cfs.filter(play~., weather, corf=discor)
cfs.filter(play~., weather, corf=discor,
           initf=asel.init.none, nextf=asel.next.forward)

  # correlation-based filter for the weatherc data
cfs.filter(play~., discnm.eqfreq(~., weatherc, 4))
cfs.filter(play~., weatherc, corf=discor)
cfs.filter(play~., weatherc, corf=discor,
           initf=asel.init.none, nextf=asel.next.forward)

  # correlation-based filter for the weatherr data
cfs.filter(playability~., weatherr, corf=discor)
cfs.filter(playability~., weatherr, corf=discor,
           initf=asel.init.none, nextf=asel.next.forward)

  # correlation-based filter for the Vehicle Silhouettes data
v.sel.cfs <- cfs.filter(Class~., discnm.eqfreq(~., v.train, 7))$subset
  # correlation-based filter for the Soybean data
s.sel.cfs <- cfs.filter(Class~., s.train)$subset
  # correlation-based filter for the Boston Housing data
bh.sel.cfs <- cfs.filter(medv~., discnm.eqfreq(~., bh.train, 7))$subset
}

\keyword{models}
