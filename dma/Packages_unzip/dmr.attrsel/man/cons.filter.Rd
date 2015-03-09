\name{cons.filter}
\alias{cons.filter}
\alias{cons.eval}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 19 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 19.4, Example 19.4.4.}

\arguments{See Section 19.4, Example 19.4.4.}

\details{See Section 19.4, Example 19.4.4.}

\value{See Section 19.4, Example 19.4.4.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{simple.filter}}
\code{\link{cfs.filter}}
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

  # consistency-based filter for the weather data
cons.filter(play~., weather)
cons.filter(play~., weather, initf=asel.init.none, nextf=asel.next.forward)

  # consistency-based filter for the weatherc data
cons.filter(play~., discnm.eqfreq(~., weatherc, 4))
cons.filter(play~., discnm.eqfreq(~., weatherc, 4),
            initf=asel.init.none, nextf=asel.next.forward)
cons.filter(play~., weatherc)
cons.filter(play~., weatherc, initf=asel.init.none, nextf=asel.next.forward)

  # correlation-based filter for the weatherr data
cons.filter(playability~., discnm.eqfreq(~., weatherr, 4))
cons.filter(playability~., discnm.eqfreq(~., weatherr, 4),
            initf=asel.init.none, nextf=asel.next.forward)

  # consistency-based for the Vehicle Silhouettes data
v.sel.cons <- cons.filter(Class~., discnm.eqfreq(~., v.train, 7))$subset
  # consistency-based for the Soybean data
s.sel.cons <- cons.filter(Class~., s.train)$subset
  # consistency-based for the Boston Housing data
bh.sel.cons <- cons.filter(medv~., discnm.eqfreq(~., bh.train, 7))$subset
}

\keyword{models}
