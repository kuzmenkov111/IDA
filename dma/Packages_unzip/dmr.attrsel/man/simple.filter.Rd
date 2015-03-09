\name{simple.filter}
\alias{simple.filter}
\alias{dd.chi2}
\alias{cd.kruskal}
\alias{cc.spearman}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 19 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 19.4, Example 19.4.1.}

\arguments{See Section 19.4, Example 19.4.1.}

\details{See Section 19.4, Example 19.4.1.}

\value{See Section 19.4, Example 19.4.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{cutoff}}
\code{\link{cfs.filter}}
\code{\link{cons.filter}}
\code{\link{relief.filter}}
\code{\link{rrelief.filter}}
\code{\link{rf.filter}}
}

\examples{
library(dmr.stats)
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

  # simple filter for the weather data
simple.filter(play~., weather)
simple.filter(play~., weather, dd=symunc)

  # simple filter for the weatherc data
simple.filter(play~., weatherc)
simple.filter(play~outlook+wind, weatherc, dd=symunc)
simple.filter(play~temperature+humidity, weatherc,
              cd=function(a1, a2) kruskal.test(a1, a2)$statistic)

  # simple filter for the weatherr data
simple.filter(playability~., weatherr)
simple.filter(playability~outlook+wind, weatherr,
              cd=function(a1, a2) kruskal.test(a1, a2)$statistic)
simple.filter(playability~temperature+humidity, weatherr,
              cc=function(a1, a2) cor(a1, a2, method="spearman")^2)

  # simple filter for the Vehicle Silhouettes data
v.utl.simple <- simple.filter(Class~., discnm.eqfreq(~., v.train, 7), dd=symunc)
  # simple filter for the Soybean data
s.utl.simple <- simple.filter(Class~., Soybean, dd=symunc)
  # simple filter for the BostonHousing data
bh.utl.simple <- simple.filter(medv~., discnm.eqfreq(~., bh.train, 7), dd=symunc)
}

\keyword{models}
