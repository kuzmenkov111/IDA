\name{cutoff}
\alias{cutoff}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 19 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 19.4, Example 19.4.8.}

\arguments{See Section 19.4, Example 19.4.8.}

\details{See Section 19.4, Example 19.4.8.}

\value{See Section 19.4, Example 19.4.8.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{simple.filter}}
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

v.utl.simple <- simple.filter(Class~., discnm.eqfreq(~., v.train, 7), dd=symunc)
v.utl.rel <- relief.filter(Class~., v.train, k=3, K=200)
v.utl.rf <- rf.filter(Class~., v.train)

s.utl.simple <- simple.filter(Class~., Soybean, dd=symunc)
s.utl.rel <- relief.filter(Class~., s.train, k=3, K=200)
s.utl.rf <- rf.filter(Class~., s.train)

bh.utl.simple <- simple.filter(medv~., discnm.eqfreq(~., bh.train, 7), dd=symunc)
bh.utl.rel <- rrelief.filter(medv~., bh.train, k=3, K=100)
bh.utl.rf <- rf.filter(medv~., bh.train)

  # cutoff based on the random forest filter for the weather data
cutoff(rf.filter(play~., weather), k=3)
  # cutoff based on the random forest filter for the weatherc data
cutoff(rf.filter(play~., weatherc), p=0.5)
  # cutoff based on the random forest filter for the weatherr data
cutoff(rf.filter(playability~., weatherr))

  # cutoff for the Vehicle Silhouettes data
v.sel.simple <- cutoff(v.utl.simple, p=0.5)
v.sel.rf <- cutoff(v.utl.rf, p=0.5)
v.sel.rel <- cutoff(v.utl.rel, p=0.5)

  # cutoff for the Soybean data
s.sel.simple <- cutoff(s.utl.simple, p=0.5)
s.sel.rf <- cutoff(s.utl.rf, p=0.5)
s.sel.rel <- cutoff(s.utl.rel, p=0.5)

  # cutoff for the Boston Housing data
bh.sel.simple <- cutoff(bh.utl.simple, p=0.5)
bh.sel.rf <- cutoff(bh.utl.rf, p=0.5)
bh.sel.rel <- cutoff(bh.utl.rel, p=0.5)

}

\keyword{models}
