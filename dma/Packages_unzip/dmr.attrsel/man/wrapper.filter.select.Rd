\name{wrapper.filter.select}
\alias{wrapper.filter.select}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 19 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 19.5, Example 19.5.3.}

\arguments{See Section 19.5, Example 19.5.3.}

\details{See Section 19.5, Example 19.5.3.}

\value{See Section 19.5, Example 19.5.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{wrapper.select}}
\code{\link{wrapper.eval}}
\code{\link{asel.search.filter}}
}

\examples{
library(dmr.stats)
library(rpart)
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

  # simple filter-driven wrapper selection for the weather data
wrapper.filter.select(play~., weather, simple.filter(play~., weather),
                      rpart, args=list(minsplit=2),
                      predf=function(...) predict(..., type="c"))

  # simple filter-driven wrapper selection for the weatherc data
wrapper.filter.select(play~., weatherc, simple.filter(play~., weatherc),
                      rpart, args=list(minsplit=2),
                      predf=function(...) predict(..., type="c"))

  # simple filter-driven wrapper selection for the weatherr data
wrapper.filter.select(playability~., weatherr,
                      simple.filter(playability~., weatherr),
                      rpart, args=list(minsplit=2))

  # RF filter-driven wrapper selection for the Vehicle Silhouettes data
v.sel.flt <- wrapper.filter.select(Class~., Vehicle, rf.filter(Class~., v.train),
                                   rpart, predf=function(...) predict(..., type="c"))

  # RF filter-driven wrapper selection for the Soybean data
s.sel.flt <- wrapper.filter.select(Class~., Soybean, rf.filter(Class~., s.train),
                                   rpart, predf=function(...) predict(..., type="c"))

  # RF filter-driven wrapper selection for the Boston Housing data
bh.sel.flt <- wrapper.filter.select(medv~., BostonHousing,
                                    rf.filter(medv~., bh.train), rpart)
}

\keyword{models}
