\name{wrapper.select}
\alias{wrapper.select}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 19 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 19.5, Example 19.5.2.}

\arguments{See Section 19.5, Example 19.5.2.}

\details{See Section 19.5, Example 19.5.2.}

\value{See Section 19.5, Example 19.5.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{wrapper.eval}}
\code{\link{wrapper.filter.select}}
\code{\link{asel.search.greedy}}
}

\examples{
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

  # wrapper selection for the weather data
wrapper.select(play~., weather, rpart, args=list(minsplit=2),
               predf=function(...) predict(..., type="c"))
wrapper.select(play~., weather, rpart, args=list(minsplit=2),
               predf=function(...) predict(..., type="c"),
               initf=asel.init.none, nextf=asel.next.forward)

  # wrapper selection for the weatherc data
wrapper.select(play~., weatherc, rpart, args=list(minsplit=2),
               predf=function(...) predict(..., type="c"))
wrapper.select(play~., weatherc, rpart, args=list(minsplit=2),
               predf=function(...) predict(..., type="c"),
               initf=asel.init.none, nextf=asel.next.forward)

  # wrapper selection for the weatherr data
wrapper.select(playability~., weatherr, rpart, args=list(minsplit=2))
wrapper.select(playability~., weatherr, rpart, args=list(minsplit=2),
               initf=asel.init.none, nextf=asel.next.forward)

  # wrapper selection for the Vehicle Silhouettes data
v.sel.fwd <- wrapper.select(Class~., v.train, rpart,
                            predf=function(...) predict(..., type="c"),
                            initf=asel.init.none, nextf=asel.next.forward)
v.sel.bwd <- wrapper.select(Class~., v.train, rpart,
                            predf=function(...) predict(..., type="c"))

  # wrapper selection for the Soybean data
s.sel.fwd <- wrapper.select(Class~., s.train, rpart,
                            predf=function(...) predict(..., type="c"),
                            initf=asel.init.none, nextf=asel.next.forward)
s.sel.bwd <- wrapper.select(Class~., s.train, rpart,
                            predf=function(...) predict(..., type="c"))

  # wrapper selection for the Boston Housing data
bh.sel.fwd <- wrapper.select(medv~., bh.train, rpart,
                             initf=asel.init.none, nextf=asel.next.forward)
bh.sel.bwd <- wrapper.select(medv~., bh.train, rpart)
}

\keyword{models}
