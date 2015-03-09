\name{rf.filter}
\alias{rf.filter}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 19 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 19.4, Example 19.4.7.}

\arguments{See Section 19.4, Example 19.4.7.}

\details{See Section 19.4, Example 19.4.7.}

\value{See Section 19.4, Example 19.4.7.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{cutoff}}
\code{\link[randomForest]{randomForest}}
\code{\link{simple.filter}}
\code{\link{cfs.filter}}
\code{\link{cons.filter}}
\code{\link{relief.filter}}
\code{\link{rrelief.filter}}
}

\examples{
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

  # random forest filter for the weather data
rf.filter(play~., weather)
  # random forest filter for the weatherc data
rf.filter(play~., weatherc)
  # random forest filter for the weatherr data
rf.filter(playability~., weatherr)

  # random forest filter for the Vehicle Silhouettes data
v.utl.rf <- rf.filter(Class~., v.train)
  # random forest filter for the Soybean data
s.utl.rf <- rf.filter(Class~., s.train)
  # random forest filter for the BostonHousing data
bh.utl.rf <- rf.filter(medv~., bh.train)
}

\keyword{models}
