\name{relief.filter}
\alias{relief.filter}
\alias{relief.diss}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 19 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 19.4, Example 19.4.5.}

\arguments{See Section 19.4, Example 19.4.5.}

\details{See Section 19.4, Example 19.4.5.}

\value{See Section 19.4, Example 19.4.5.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{cutoff}}
\code{\link{rrelief.filter}}
\code{\link{simple.filter}}
\code{\link{cfs.filter}}
\code{\link{cons.filter}}
\code{\link{rf.filter}}
}

\examples{
data(weather, package="dmr.data")
data(weatherc, package="dmr.data")
data(Vehicle, package="mlbench")
data(Soybean, package="mlbench")

set.seed(12)

rv <- runif(nrow(Vehicle))
v.train <- Vehicle[rv>=0.33,]
v.test <- Vehicle[rv<0.33,]

rs <- runif(nrow(Soybean))
s.train <- Soybean[rs>=0.33,]
s.test <- Soybean[rs<0.33,]

  # RELIEF for the weather data
relief.filter(play~., weather, K=100)
relief.filter(play~., weather, k=3, K=100)

  # RELIEF for the weatherc data
relief.filter(play~., weatherc, K=100)
relief.filter(play~., weatherc, k=3, K=100)

  # RELIEF for the Vehicle Silhouettes data
v.utl.rel <- relief.filter(Class~., v.train, k=3, K=200)
  # RELIEF for the Soybean data
s.utl.rel <- relief.filter(Class~., s.train, k=3, K=200)
}

\keyword{models}
