\name{rrelief.filter}
\alias{rrelief.filter}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 19 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 19.4, Example 19.4.6.}

\arguments{See Section 19.4, Example 19.4.6.}

\details{See Section 19.4, Example 19.4.6.}

\value{See Section 19.4, Example 19.4.6.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{cutoff}}
\code{\link{relief.filter}}
\code{\link{simple.filter}}
\code{\link{cfs.filter}}
\code{\link{cons.filter}}
\code{\link{rf.filter}}
}

\examples{
data(weatherr, package="dmr.data")
data(BostonHousing, package="mlbench")
data(BostonHousing, package="mlbench")

set.seed(12)

rbh <- runif(nrow(BostonHousing))
bh.train <- BostonHousing[rbh>=0.33,]
bh.test <- BostonHousing[rbh<0.33,]

  # RELIEF for the weatherr data
rrelief.filter(playability~., weatherr, K=100)
rrelief.filter(playability~., weatherr, k=3, K=100)

  # RELIEF for the Boston Housing data
bh.utl.rel <- rrelief.filter(medv~., bh.train, k=3, K=200)

}

\keyword{models}
