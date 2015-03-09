\name{wmae}
\alias{wmae}
\alias{wmse}
\alias{wrmse}
\alias{wrae}
\alias{wr2}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 10 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 10.2, Example 10.2.8.}

\arguments{See Section 10.2, Example 10.2.8.}

\details{See Section 10.2, Example 10.2.8.}

\value{See Section 10.2, Example 10.2.8.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{mae}}
\code{\link{mse}}
\code{\link{rmse}}
\code{\link{rae}}
\code{\link{r2}}
\code{\link{mls}}
}

\examples{
library(rpart)
data(BostonHousing, package="mlbench")

set.seed(12)
rbh <- runif(nrow(BostonHousing))
bh.train <- BostonHousing[rbh>=0.33,]
bh.test <- BostonHousing[rbh<0.33,]
bh.tree <- rpart(medv~., bh.train)

  # double weight for medv>25
bh.wtest <- ifelse(bh.test$medv>25, 2, 1)

wmae(predict(bh.tree, bh.test), bh.test$medv, bh.wtest)
wmse(predict(bh.tree, bh.test), bh.test$medv, bh.wtest)
wrmse(predict(bh.tree, bh.test), bh.test$medv, bh.wtest)
wrae(predict(bh.tree, bh.test), bh.test$medv, bh.wtest)
wr2(predict(bh.tree, bh.test), bh.test$medv, bh.wtest)
}

\keyword{arith}
\keyword{models}
