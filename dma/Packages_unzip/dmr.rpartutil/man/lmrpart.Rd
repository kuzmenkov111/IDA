\name{lmrpart}
\alias{lmrpart}
\alias{predict.lmrpart}

\title{Utility functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Utility functions from Chapter 20 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section Section 20.1.}

\arguments{See Section 20.1.}

\details{See Section 20.1.}

\value{See Section 20.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{rpart}}
\code{\link{lm}}
}

\examples{
library(dmr.regeval)

data(BostonHousing, package="mlbench")
BostonHousing$chas <- NULL

set.seed(12)
rbh <- runif(nrow(BostonHousing))
bh.train <- BostonHousing[rbh>=0.33,]
bh.test <- BostonHousing[rbh<0.33,]

bh.mtree <- lmrpart(medv~., bh.train, cp=0.05)

  # generate and evaluate test set predictions
mse(predict(bh.mtree, bh.test), bh.test$medv)
  # compare to the internal regression tree
mse(predict(bh.mtree, bh.test), bh.test$medv)
  # and a single linear model
mse(predict(lm(medv~., bh.train), bh.test), bh.test$medv)
}

\keyword{models}
