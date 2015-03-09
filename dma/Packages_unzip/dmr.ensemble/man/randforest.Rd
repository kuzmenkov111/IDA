\name{randforest}
\alias{randforest}
\alias{predict.randforest}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 15 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 15.5, Example 15.5.5.}

\arguments{See Section 15.5, Example 15.5.5.}

\details{See Section 15.5, Example 15.5.5.}

\value{See Section 15.5, Example 15.5.5.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{randnaiveBayes}}
\code{\link{adaboost}}
\code{\link{bagging}}
\code{\link{gradboost}}
}

\examples{
library(dmr.claseval)
library(dmr.regeval)
data(HouseVotes84, package="mlbench")
data(BostonHousing, package="mlbench")

set.seed(12)
rhv <- runif(nrow(HouseVotes84))
hv.train <- HouseVotes84[rhv>=0.33,]
hv.test <- HouseVotes84[rhv<0.33,]
rbh <- runif(nrow(BostonHousing))
bh.train <- BostonHousing[rbh>=0.33,]
bh.test <- BostonHousing[rbh<0.33,]

  # random forest for the HouseVotes84 data
hv.rf.tree3 <- randforest(Class~., hv.train, 50, args=list(maxdepth=3))
hv.rf.tree5 <- randforest(Class~., hv.train, 50, args=list(maxdepth=5))
hv.rf.tree8 <- randforest(Class~., hv.train, 50, args=list(maxdepth=8))

hv.pred.rf.tree3 <- predict(hv.rf.tree3, hv.test)
hv.pred.rf.tree5 <- predict(hv.rf.tree5, hv.test)
hv.pred.rf.tree8 <- predict(hv.rf.tree8, hv.test)

  # random forest for the BostonHousing data
bh.rf.tree3 <- randforest(medv~., bh.train, 50, args=list(maxdepth=3))
bh.rf.tree5 <- randforest(medv~., bh.train, 50, args=list(maxdepth=5))
bh.rf.tree8 <- randforest(medv~., bh.train, 50, args=list(maxdepth=8))

bh.pred.rf.tree3 <- predict(bh.rf.tree3, bh.test)
bh.pred.rf.tree5 <- predict(bh.rf.tree8, bh.test)
bh.pred.rf.tree8 <- predict(bh.rf.tree8, bh.test)

  # random forest test set errors for the HouseVotes84 data
hv.err.rf <- list(tree3 = err(hv.pred.rf.tree3, hv.test$Class),
                  tree5 = err(hv.pred.rf.tree5, hv.test$Class),
                  tree8 = err(hv.pred.rf.tree8, hv.test$Class))

  # random forest test set MSE values for the BostonHousing data
bh.mse.rf <- list(tree3 = mse(bh.pred.rf.tree3, bh.test$medv),
                  tree5 = mse(bh.pred.rf.tree5, bh.test$medv),
                  tree8 = mse(bh.pred.rf.tree8, bh.test$medv))
}

\keyword{models}
