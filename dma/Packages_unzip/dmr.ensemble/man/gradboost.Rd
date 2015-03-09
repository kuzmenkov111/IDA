\name{gradboost}
\alias{gradboost}
\alias{predict.gradboost}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 15 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 15.5, Example 15.5.4.}

\arguments{See Section 15.5, Example 15.5.4.}

\details{See Section 15.5, Example 15.5.4.}

\value{See Section 15.5, Example 15.5.4.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{adaboost}}
\code{\link{bagging}}
\code{\link{randforest}}
}

\examples{
library(dmr.regeval)
library(rpart)
data(BostonHousing, package="mlbench")

set.seed(12)
rbh <- runif(nrow(BostonHousing))
bh.train <- BostonHousing[rbh>=0.33,]
bh.test <- BostonHousing[rbh<0.33,]

  # gradient boosting for the BostonHousing data
bh.gbst.tree1 <- gradboost(medv~., bh.train, 50, rpart,
                           args=list(minsplit=2, cp=0, maxdepth=1))
bh.gbst.tree3 <- gradboost(medv~., bh.train, 50, rpart,
                           args=list(minsplit=2, cp=0, maxdepth=3))
bh.gbst.tree5 <- gradboost(medv~., bh.train, 50, rpart,
                           args=list(minsplit=2, cp=0, maxdepth=5))
bh.gbst.lm <- gradboost(medv~., bh.train, 50, lm)

bh.pred.gbst.tree1 <- predict(bh.gbst.tree1, bh.test)
bh.pred.gbst.tree3 <- predict(bh.gbst.tree3, bh.test)
bh.pred.gbst.tree5 <- predict(bh.gbst.tree5, bh.test)
bh.pred.gbst.lm <- predict(bh.gbst.lm, bh.test)

  # gradient boosting test set MSE values for the BostonHousing data
bh.mse.gbst <- list(tree1 = mse(bh.pred.gbst.tree1, bh.test$medv),
                    tree3 = mse(bh.pred.gbst.tree3, bh.test$medv),
                    tree5 = mse(bh.pred.gbst.tree5, bh.test$medv),
                    lm = mse(bh.pred.gbst.lm, bh.test$medv))
}

\keyword{models}
