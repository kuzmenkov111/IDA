\name{base.ensemble.sample.x}
\alias{base.ensemble.sample.x}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 15 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 15.3, Example 15.3.1.}

\arguments{See Section 15.3, Example 15.3.1.}

\details{See Section 15.3, Example 15.3.1.}

\value{See Section 15.3, Example 15.3.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{base.ensemble.sample.a}}
\code{\link{base.ensemble.weight.x}}
\code{\link{base.ensemble.simple}}
}

\examples{
library(dmr.claseval)
library(dmr.regeval)
library(rpart)
library(e1071)
data(HouseVotes84, package="mlbench")
data(BostonHousing, package="mlbench")

set.seed(12)
rhv <- runif(nrow(HouseVotes84))
hv.train <- HouseVotes84[rhv>=0.33,]
hv.test <- HouseVotes84[rhv<0.33,]
rbh <- runif(nrow(BostonHousing))
bh.train <- BostonHousing[rbh>=0.33,]
bh.test <- BostonHousing[rbh<0.33,]

  # base models for the HouseVotes84 data
hv.bm.tree.sx <- base.ensemble.sample.x(Class~., hv.train, 50, rpart)
hv.bm.nb.sx <- base.ensemble.sample.x(Class~., hv.train, 50, naiveBayes)

  # base models for the BostonHousing data
bh.bm.tree.sx <- base.ensemble.sample.x(medv~., bh.train, 50, rpart)
bh.bm.lm.sx <- base.ensemble.sample.x(medv~., bh.train, 50, lm)

  # base model training set errors for the HouseVotes84 data
hv.train.err.tree.sx <- sapply(hv.bm.tree.sx, function(h)
                                              err(predict(h, hv.train, type="c"),
                                                  hv.train$Class))
hv.train.err.nb.sx <- sapply(hv.bm.nb.sx, function(h)
                                          err(predict(h, hv.train), hv.train$Class))

  # base model training set MSE values for the BostonHousing data
bh.train.mse.tree.sx <- sapply(bh.bm.tree.sx, function(h)
                                              mse(predict(h, bh.train), bh.train$medv))
bh.train.mse.lm.sx <- sapply(bh.bm.lm.sx, function(h)
                                          mse(predict(h, bh.train), bh.train$medv))

  # base model test set errors for the HouseVotes84 data
hv.test.err.tree.sx <- sapply(hv.bm.tree.sx, function(h)
                                             err(predict(h, hv.test, type="c"),
                                                 hv.test$Class))
hv.test.err.nb.sx <- sapply(hv.bm.nb.sx, function(h)
                                         err(predict(h, hv.test), hv.test$Class))

  # base model test set MSE values for the BostonHousing data
bh.test.mse.tree.sx <- sapply(bh.bm.tree.sx, function(h)
                                              mse(predict(h, bh.test), bh.test$medv))
bh.test.mse.lm.sx <- sapply(bh.bm.lm.sx, function(h)
                                          mse(predict(h, bh.test), bh.test$medv))
}

\keyword{models}
