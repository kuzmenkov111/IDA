\name{base.ensemble.sample.a}
\alias{base.ensemble.sample.a}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 15 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 15.3, Example 15.3.3.}

\arguments{See Section 15.3, Example 15.3.3.}

\details{See Section 15.3, Example 15.3.3.}

\value{See Section 15.3, Example 15.3.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{base.ensemble.sample.x}}
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
hv.bm.tree.sa <- base.ensemble.sample.a(Class~., hv.train, 50, rpart,
                                        args=list(minsplit=2, cp=0))
hv.bm.nb.sa <- base.ensemble.sample.a(Class~., hv.train, 50, naiveBayes)

  # base models for the BostonHousing data
bh.bm.tree.sa <- base.ensemble.sample.a(medv~., bh.train, 50, rpart,
                                        args=list(minsplit=2, cp=0))
bh.bm.lm.sa <- base.ensemble.sample.a(medv~., bh.train, 50, lm)

  # base model training set errors for the HouseVotes84 data
hv.train.err.tree.sa <- sapply(hv.bm.tree.sa,
                               function(h) err(predict(h, hv.train, type="c"),
                                               hv.train$Class))
hv.train.err.nb.sa <- sapply(hv.bm.nb.sa,
                             function(h) err(predict(h, hv.train), hv.train$Class))

  # base model training set MSE values for the BostonHousing data
bh.train.mse.tree.sa <- sapply(bh.bm.tree.sa,
                               function(h) mse(predict(h, bh.train), bh.train$medv))
bh.train.mse.lm.sa <- sapply(bh.bm.lm.sa,
                             function(h) mse(predict(h, bh.train), bh.train$medv))

  # base model test set errors for the HouseVotes84 data
hv.test.err.tree.sa <- sapply(hv.bm.tree.sa,
                              function(h) err(predict(h, hv.test, type="c"),
                                              hv.test$Class))
hv.test.err.nb.sa <- sapply(hv.bm.nb.sa,
                            function(h) err(predict(h, hv.test), hv.test$Class))

  # base model test set MSE values for the BostonHousing data
bh.test.mse.tree.sa <- sapply(bh.bm.tree.sa,
                              function(h) mse(predict(h, bh.test), bh.test$medv))
bh.test.mse.lm.sa <- sapply(bh.bm.lm.sa,
                            function(h) mse(predict(h, bh.test), bh.test$medv))
}

\keyword{models}
