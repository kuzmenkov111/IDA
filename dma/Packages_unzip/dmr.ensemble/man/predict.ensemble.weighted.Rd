\name{predict.ensemble.weighted}
\alias{predict.ensemble.weighted}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 15 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 15.4, Example 15.4.3.}

\arguments{See Section 15.4, Example 15.4.3.}

\details{See Section 15.4, Example 15.4.3.}

\value{See Section 15.4, Example 15.4.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{predict.ensemble.basic}}
\code{\link{predict.ensemble.prob}}
\code{\link{predict.ensemble.attributes}}
}

\examples{
library(dmr.dectree)
library(dmr.regtree)
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

hv.tree <- rpart(Class~., hv.train)
hv.nb <- naiveBayes(Class~., hv.train)
hv.err.tree <- err(predict(hv.tree, hv.test, type="c"), hv.test$Class)
hv.err.nb <- err(predict(hv.nb, hv.test), hv.test$Class)

bh.tree <- rpart(medv~., bh.train)
bh.lm <- lm(medv~., bh.train)
bh.mse.tree <- mse(predict(bh.tree, bh.test), bh.test$medv)
bh.mse.lm <- mse(predict(bh.lm, bh.test), bh.test$medv)

  # decision/regression tree parameter setups
tree.args <- mapply(function(v1, v2) list(minsplit=v1, cp=v2),
                    rep(seq(2, 20, 2), 2), c(rep(0.0, 10), rep(0.01, 10)),
                        SIMPLIFY=FALSE)

  # base models for the HouseVotes84 data
hv.bm.tree.sx <- base.ensemble.sample.x(Class~., hv.train, 50, rpart)
hv.bm.nb.sx <- base.ensemble.sample.x(Class~., hv.train, 50, naiveBayes)
hv.bm.tree.wx <- base.ensemble.weight.x(Class~., hv.train, 50, rpart)
hv.bm.tree.sa <- base.ensemble.sample.a(Class~., hv.train, 50, rpart,
                                        args=list(minsplit=2, cp=0))
hv.bm.nb.sa <- base.ensemble.sample.a(Class~., hv.train, 50, naiveBayes)
hv.bm.tree.rnd <- base.ensemble.simple(Class~., hv.train, 50, grow.randdectree)

  # base model training set errors for the HouseVotes84 data
hv.train.err.tree.sx <- sapply(hv.bm.tree.sx,
                               function(h) err(predict(h, hv.train, type="c"),
                                               hv.train$Class))
hv.train.err.nb.sx <- sapply(hv.bm.nb.sx,
                             function(h) err(predict(h, hv.train), hv.train$Class))
hv.train.err.tree.wx <- sapply(hv.bm.tree.wx,
                               function(h) err(predict(h, hv.train, type="c"),
                                               hv.train$Class))
hv.train.err.tree.sa <- sapply(hv.bm.tree.sa,
                               function(h) err(predict(h, hv.train, type="c"),
                                               hv.train$Class))
hv.train.err.nb.sa <- sapply(hv.bm.nb.sa,
                             function(h) err(predict(h, hv.train), hv.train$Class))
hv.train.err.tree.rnd <- sapply(hv.bm.tree.rnd,
                                function(h) err(predict(h, hv.train),
                                                hv.train$Class))

  # base models for the BostonHousing data
bh.bm.tree.sx <- base.ensemble.sample.x(medv~., bh.train, 50, rpart)
bh.bm.lm.sx <- base.ensemble.sample.x(medv~., bh.train, 50, lm)
bh.bm.tree.wx <- base.ensemble.weight.x(medv~., bh.train, 50, rpart)
bh.bm.lm.wx <- base.ensemble.weight.x(medv~., bh.train, 50, lm)
bh.bm.tree.sa <- base.ensemble.sample.a(medv~., bh.train, 50, rpart,
                                        args=list(minsplit=2, cp=0))
bh.bm.lm.sa <- base.ensemble.sample.a(medv~., bh.train, 50, lm)
bh.bm.tree.rnd <- base.ensemble.simple(medv~., bh.train, 50, grow.randregtree,
                                       args=list(minvar=25))

  # base model training set MSE values for the BostonHousing data
bh.train.mse.tree.sx <- sapply(bh.bm.tree.sx,
                               function(h) mse(predict(h, bh.train), bh.train$medv))
bh.train.mse.lm.sx <- sapply(bh.bm.lm.sx,
                             function(h) mse(predict(h, bh.train), bh.train$medv))
bh.train.mse.tree.wx <- sapply(bh.bm.tree.wx,
                               function(h) mse(predict(h, bh.train), bh.train$medv))
bh.train.mse.lm.wx <- sapply(bh.bm.lm.wx,
                             function(h) mse(predict(h, bh.train), bh.train$medv))
bh.train.mse.tree.sa <- sapply(bh.bm.tree.sa,
                               function(h) mse(predict(h, bh.train), bh.train$medv))
bh.train.mse.lm.sa <- sapply(bh.bm.lm.sa,
                             function(h) mse(predict(h, bh.train), bh.train$medv))
bh.train.mse.tree.rnd <- sapply(bh.bm.tree.rnd,
                                function(h) mse(predict(h, bh.train), bh.train$medv))

  # combine base models for the HouseVotes84 data
hv.pred.tree.sx.w <- predict.ensemble.weighted(hv.bm.tree.sx,
                                               1/(hv.train.err.tree.sx+0.01),
                                               hv.test,
                                               predf=function(...)
                                                     predict(..., type="c"))
hv.pred.nb.sx.w <- predict.ensemble.weighted(hv.bm.nb.sx,
                                             1/(hv.train.err.nb.sx+0.01),
                                             hv.test)
hv.pred.tree.wx.w <- predict.ensemble.weighted(hv.bm.tree.wx,
                                               1/(hv.train.err.tree.wx+0.01),
                                               hv.test,
                                               predf=function(...)
                                                     predict(..., type="c"))
hv.pred.tree.sa.w <- predict.ensemble.weighted(hv.bm.tree.sa,
                                               1/(hv.train.err.tree.sa+0.01),
                                               hv.test,
                                               predf=function(...)
                                                     predict(..., type="c"))
hv.pred.nb.sa.w <- predict.ensemble.weighted(hv.bm.nb.sa,
                                             1/(hv.train.err.nb.sa+0.01), hv.test)
hv.pred.tree.rnd.w <- predict.ensemble.weighted(hv.bm.tree.rnd,
                                                1/(hv.train.err.tree.rnd+0.01),
                                                hv.test)

  # combine base models for the BostonHousing data
bh.pred.tree.sx.w <- predict.ensemble.weighted(bh.bm.tree.sx,
                                               1/(bh.train.mse.tree.sx+1), bh.test)
bh.pred.lm.sx.w <- predict.ensemble.weighted(bh.bm.lm.sx,
                                             1/(bh.train.mse.lm.sx+1), bh.test)
bh.pred.tree.wx.w <- predict.ensemble.weighted(bh.bm.tree.wx,
                                               1/(bh.train.mse.tree.wx+1), bh.test)
bh.pred.lm.wx.w <- predict.ensemble.weighted(bh.bm.lm.wx,
                                             1/(bh.train.mse.lm.wx+1), bh.test)
bh.pred.tree.sa.w <- predict.ensemble.weighted(bh.bm.tree.sa,
                                               1/(bh.train.mse.tree.sa+1), bh.test)
bh.pred.lm.sa.w <- predict.ensemble.weighted(bh.bm.lm.sa,
                                             1/(bh.train.mse.lm.sa+1), bh.test)
bh.pred.tree.rnd.w <- predict.ensemble.weighted(bh.bm.tree.rnd,
                                                1/(bh.train.mse.tree.rnd+1), bh.test)

  # ensemble model test set errors for the HouseVotes84 data
hv.err.w <- c(tree = hv.err.tree,
              tree.sx = err(hv.pred.tree.sx.w, hv.test$Class),
              tree.wx = err(hv.pred.tree.wx.w, hv.test$Class),
              tree.sa = err(hv.pred.tree.sa.w, hv.test$Class),
              tree.rnd = err(hv.pred.tree.rnd.w, hv.test$Class),
              nb = hv.err.nb,
              nb.sx = err(hv.pred.nb.sx.w, hv.test$Class),
              nb.sa = err(hv.pred.nb.sa.w, hv.test$Class))

  # ensemble model test set MSE values for the BostonHousing data
bh.mse.w <- c(tree = bh.mse.tree,
              tree.sx = mse(bh.pred.tree.sx.w, bh.test$medv),
              tree.wx = mse(bh.pred.tree.wx.w, bh.test$medv),
              tree.sa = mse(bh.pred.tree.sa.w, bh.test$medv),
              tree.rnd = mse(bh.pred.tree.rnd.w, bh.test$medv),
              lm = bh.mse.lm,
              lm.sx = mse(bh.pred.lm.sx.w, bh.test$medv),
              lm.wx = mse(bh.pred.lm.wx.w, bh.test$medv),
              lm.sa = mse(bh.pred.lm.sa.w, bh.test$medv))

barplot(hv.err.w, main="HouseVotes84", ylab="Error", las=2, col="blue")
lines(c(0, 10), rep(hv.err.w[1], 2), col="red")
lines(c(0, 10), rep(hv.err.w[6], 2), col="orange")

barplot(bh.mse.w, main="Boston Housing", ylab="MSE", las=2, col="blue")
lines(c(0, 11), rep(bh.mse.w[1], 2), col="red")
lines(c(0, 11), rep(bh.mse.w[6], 2), col="orange")
}

\keyword{models}
