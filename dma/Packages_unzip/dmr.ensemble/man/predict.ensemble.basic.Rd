\name{predict.ensemble.basic}
\alias{predict.ensemble.basic}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 15 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 15.4, Example 15.4.1.}

\arguments{See Section 15.4, Example 15.4.1.}

\details{See Section 15.4, Example 15.4.1.}

\value{See Section 15.4, Example 15.4.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{predict.ensemble.prob}}
\code{\link{predict.ensemble.weighted}}
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

  # combine base models for the HouseVotes84 data
hv.pred.tree.sx.b <- predict.ensemble.basic(hv.bm.tree.sx, hv.test,
                                            predf=function(...)
                                                  predict(..., type="c"))
hv.pred.nb.sx.b <- predict.ensemble.basic(hv.bm.nb.sx, hv.test)
hv.pred.tree.wx.b <- predict.ensemble.basic(hv.bm.tree.wx, hv.test,
                                            predf=function(...)
                                                  predict(..., type="c"))
hv.pred.tree.sa.b <- predict.ensemble.basic(hv.bm.tree.sa, hv.test,
                                            predf=function(...)
                                                  predict(..., type="c"))
hv.pred.nb.sa.b <- predict.ensemble.basic(hv.bm.nb.sa, hv.test)
hv.pred.tree.rnd.b <- predict.ensemble.basic(hv.bm.tree.rnd, hv.test)

  # combine base models for the BostonHousing data
bh.pred.tree.sx.b <- predict.ensemble.basic(bh.bm.tree.sx, bh.test)
bh.pred.lm.sx.b <- predict.ensemble.basic(bh.bm.lm.sx, bh.test)
bh.pred.tree.wx.b <- predict.ensemble.basic(bh.bm.tree.wx, bh.test)
bh.pred.lm.wx.b <- predict.ensemble.basic(bh.bm.lm.wx, bh.test)
bh.pred.tree.sa.b <- predict.ensemble.basic(bh.bm.tree.sa, bh.test)
bh.pred.lm.sa.b <- predict.ensemble.basic(bh.bm.lm.sa, bh.test)
bh.pred.tree.rnd.b <- predict.ensemble.basic(bh.bm.tree.rnd, bh.test)

  # ensemble model test set errors for the HouseVotes84 data
hv.err.b <- c(tree = hv.err.tree,
              tree.sx = err(hv.pred.tree.sx.b, hv.test$Class),
              tree.wx = err(hv.pred.tree.wx.b, hv.test$Class),
              tree.sa = err(hv.pred.tree.sa.b, hv.test$Class),
              tree.rnd = err(hv.pred.tree.rnd.b, hv.test$Class),
              nb = hv.err.nb,
              nb.sx = err(hv.pred.nb.sx.b, hv.test$Class),
              nb.sa = err(hv.pred.nb.sa.b, hv.test$Class))

  # ensemble model test set MSE values for the BostonHousing data
bh.mse.b <- c(tree = bh.mse.tree,
              tree.sx = mse(bh.pred.tree.sx.b, bh.test$medv),
              tree.wx = mse(bh.pred.tree.wx.b, bh.test$medv),
              tree.sa = mse(bh.pred.tree.sa.b, bh.test$medv),
              tree.rnd = mse(bh.pred.tree.rnd.b, bh.test$medv),
              lm = bh.mse.lm,
              lm.sx = mse(bh.pred.lm.sx.b, bh.test$medv),
              lm.wx = mse(bh.pred.lm.wx.b, bh.test$medv),
              lm.sa = mse(bh.pred.lm.sa.b, bh.test$medv))

barplot(hv.err.b, main="HouseVotes84", ylab="Error", las=2, col="blue")
lines(c(0, 10), rep(hv.err.b[1], 2), col="red")
lines(c(0, 10), rep(hv.err.b[6], 2), col="orange")

barplot(bh.mse.b, main="Boston Housing", ylab="MSE", las=2, col="blue")
lines(c(0, 11), rep(bh.mse.b[1], 2), col="red")
lines(c(0, 11), rep(bh.mse.b[6], 2), col="orange")
}

\keyword{models}
