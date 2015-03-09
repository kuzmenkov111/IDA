\name{predict.ensemble.attributes}
\alias{predict.ensemble.attributes}
\alias{combine.ensemble.attributes}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 15 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 15.4, Example 15.4.4.}

\arguments{See Section 15.4, Example 15.4.4.}

\details{See Section 15.4, Example 15.4.4.}

\value{See Section 15.4, Example 15.4.4.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{predict.ensemble.basic}}
\code{\link{predict.ensemble.prob}}
\code{\link{predict.ensemble.weighted}}
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

  # base models for the BostonHousing data
bh.bm.tree.sx <- base.ensemble.sample.x(medv~., bh.train, 50, rpart)
bh.bm.lm.sx <- base.ensemble.sample.x(medv~., bh.train, 50, lm)
bh.bm.tree.wx <- base.ensemble.weight.x(medv~., bh.train, 50, rpart)
bh.bm.lm.wx <- base.ensemble.weight.x(medv~., bh.train, 50, lm)
bh.bm.tree.sa <- base.ensemble.sample.a(medv~., bh.train, 50, rpart,
                                        args=list(minsplit=2, cp=0))
bh.bm.lm.sa <- base.ensemble.sample.a(medv~., bh.train, 50, lm)

  # combine base models for the HouseVotes84 data
hv.tree.sx.nb <- combine.ensemble.attributes(hv.bm.tree.sx, hv.train, naiveBayes,
                                             predf=function(...)
                                                   predict(..., type="c"))
hv.pred.tree.sx.nb <- predict.ensemble.attributes(hv.tree.sx.nb, hv.bm.tree.sx,
                                                  hv.test,
                                                  base.predf=function(...)
                                                             predict(..., type="c"))
hv.tree.wx.nb <- combine.ensemble.attributes(hv.bm.tree.wx, hv.train, naiveBayes,
                                             predf=function(...)
                                                   predict(..., type="c"))
hv.pred.tree.wx.nb <- predict.ensemble.attributes(hv.tree.wx.nb, hv.bm.tree.wx,
                                                  hv.test,
                                                  base.predf=function(...)
                                                             predict(..., type="c"))
hv.tree.sa.nb <- combine.ensemble.attributes(hv.bm.tree.sa, hv.train, naiveBayes,
                                             predf=function(...)
                                                   predict(..., type="c"))
hv.pred.tree.sa.nb <- predict.ensemble.attributes(hv.tree.sa.nb, hv.bm.tree.sa,
                                                  hv.test,
                                                  base.predf=function(...)
                                                             predict(..., type="c"))

  # combine base models for the BostonHousing data
bh.tree.sx.lm <- combine.ensemble.attributes(bh.bm.tree.sx, bh.train, lm)
bh.pred.tree.sx.lm <- predict.ensemble.attributes(bh.tree.sx.lm, bh.bm.tree.sx,
                                                  bh.test)
bh.tree.wx.lm <- combine.ensemble.attributes(bh.bm.tree.wx, bh.train, lm)
bh.pred.tree.wx.lm <- predict.ensemble.attributes(bh.tree.wx.lm, bh.bm.tree.wx,
                                                  bh.test)
bh.tree.sa.lm <- combine.ensemble.attributes(bh.bm.tree.sa, bh.train, lm)
bh.pred.tree.sa.lm <- predict.ensemble.attributes(bh.tree.sa.lm, bh.bm.tree.sa,
                                                  bh.test)

  # ensemble model test set errors for the HouseVotes84 data
hv.err.a <- c(tree = hv.err.tree,
              tree.sx.nb = err(hv.pred.tree.sx.nb, hv.test$Class),
              tree.wx.nb = err(hv.pred.tree.wx.nb, hv.test$Class),
              tree.sa.nb = err(hv.pred.tree.sa.nb, hv.test$Class))

  # ensemble model test set MSE values for the BostonHousing data
bh.mse.a <- c(tree = bh.mse.tree,
              tree.sx.lm = mse(bh.pred.tree.sx.lm, bh.test$medv),
              tree.wx.lm = mse(bh.pred.tree.wx.lm, bh.test$medv),
              tree.sa.lm = mse(bh.pred.tree.sa.lm, bh.test$medv))

barplot(hv.err.a, main="HouseVotes84", ylab="Error", las=2, col="blue")
lines(c(0, 5), rep(hv.err.a[1], 2), col="red")

barplot(bh.mse.a, main="Boston Housing", ylab="MSE", las=2, col="blue")
lines(c(0, 5), rep(bh.mse.a[1], 2), col="red")
}

\keyword{models}
