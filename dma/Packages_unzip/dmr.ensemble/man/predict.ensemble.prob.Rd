\name{predict.ensemble.prob}
\alias{predict.ensemble.prob}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 15 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 15.4, Example 15.4.2.}

\arguments{See Section 15.4, Example 15.4.2.}

\details{See Section 15.4, Example 15.4.2.}

\value{See Section 15.4, Example 15.4.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{predict.ensemble.basic}}
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

set.seed(12)
rhv <- runif(nrow(HouseVotes84))
hv.train <- HouseVotes84[rhv>=0.33,]
hv.test <- HouseVotes84[rhv<0.33,]

hv.tree <- rpart(Class~., hv.train)
hv.nb <- naiveBayes(Class~., hv.train)
hv.err.tree <- err(predict(hv.tree, hv.test, type="c"), hv.test$Class)
hv.err.nb <- err(predict(hv.nb, hv.test), hv.test$Class)

  # base models for the HouseVotes84 data
hv.bm.tree.sx <- base.ensemble.sample.x(Class~., hv.train, 50, rpart)
hv.bm.nb.sx <- base.ensemble.sample.x(Class~., hv.train, 50, naiveBayes)
hv.bm.tree.wx <- base.ensemble.weight.x(Class~., hv.train, 50, rpart)
hv.bm.tree.sa <- base.ensemble.sample.a(Class~., hv.train, 50, rpart,
                                        args=list(minsplit=2, cp=0))
hv.bm.nb.sa <- base.ensemble.sample.a(Class~., hv.train, 50, naiveBayes)

  # combine base models for the HouseVotes84 data
  # combine base models for the HouseVotes84 data
hv.pred.tree.sx.p <- predict.ensemble.prob(hv.bm.tree.sx, hv.test)
hv.pred.nb.sx.p <- predict.ensemble.prob(hv.bm.nb.sx, hv.test,
                                         predf=function(...) predict(..., type="r"))
hv.pred.tree.wx.p <- predict.ensemble.prob(hv.bm.tree.wx, hv.test)
hv.pred.tree.sa.p <- predict.ensemble.prob(hv.bm.tree.sa, hv.test)
hv.pred.nb.sa.p <- predict.ensemble.prob(hv.bm.nb.sa, hv.test,
                                         predf=function(...) predict(..., type="r"))

  # ensemble model test set errors for the HouseVotes84 data
hv.err.p <- c(tree = hv.err.tree,
              tree.sx = err(hv.pred.tree.sx.p, hv.test$Class),
              tree.wx = err(hv.pred.tree.wx.p, hv.test$Class),
              tree.sa = err(hv.pred.tree.sa.p, hv.test$Class),
              nb = hv.err.nb,
              nb.sx = err(hv.pred.nb.sx.p, hv.test$Class),
              nb.sa = err(hv.pred.nb.sa.p, hv.test$Class))

barplot(hv.err.p, main="HouseVotes84", ylab="Error", las=2, col="blue")
lines(c(0, 9), rep(hv.err.p[1], 2), col="red")
lines(c(0, 9), rep(hv.err.p[5], 2), col="orange")
}

\keyword{models}
