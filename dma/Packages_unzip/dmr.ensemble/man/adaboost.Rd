\name{adaboost}
\alias{adaboost}
\alias{predict.adaboost}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 15 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 15.5, Example 15.5.3.}

\arguments{See Section 15.5, Example 15.5.3}

\details{See Section 15.5, Example 15.5.3.}

\value{See Section 15.5, Example 15.5.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{bagging}}
\code{\link{randforest}}
\code{\link{gradboost}}
}

\examples{
library(dmr.claseval)
library(rpart)
data(HouseVotes84, package="mlbench")

set.seed(12)
rhv <- runif(nrow(HouseVotes84))
hv.train <- HouseVotes84[rhv>=0.33,]
hv.test <- HouseVotes84[rhv<0.33,]

  # AdaBoost for the HouseVotes84 data
hv.abst.tree1 <- adaboost(Class~., hv.train, 50, rpart,
                          args=list(minsplit=2, cp=0, maxdepth=1),
                          predf=function(...) predict(..., type="c"))
hv.abst.tree3 <- adaboost(Class~., hv.train, 50, rpart,
                          args=list(minsplit=2, cp=0, maxdepth=3),
                          predf=function(...) predict(..., type="c"))
hv.abst.tree5 <- adaboost(Class~., hv.train, 50, rpart,
                          args=list(minsplit=2, cp=0, maxdepth=5),
                          predf=function(...) predict(..., type="c"))

hv.pred.abst.tree1 <- predict(hv.abst.tree1, hv.test,
                              predf=function(...) predict(..., type="c"))
hv.pred.abst.tree3 <- predict(hv.abst.tree3, hv.test,
                              predf=function(...) predict(..., type="c"))
hv.pred.abst.tree5 <- predict(hv.abst.tree5, hv.test,
                              predf=function(...) predict(..., type="c"))

  # AdaBoost test set errors for the HouseVotes84 data
hv.err.abst <- list(tree1 = err(hv.pred.abst.tree1, hv.test$Class),
                    tree3 = err(hv.pred.abst.tree3, hv.test$Class),
                    tree5 = err(hv.pred.abst.tree5, hv.test$Class))
}

\keyword{models}
