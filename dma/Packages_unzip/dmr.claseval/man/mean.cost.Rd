\name{mean.cost}
\alias{mean.cost}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 7 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 7.2, Example 7.2.3.}

\arguments{See Section 7.2, Example 7.2.3.}

\details{See Section 7.2, Example 7.2.3.}

\value{See Section 7.2, Example 7.2.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{err}}
\code{\link{werr}}
\code{\link{confmat}}
}

\examples{
library(rpart)
data(Soybean, package="mlbench")

set.seed(12)
rs <- runif(nrow(Soybean))
s.train <- Soybean[rs>=0.33,]
s.test <- Soybean[rs<0.33,]
s.tree <- rpart(Class~., s.train)

  # uniform cost matrix
s.r1test <- matrix(1, nrow=nlevels(s.test$Class), ncol=nlevels(s.test$Class))
diag(s.r1test) <- 0
mean.cost(predict(s.tree, s.test, type="c"), s.test$Class, s.r1test)

  # double cost for misclassifying the least frequent class
s.r2test <- matrix(1, nrow=nlevels(s.test$Class), ncol=nlevels(s.test$Class))
s.r2test[,levels(s.test$Class)=="herbicide-injury"] <- 2
diag(s.r2test) <- 0
mean.cost(predict(s.tree, s.test, type="c"), s.test$Class, s.r2test)
  # this should give the same result
s.w2test <- ifelse(s.test$Class=="herbicide-injury", 2, 1)
sum(s.w2test)/nrow(s.test)*werr(predict(s.tree, s.test, type="c"),
                                s.test$Class, s.w2test)

  # random per-class costs 1..5
s.wctest <- round(runif(nlevels(s.test$Class), min=1, max=5))
s.w3test <- s.wctest[s.test$Class]
s.r3test <- matrix(s.wctest, nrow=nlevels(s.test$Class), ncol=nlevels(s.test$Class),
                   byrow=TRUE)
diag(s.r3test) <- 0
mean.cost(predict(s.tree, s.test, type="c"), s.test$Class, s.r3test)
  # this should give the same result
sum(s.w3test)/nrow(s.test)*werr(predict(s.tree, s.test, type="c"),
                                s.test$Class, s.w3test)

  # random costs 1..5
s.r4test <- matrix(round(runif(nlevels(s.test$Class)*nlevels(s.test$Class),
                               min=1, max=5)),
                   nrow=nlevels(s.test$Class), ncol=nlevels(s.test$Class))
diag(s.r4test) <- 0
mean.cost(predict(s.tree, s.test, type="c"), s.test$Class, s.r4test)
}

\keyword{arith}
\keyword{models}
