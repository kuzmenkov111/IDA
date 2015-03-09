\name{crossval}
\alias{crossval}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 7 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 7.3, Example 7.3.2.}

\arguments{See Section 7.3, Example 7.3.2.}

\details{See Section 7.3, Example 7.3.2.}

\value{See Section 7.3, Example 7.3.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{holdout}}
\code{\link{leave1out}}
\code{\link{bootstrap}}
}

\examples{
library(rpart)
library(dmr.util)
data(Soybean, package="mlbench")

s01.labels <- c("other", "brown-spot")
Soybean01 <- Soybean
Soybean01$Class <- factor(ifelse(Soybean$Class=="brown-spot", "brown-spot", "other"),
                          levels=s01.labels)

  # 3-fold cross-validation for discrete predictions
s01cv3 <- crossval(rpart, Class~., Soybean01,
                   predf=function(...) predict(..., type="c"), k=3)
err(s01cv3$pred, s01cv3$true)
confmat(s01cv3$pred, s01cv3$true)

  # 10-fold cross-validation for discrete predictions
s01cv10 <- crossval(rpart, Class~., Soybean01,
                    predf=function(...) predict(..., type="c"), k=10)
err(s01cv10$pred, s01cv10$true)
confmat(s01cv10$pred, s01cv10$true)

  # 20-fold cross-validation for discrete predictions
s01cv20 <- crossval(rpart, Class~., Soybean01,
                    predf=function(...) predict(..., type="c"), k=20)
err(s01cv20$pred, s01cv20$true)
confmat(s01cv20$pred, s01cv20$true)

  # 4x5-fold cross-validation for discrete predictions
s01cv4x5 <- crossval(rpart, Class~., Soybean01,
                     predf=function(...) predict(..., type="c"), k=5, n=4)
err(s01cv20$pred, s01cv4x5$true)
confmat(s01cv4x5$pred, s01cv4x5$true)

  # 10-fold cross-validation for probabilistic predictions
s01cv10p <- crossval(rpart, Class~., Soybean01,
                     predf=function(...) predict(..., type="p")[,2], prob=TRUE, k=10)
s01cv10p.roc <- roc(s01cv10p$pred, s01cv10p$true)
plot(s01cv10p.roc$fpr, s01cv10p.roc$tpr, type="l", col="blue",
     xlab="FP rate", ylab="TP rate")
auc(s01cv10p.roc)
}

\keyword{arith}
\keyword{models}
