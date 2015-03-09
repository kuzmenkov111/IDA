\name{roc.shift}
\alias{roc.shift}
\alias{cutclass}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 7 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 7.2, Example 7.2.12.}

\arguments{See Section 7.2, Example 7.2.12.}

\details{See Section 7.2, Example 7.2.12.}

\value{See Section 7.2, Example 7.2.12.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{mixclass}}
\code{\link{roc}}
}

\examples{
library(rpart)
library(dmr.util)
data(Soybean, package="mlbench")

s01.labels <- c("other", "brown-spot")
Soybean01 <- Soybean
Soybean01$Class <- factor(ifelse(Soybean$Class=="brown-spot", "brown-spot", "other"),
                          levels=s01.labels)
set.seed(12)
rs <- runif(nrow(Soybean))
s01.train <- Soybean01[rs>=0.33,]
s01.test <- Soybean01[rs<0.33,]
s01.tree <- rpart(Class~., s01.train)

s01.roc <- roc(predict(s01.tree, s01.test)[,2], s01.test$Class)
s01.cm <- confmat(predict(s01.tree, s01.test, type="c"), s01.test$Class)

  # shift to achieve tpr>0.85 at minimum fpr
s01.t085 <- roc.shift(s01.roc, min.tpr=0.85)
s01.cm.t085 <- confmat(cutclass(predict(s01.tree, s01.test)[,2],
                                s01.t085, s01.labels),
                       s01.test$Class)
  # shift to achieve maximum tpr at fpr<0.5
s01.f05 <- roc.shift(s01.roc, max.fpr=0.5)
s01.cm.f05 <- confmat(cutclass(predict(s01.tree, s01.test)[,2], s01.f05, s01.labels),
                      s01.test$Class)
  # the ROC curve
plot(s01.roc$fpr, s01.roc$tpr, type="l", col="blue", xlab="FP rate", ylab="TP rate")
  # the default operating point
points(fpr(s01.cm), tpr(s01.cm), col="red")
  # the shifted operating points
points(fpr(s01.cm.t085), tpr(s01.cm.t085), col="green")
points(fpr(s01.cm.f05), tpr(s01.cm.f05), col="green")
}

\keyword{arith}
\keyword{models}
