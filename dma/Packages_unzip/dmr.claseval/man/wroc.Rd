\name{wroc}
\alias{wroc}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 7 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 7.2, Example 7.2.15.}

\arguments{See Section 7.2, Example 7.2.15.}

\details{See Section 7.2, Example 7.2.15.}

\value{See Section 7.2, Example 7.2.15.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{roc}}
\code{\link{tpr}}
\code{\link{fpr}}
\code{\link{auc}}
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

  # ROC curve with double weight for the brown-spot class
s01.w1test <- ifelse(s01.test$Class=="brown-spot", 2, 1)
s01.w1roc <- wroc(predict(s01.tree, s01.test)[,2], s01.test$Class, s01.w1test)
plot(s01.w1roc$fpr, s01.w1roc$tpr, type="l", col="blue", xlab="FP rate", ylab="TP rate")
auc(s01.w1roc)

  # ROC curve with 10 times less weight for instances with plant.stand=1
s01.w2test <- ifelse(!is.na(s01.test$plant.stand) & s01.test$plant.stand=="1", 0.1, 1)
s01.w2roc <- wroc(predict(s01.tree, s01.test)[,2], s01.test$Class, s01.w2test)
lines(s01.w2roc$fpr, s01.w2roc$tpr, col="red")
legend("bottomright", c("brown-spot x2", "plant.stand=1 x10"),
       col=c("blue", "red"), lty=1)
auc(s01.w2roc)
}

\keyword{arith}
\keyword{models}
