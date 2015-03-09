\name{roc}
\alias{roc}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 7 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 7.2, Example 7.2.11.}

\arguments{See Section 7.2, Example 7.2.11.}

\details{See Section 7.2, Example 7.2.11.}

\value{See Section 7.2, Example 7.2.11.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{wroc}}
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

  # ROC curve for the decision tree model
s01.roc <- roc(predict(s01.tree, s01.test)[,2], s01.test$Class)
plot(s01.roc$fpr, s01.roc$tpr, type="l", col="blue", xlab="FP rate", ylab="TP rate")

  # ROC curve for a random model
s01rand <- runif(nrow(s01.test))
s01rand.roc <- roc(s01rand, s01.test$Class)
lines(s01rand.roc$fpr, s01rand.roc$tpr, col="black")
}

\keyword{arith}
\keyword{models}
