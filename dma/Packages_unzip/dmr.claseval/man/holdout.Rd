\name{holdout}
\alias{holdout}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 7 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 7, Example 7.3.1.}

\arguments{See Section 7, Example 7.3.1.}

\details{See Section 7, Example 7.3.1.}

\value{See Section 7, Example 7.3.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{crossval}}
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

  # hold-out evaluation of discrete predictions
s01ho <- holdout(rpart, Class~., Soybean01,
                 predf=function(...) predict(..., type="c"), n=10)
err(s01ho$pred, s01ho$true)
confmat(s01ho$pred, s01ho$true)

  # hold-out evaluation of probabilistic predictions
s01hop <- holdout(rpart, Class~., Soybean01,
                  predf=function(...) predict(..., type="p")[,2], prob=TRUE, n=10)
s01hop.roc <- roc(s01hop$pred, s01hop$true)
plot(s01hop.roc$fpr, s01hop.roc$tpr, type="l", col="blue",
     xlab="FP rate", ylab="TP rate")
auc(s01hop.roc)
}

\keyword{arith}
\keyword{models}
