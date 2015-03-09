\name{leave1out}
\alias{leave1out}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 7 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 7.3, Example 7.3.3.}

\arguments{See Section 7.3, Example 7.3.3.}

\details{See Section 7.3, Example 7.3.3.}

\value{See Section 7.3, Example 7.3.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{holdout}}
\code{\link{crossval}}
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

  # leave-one-out for discrete predictions
s01l1o <- leave1out(rpart, Class~., Soybean01,
                    predf=function(...) predict(..., type="c"))
err(s01l1o$pred, s01l1o$true)
confmat(s01l1o$pred, s01l1o$true)

  # leave-one-out for probabilistic predictions
s01l1op <- leave1out(rpart, Class~., Soybean01,
                     predf=function(...) predict(..., type="p")[,2], prob=TRUE)
s01l1op.roc <- roc(s01l1op$pred, s01l1op$true)
plot(s01l1op.roc$fpr, s01l1op.roc$tpr, type="l", col="blue",
     xlab="FP rate", ylab="TP rate")
auc(s01l1op.roc)
}

\keyword{arith}
\keyword{models}
