\name{bootstrap}
\alias{bootstrap}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 7 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 7.3, Example 7.3.4.}

\arguments{See Section 7.3, Example 7.3.4.}

\details{See Section 7.3, Example 7.3.4.}

\value{See Section 7.3, Example 7.3.4.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{holdout}}
\code{\link{crossval}}
\code{\link{leave1out}}
}

\examples{
library(rpart)
library(dmr.util)
data(Soybean, package="mlbench")

s01.labels <- c("other", "brown-spot")
Soybean01 <- Soybean
Soybean01$Class <- factor(ifelse(Soybean$Class=="brown-spot", "brown-spot", "other"),
                          levels=s01.labels)

  # 20x bootstrap for discrete predictions
s01bs20 <- bootstrap(rpart, Class~., Soybean01,
                     predf=function(...) predict(..., type="c"), w=1, m=20)
err(s01bs20$pred, s01bs20$true)
confmat(s01bs20$pred, s01bs20$true)

  # 20x .632 bootstrap for discrete predictions
s01.632bs20 <- bootstrap(rpart, Class~., Soybean01,
                         predf=function(...) predict(..., type="c"), m=20)
  # weighted error
werr(s01.632bs20$pred, s01.632bs20$true, s01.632bs20$w)
  # weighted confusion matrix
wconfmat(s01.632bs20$pred, s01.632bs20$true, s01.632bs20$w)

  # 20x bootstrap for probabilistic predictions
s01bs20p <- bootstrap(rpart, Class~., Soybean01,
                      predf=function(...) predict(..., type="p")[,2],
                      prob=TRUE, w=1, m=20)
s01bs20p.roc <- roc(s01bs20p$pred, s01bs20p$true)
plot(s01bs20p.roc$fpr, s01bs20p.roc$tpr, type="l", col="blue",
     xlab="FP rate", ylab="TP rate")
auc(s01bs20p.roc)

  # 20x .632 bootstrap for probabilistic predictions
s01.632bs20p <- bootstrap(rpart, Class~., Soybean01,
                          predf=function(...) predict(..., type="p")[,2],
                          prob=TRUE, m=20)
  # weighted ROC
s01.632bs20p.roc <- wroc(s01.632bs20p$pred, s01.632bs20p$true, s01.632bs20p$w)
lines(s01.632bs20p.roc$fpr, s01.632bs20p.roc$tpr, col="red")
legend("bottomright", c("plain", ".632"), col=c("blue", "red"), lty=1)
auc(s01.632bs20p.roc)
}

\keyword{arith}
\keyword{models}
