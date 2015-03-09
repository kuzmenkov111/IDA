\name{eval.bias.var}
\alias{eval.bias.var}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 7 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 7.3, Example 7.3.5.}

\arguments{See Section 7.3, Example 7.3.5.}

\details{See Section 7.3, Example 7.3.5.}

\value{See Section 7.3, Example 7.3.5.}

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

  # the commented lines run a 200-repetition experiment, which takes a long time
#s01.ebv <- eval.bias.var(rpart, Class~., Soybean01,
#                         predf=function(...) predict(..., type="c"), n=200)
  # this can be used for a quick illustration
s01.ebv <- eval.bias.var(rpart, Class~., Soybean01,
                         predf=function(...) predict(..., type="c"), n=10)

boxplot(s01.ebv$performance[,-1], main="Error", las=2, col="blue")
lines(c(0, 13), rep(mean(s01.ebv$performance[,1]), 2), col="red")
barplot(s01.ebv$bias, main="Bias", las=2, col="blue")
barplot(s01.ebv$variance, main="Variance", las=2, col="blue")
}

\keyword{arith}
\keyword{models}
