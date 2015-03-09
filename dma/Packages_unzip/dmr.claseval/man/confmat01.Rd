\name{confmat01}
\alias{confmat01}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 7 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 7.2, Example 7.2.8.}

\arguments{See Section 7.2, Example 7.2.8.}

\details{See Section 7.2, Example 7.2.8.}

\value{See Section 7.2, Example 7.2.8.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{confmat}}
\code{\link{tpr}}
\code{\link{fpr}}
\code{\link{precision}}
\code{\link{recall}}
\code{\link{sensitivity}}
\code{\link{specificity}}
\code{\link{f.measure}}
}

\examples{
library(rpart)
library(dmr.util)
data(Soybean, package="mlbench")

set.seed(12)
rs <- runif(nrow(Soybean))
s.train <- Soybean[rs>=0.33,]
s.test <- Soybean[rs<0.33,]
s.tree <- rpart(Class~., s.train)

s.cm01 <- confmat01(predict(s.tree, s.test, type="c"), s.test$Class)
  # average TP rate, FP rate, and f-measure
rowMeans(sapply(s.cm01, function(cm) c(tpr=tpr(cm), fpr=fpr(cm), fm=f.measure(cm))))

}

\keyword{arith}
\keyword{models}
