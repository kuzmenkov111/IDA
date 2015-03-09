\name{f.measure}
\alias{f.measure}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 7 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 7.2, Example 7.2.7.}

\arguments{See Section 7.2, Example 7.2.7.}

\details{See Section 7.2, Example 7.2.7.}

\value{See Section 7.2, Example 7.2.7.}

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

s01.cm <- confmat(predict(s01.tree, s01.test, type="c"), s01.test$Class)
f.measure(s01.cm)
}

\keyword{arith}
\keyword{models}
