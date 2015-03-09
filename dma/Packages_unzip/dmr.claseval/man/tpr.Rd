\name{tpr}
\alias{tpr}
\alias{fpr}
\alias{precision}
\alias{recall}
\alias{sensitivity}
\alias{specificity}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 7 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 7.2, Example 7.2.6.}

\arguments{See Section 7.2, Example 7.2.6.}

\details{See Section 7.2, Example 7.2.6.}

\value{See Section 7.2, Example 7.2.6.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{confmat}}
\code{\link{f.measure}}
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

s01.cm <- confmat(predict(s01.tree, s01.test, type="c"), s01.test$Class)
list(tpr=tpr(s01.cm),
     fpr=fpr(s01.cm),
     precision=precision(s01.cm),
     recall=recall(s01.cm),
     sensitivity=sensitivity(s01.cm),
     specificity=specificity(s01.cm))
}

\keyword{arith}
\keyword{models}
