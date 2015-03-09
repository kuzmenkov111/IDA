\name{confmat}
\alias{confmat}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 7 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 7.2, Example 7.2.4.}

\arguments{See Section 7.2, Example 7.2.4.}

\details{See Section 7.2, Example 7.2.4.}

\value{See Section 7.2, Example 7.2.4.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{wconfmat}}
\code{\link{tpr}}
\code{\link{fpr}}
\code{\link{precision}}
\code{\link{recall}}
\code{\link{sensitivity}}
\code{\link{specificity}}
\code{\link{f.measure}}
\code{\link{roc}}
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

s.cm <- confmat(predict(s.tree, s.test, type="c"), s.test$Class)
  # error
(sum(s.cm)-sum(diag(s.cm)))/(sum(s.cm))
  # mean misclassification cost
s.r4test <- matrix(round(runif(nlevels(s.test$Class)*nlevels(s.test$Class),
                         min=1, max=5)),
                   nrow=nlevels(s.test$Class), ncol=nlevels(s.test$Class))
sum(s.cm*s.r4test)/sum(s.cm)
}

\keyword{arith}
\keyword{models}
