\name{werr}
\alias{werr}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 7 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 7.2, Example 7.2.2.}

\arguments{See Section 7.2, Example 7.2.2.}

\details{See Section 7.2, Example 7.2.2.}

\value{See Section 7.2, Example 7.2.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{err}}
\code{\link{mean.cost}}
\code{\link{wconfmat}}
}

\examples{
library(rpart)
data(Soybean, package="mlbench")

set.seed(12)
rs <- runif(nrow(Soybean))
s.train <- Soybean[rs>=0.33,]
s.test <- Soybean[rs<0.33,]
s.tree <- rpart(Class~., s.train)

  # double weight for the least frequent class
s.w2test <- ifelse(s.test$Class=="herbicide-injury", 2, 1)
werr(predict(s.tree, s.test, type="c"), s.test$Class, s.w2test)

  # random per-class weights 1..5
s.wctest <- round(runif(nlevels(s.test$Class), min=1, max=5))
s.w3test <- s.wctest[s.test$Class]
werr(predict(s.tree, s.test, type="c"), s.test$Class, s.w3test)
}

\keyword{arith}
\keyword{models}
