\name{mae}
\alias{mae}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 10 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 10.2, Example 10.2.2.}

\arguments{See Section 10.2, Example 10.2.2.}

\details{See Section 10.2, Example 10.2.2.}

\value{See Section 10.2, Example 10.2.2.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{rae}}
\code{\link{mse}}
\code{\link{rmse}}
\code{\link{r2}}
\code{\link{wmae}}
\code{\link{mls}}
}

\examples{
library(rpart)
data(BostonHousing, package="mlbench")

set.seed(12)
rbh <- runif(nrow(BostonHousing))
bh.train <- BostonHousing[rbh>=0.33,]
bh.test <- BostonHousing[rbh<0.33,]
bh.tree <- rpart(medv~., bh.train)

mae(predict(bh.tree, bh.test), bh.test$medv)
}

\keyword{arith}
\keyword{models}
