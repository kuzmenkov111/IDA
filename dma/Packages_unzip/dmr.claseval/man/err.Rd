\name{err}
\alias{err}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 7 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 7.2, Example 7.2.1.}

\arguments{See Section 7.2, Example 7.2.1.}

\details{See Section 7.2, Example 7.2.1.}

\value{See Section 7.2, Example 7.2.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{werr}}
\code{\link{mean.cost}}
\code{\link{confmat}}
}

\examples{
library(rpart)
data(Soybean, package="mlbench")

set.seed(12)
rs <- runif(nrow(Soybean))
s.train <- Soybean[rs>=0.33,]
s.test <- Soybean[rs<0.33,]
s.tree <- rpart(Class~., s.train)

err(predict(s.tree, s.test, type="c"), s.test$Class)
}

\keyword{arith}
\keyword{models}
