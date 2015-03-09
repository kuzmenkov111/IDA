\name{mincostclas}
\alias{mincostclas}
\alias{mincostclas1}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 6 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 6.3, Example 6.3.3.}

\arguments{See Section 6.3, Example 6.3.3.}

\details{See Section 6.3, Example 6.3.3.}

\value{See Section 6.3, Example 6.3.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{mc.mincost}}
\code{\link{mincostclas1}}
}

\examples{
library(rpart)
library(dmr.claseval)
data(Vehicle, package="mlbench")

set.seed(12)

  # training and test subsets
rv <- runif(nrow(Vehicle))
v.train <- Vehicle[rv>=0.33,]
v.test <- Vehicle[rv<0.33,]

  # misclassification cost matrix
v.rm <- matrix(0, nrow=nlevels(Vehicle$Class), ncol=nlevels(Vehicle$Class),
               dimnames=list(predicted=levels(Vehicle$Class),
                             true=levels(Vehicle$Class)))

v.rm["bus","opel"] <- 7
v.rm["bus","van"] <- 0.2
v.rm["bus","saab"] <- 7
v.rm["opel","bus"] <- 1.4
v.rm["opel","saab"] <- 1
v.rm["opel","van"] <- 1.4
v.rm["saab","bus"] <- 1.4
v.rm["saab","opel"] <- 1
v.rm["saab","van"] <- 1.4
v.rm["van","bus"] <- 0.2
v.rm["van","opel"] <- 7
v.rm["van","saab"] <- 7

v.tree <- rpart(Class~., v.train)
v.pred <- predict(v.tree, v.test, type="p")
mincostclas1(v.pred[1,], v.rm)
mincost.pred <- mincostclas(v.pred, v.rm)
}

\keyword{models}
