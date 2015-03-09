\name{mc.weight}
\alias{mc.weight}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 6 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 6.3, Example 6.3.1.}

\arguments{See Section 6.3, Example 6.3.1.}

\details{See Section 6.3, Example 6.3.1.}

\value{See Section 6.3, Example 6.3.1.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{mc.resample}}
\code{\link{mc.mincost}}
\code{\link{mc.relabel}}
}

\examples{
library(rpart)
library(dmr.claseval)
data(Vehicle, package="mlbench")
data(Glass, package="mlbench")

set.seed(12)

  # training and test subsets
rv <- runif(nrow(Vehicle))
v.train <- Vehicle[rv>=0.33,]
v.test <- Vehicle[rv<0.33,]

  # two-class version
Vehicle01 <- Vehicle
Vehicle01$Class <- factor(ifelse(Vehicle$Class \%in\% c("opel", "saab"),
                                 "car", "other"))
v01.train <- Vehicle01[rv>=0.33,]
v01.test <- Vehicle01[rv<0.33,]

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

  # per-class cost vector
v.rc <- rhom2c(v.rm)

  # per-class cost vector in a matrix representation
v.rcm <- rhoc2m(v.rc)

  # two-class version
v01.rm <- matrix(0, nrow=nlevels(Vehicle01$Class), ncol=nlevels(Vehicle01$Class),
                 dimnames=list(predicted=levels(Vehicle01$Class),
                               true=levels(Vehicle01$Class)))
v01.rm["other","car"] <- 5
v01.rm["car","other"] <- 1

  # per-class cost vector
v01.rc <- rhom2c(v01.rm)

  # weighting wrapper around rpart
rpart.w <- mc.weight(rpart, predf=function(...) predict(..., type="c"))

  # decision trees with instance weighting
v.tree.w <- rpart.w$alg(Class~., v.train, v.rc)
v01.tree.w <- rpart.w$alg(Class~., v01.train, v01.rc)

  # mean misclassification cost with respect to the cost matrix
v.mc.w <- list(tree=mean.cost(rpart.w$predict(v.tree.w, v.test), v.test$Class, v.rm))
v01.mc.w <- list(tree=mean.cost(rpart.w$predict(v01.tree.w, v01.test),
                                v01.test$Class, v01.rm))

# mean misclassification cost with respect to the per-class cost vector
v.mcc.w <- list(tree=mean.cost(rpart.w$predict(v.tree.w, v.test),
                               v.test$Class, v.rcm))

  # misclassification error
v.err.w <- list(tree=err(rpart.w$predict(v.tree.w, v.test), v.test$Class))
v01.err.w <- list(tree=err(rpart.w$predict(v01.tree.w, v01.test), v01.test$Class))

  # confusion matrix
confmat(rpart.w$predict(v.tree.w, v.test), v.test$Class)
confmat(rpart.w$predict(v01.tree.w, v01.test), v01.test$Class)
}

\keyword{models}
