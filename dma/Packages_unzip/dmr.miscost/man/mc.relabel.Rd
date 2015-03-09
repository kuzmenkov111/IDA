\name{mc.relabel}
\alias{mc.relabel}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 6 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 6.3, Example 6.3.4.}

\arguments{See Section 6.3, Example 6.3.4.}

\details{See Section 6.3, Example 6.3.4.}

\value{See Section 6.3, Example 6.3.4.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{mc.weight}}
\code{\link{mc.resample}}
\code{\link{mc.mincost}}
}

\examples{
library(rpart)
library(e1071)
library(ipred)
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

  # relabeling wrapper around rpart
rpart.l <- mc.relabel(rpart, pargs=list(cp=0.025),
                      predf=function(...) predict(..., type="c"))

  # relabeling wrapper around rpart using bagging for probability estimation
rpart.bagg.l <- mc.relabel(rpart, bagging,
                           pargs=list(control=rpart.control(cp=0.025)),
                           predf=function(...) predict(..., type="c"),
                           ppredf=function(...) predict(..., type="p",
                                                             aggregation="a"))

  # relabeling wrapper around naiveBayes
naiveBayes.l <- mc.relabel(naiveBayes, ppredf=function(...) predict(..., type="r"))

  # decision trees with instance relabeling
v.tree.l <- rpart.l$alg(Class~., v.train, v.rm)
v.tree.bagg.l <- rpart.bagg.l$alg(Class~., v.train, v.rm)
v01.tree.l <- rpart.l$alg(Class~., v01.train, v01.rm)
v01.tree.bagg.l <- rpart.bagg.l$alg(Class~., v01.train, v01.rm)

  # naive Bayes with instance relabeling
v.nb.l <- naiveBayes.l$alg(Class~., v.train, v.rm)
v01.nb.l <- naiveBayes.l$alg(Class~., v01.train, v01.rm)

  # mean misclassification cost with respect to the cost matrix
v.mc.l <- list(tree=mean.cost(rpart.l$predict(v.tree.l, v.test), v.test$Class, v.rm),
               tree.bagg=mean.cost(rpart.bagg.l$predict(v.tree.bagg.l, v.test),
                                   v.test$Class, v.rm),
               nb=mean.cost(naiveBayes.l$predict(v.nb.l, v.test),
                            v.test$Class, v.rm))
v01.mc.l <- list(tree=mean.cost(rpart.l$predict(v01.tree.l, v01.test),
                                v01.test$Class, v01.rm),
                 tree.bagg=mean.cost(rpart.bagg.l$predict(v01.tree.bagg.l, v01.test),
                                     v01.test$Class, v01.rm),
                 nb=mean.cost(naiveBayes.l$predict(v01.nb.l, v01.test),
                              v01.test$Class, v01.rm))

# mean misclassification cost with respect to the per-class cost vector
v.mcc.l <- list(tree=mean.cost(rpart.l$predict(v.tree.l, v.test),
                               v.test$Class, v.rcm),
                tree.bagg=mean.cost(rpart.bagg.l$predict(v.tree.bagg.l, v.test),
                                    v.test$Class, v.rcm),
                nb=mean.cost(naiveBayes.l$predict(v.nb.l, v.test),
                             v.test$Class, v.rcm))

  # misclassification error
v.err.l <- list(tree=err(rpart.l$predict(v.tree.l, v.test), v.test$Class),
                tree.bagg=err(rpart.bagg.l$predict(v.tree.bagg.l, v.test),
                              v.test$Class),
                nb=err(naiveBayes.l$predict(v.nb.l, v.test), v.test$Class))
v01.err.l <- list(tree=err(rpart.l$predict(v01.tree.l, v01.test), v01.test$Class),
                  tree.bagg=err(rpart.bagg.l$predict(v01.tree.bagg.l, v01.test),
                                v01.test$Class),
                  nb=err(naiveBayes.l$predict(v01.nb.l, v01.test), v01.test$Class))

  # confusion matrix
confmat(rpart.l$predict(v.tree.l, v.test), v.test$Class)
confmat(rpart.bagg.l$predict(v.tree.bagg.l, v.test), v.test$Class)
confmat(naiveBayes.l$predict(v.nb.l, v.test), v.test$Class)

confmat(rpart.l$predict(v01.tree.l, v01.test), v01.test$Class)
confmat(naiveBayes.l$predict(v01.nb.l, v01.test), v01.test$Class)
}

\keyword{models}
