\name{mc.experiment}
\alias{mc.experiment}

\title{An example function from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{An example function from Chapter 6 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 6.5, Example 6.5.1.}

\arguments{See Section 6.5, Example 6.5.1.}

\details{See Section 6.5, Example 6.5.1.}

\value{See Section 6.5, Example 6.5.1.}

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
\code{\link{mc.relabel}}
}

\examples{
library(rpart)
library(e1071)
library(ipred)
data(Vehicle, package="mlbench")

set.seed(12)

Vehicle01 <- Vehicle
Vehicle01$Class <- factor(ifelse(Vehicle$Class \%in\% c("opel", "saab"),
                                 "car", "other"))
  # experiments with decision trees and naive Bayes
  # for the Vehicle and Vehicle01 datasets
mc.res <- mc.experiment(c("rpart", "rpart", "naiveBayes"),
                        c("Vehicle", "Vehicle01"), c("Class", "Class"), k=5,
                        palgs=list(NULL, "bagging", NULL),
                        args.p=list(list(cp=0.025), list(cp=0.025), NULL),
                        pargs=list(NULL, list(control=rpart.control(cp=0.025)),
                                   NULL),
                        predfs=c(function(...) predict(..., type="c"),
                                 function(...) predict(..., type="c"), predict),
                        ppredfs.algs=c(predict, predict,
                                       function(...) predict(..., type="r")),
                        ppredfs.palgs=list(NULL,
                                           function(...) predict(..., type="p",
                                                                 aggregation="a"),
                                           NULL))

barplot(colMeans(cbind(mc.res$Vehicle$rpart.NULL$mc[,6:9],
                       mc.res$Vehicle$rpart.bagging$mc[,3])),
        main="Four-class, rpart", ylab="Cost reduction",
        las=2, col="blue", ylim=c(-0.01, 0.11),
        names.arg=c("weight", "resample", "mincost", "relabel", "relabel.b"))
barplot(colMeans(mc.res$Vehicle$naiveBayes.NULL$mc[,7:9]),
        main="Four-class, naiveBayes", ylab="Cost reduction",
        las=2, col="blue", ylim=c(-0.01, 0.11),
        names.arg=c("resample", "mincost", "relabel"))

barplot(colMeans(cbind(mc.res$Vehicle01$rpart.NULL$mc[,6:9],
                       mc.res$Vehicle01$rpart.bagging$mc[,3])),
        main="Two-class, rpart", ylab="Cost reduction",
        las=2, col="blue", ylim=c(-0.26, 0.15),
        names.arg=c("weight", "resample", "mincost", "relabel", "relabel.b"))
barplot(colMeans(mc.res$Vehicle01$naiveBayes.NULL$mc[,7:9]),
        main="Two-class, naiveBayes", ylab="Cost reduction",
        las=2, col="blue", ylim=c(-0.26, 0.15),
        names.arg=c("resample", "mincost", "relabel"))
}

\keyword{models}
