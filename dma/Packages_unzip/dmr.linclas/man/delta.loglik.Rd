\name{delta.loglik}
\alias{delta.loglik}
\alias{delta.err}

\title{Example functions from the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R}

\description{Example functions from Chapter 5 of the book Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. See Appendix B or http://www.wiley.com/go/data_mining_algorithms for more details.}

\usage{See Section 5.3, Example 5.3.3.}

\arguments{See Section 5.3, Example 5.3.3.}

\details{See Section 5.3, Example 5.3.3.}

\value{See Section 5.3, Example 5.3.3.}

\references{Cichosz, P. (2015): Data Mining Algorithms: Explained Using R. Wiley.}

\author{
Pawel Cichosz <p.cichosz@elka.pw.edu.pl>
}

\note{
}


\seealso{
\code{\link{delta.mse}}
\code{\link{gradient.descent}}
\code{\link{predict.par}}
}

\examples{
library(dmr.claseval)

repf.perf <- repf.poly(2)
grad.perf <- grad.poly(2)

  # estimate parameters for the threshold representation
gd.th <- gradient.descent(c~., pcdat.train, w=rep(0, 9),
                          repf=repf.threshold(repf.perf),
                          grad=grad.threshold(grad.perf),
                          delta=delta.err, perf=err,
                          beta=1, batch=TRUE, eps=0.001)

  # estimate parameters for the logit representation
gd.lt <- gradient.descent(c~., pcdat.train, w=rep(0, 9),
                          repf=repf.logit(repf.perf),
                          grad=grad.logit(repf.perf, grad.perf),
                          delta=delta.loglik, perf=function(p, y) -loglik01(p, y),
                          beta=0.001, batch=TRUE, eps=5)

  # training set error
err(predict(gd.th$model, pcdat.train[,1:4]), pcdat.train$c)
err(ustep(predict(gd.lt$model, pcdat.train[,1:4]), 0.5), pcdat.train$c)

  # training set loglikelihood
loglik01(predict(gd.th$model, pcdat.train[,1:4]), pcdat.train$c)
loglik01(predict(gd.lt$model, pcdat.train[,1:4]), pcdat.train$c)

  # test set error
err(predict(gd.th$model, pcdat.test[,1:4]), pcdat.test$c)
err(ustep(predict(gd.lt$model, pcdat.test[,1:4]), 0.5), pcdat.test$c)

  # test set loglikelihood
loglik01(predict(gd.th$model, pcdat.test[,1:4]), pcdat.test$c)
loglik01(predict(gd.lt$model, pcdat.test[,1:4]), pcdat.test$c)

  # linearly separable subsets
lcdat.ls <- linsep.sub(c~., lcdat)
lcdat.train.ls <- lcdat[1:200,][lcdat.ls[1:200],]
lcdat.test.ls <- lcdat[201:400,][lcdat.ls[201:400],]


  # linear threshold for the artificial data
gdl.th <- gradient.descent(c~., lcdat.train, w=rep(0, 5),
                           repf=repf.threshold(repf.linear),
                           grad=grad.threshold(grad.linear),
                           delta=delta.err, perf=err,
                           beta=1, batch=TRUE, eps=0.03)
gdl.th.ls <- gradient.descent(c~., lcdat.train.ls, w=rep(0, 5),
                              repf=repf.threshold(repf.linear),
                              grad=grad.threshold(grad.linear),
                              delta=delta.err, perf=err,
                              beta=1, batch=TRUE, eps=0.001)

  # linear logit for the artificial data
gdl.lt <- gradient.descent(c~., lcdat.train, w=rep(0, 5),
                           repf=repf.logit(repf.linear),
                           grad=grad.logit(repf.linear, grad.linear),
                           delta=delta.loglik, perf=function(p, y) -loglik01(p, y),
                           beta=0.01, batch=TRUE, eps=15.4)
gdl.lt.ls <- gradient.descent(c~., lcdat.train.ls, w=rep(0, 5),
                              repf=repf.logit(repf.linear),
                              grad=grad.logit(repf.linear, grad.linear),
                              delta=delta.loglik,
                              perf=function(p, y) -loglik01(p, y),
                              beta=0.1, batch=TRUE, eps=3)

  # training set error
err(predict(gdl.th$model, lcdat.train[,1:4]), lcdat.train$c)
err(ustep(predict(gdl.lt$model, lcdat.train[,1:4]), 0.5), lcdat.train$c)

err(predict(gdl.th.ls$model, lcdat.train.ls[,1:4]), lcdat.train.ls$c)
err(ustep(predict(gdl.lt.ls$model, lcdat.train.ls[,1:4]), 0.5), lcdat.train.ls$c)

  # training set loglikelihood
loglik01(predict(gdl.th$model, lcdat.train[,1:4]), lcdat.train$c)
loglik01(predict(gdl.lt$model, lcdat.train[,1:4]), lcdat.train$c)

loglik01(predict(gdl.th.ls$model, lcdat.train.ls[,1:4]), lcdat.train.ls$c)
loglik01(predict(gdl.lt.ls$model, lcdat.train.ls[,1:4]), lcdat.train.ls$c)

  # test set error
err(predict(gdl.th$model, lcdat.test[,1:4]), lcdat.test$c)
err(ustep(predict(gdl.lt$model, lcdat.test[,1:4]), 0.5), lcdat.test$c)

err(predict(gdl.th.ls$model, lcdat.test.ls[,1:4]), lcdat.test.ls$c)
err(ustep(predict(gdl.lt.ls$model, lcdat.test.ls[,1:4]), 0.5), lcdat.test.ls$c)

  # test set loglikelihood
loglik01(predict(gdl.th$model, lcdat.test[,1:4]), lcdat.test$c)
loglik01(predict(gdl.lt$model, lcdat.test[,1:4]), lcdat.test$c)

loglik01(predict(gdl.th.ls$model, lcdat.test.ls[,1:4]), lcdat.test.ls$c)
loglik01(predict(gdl.lt.ls$model, lcdat.test.ls[,1:4]), lcdat.test.ls$c)
}

\keyword{models}
