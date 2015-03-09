## calculate parameter update based on given true and predicted values,
## gradient, and step-size using the delta rule for loglikelihood minimization
delta.loglik <- function(true.y, pred.y, gr, beta)
{
  d <- ifelse(is.finite(d <- rmm(gr, 1/(pred.y*(1-pred.y)))), d, 1)
  colSums(beta*rmm(d, true.y-pred.y))
}

## calculate parameter update based on given true and predicted values,
## gradient, and step-size using the delta rule for error minimization
delta.err <- delta.mse

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

  # linear threshold for the Pima Indians Diabetes data
pid.gdl.th <- gradient.descent(diabetes~., pid.train, w=rep(0, ncol(pid.train)),
                               repf=repf.threshold(repf.linear),
                               grad=grad.threshold(grad.linear),
                               delta=delta.err, perf=err,
                               beta=1, batch=TRUE, eps=0.28, niter=10000)

  # linear logit for the Pima Indians Diabetes data
pid.gdl.lt <- gradient.descent(diabetes~., pid.train, w=rep(0, ncol(pid.train)),
                               repf=repf.logit(repf.linear),
                               grad=grad.logit(repf.linear, grad.linear),
                               delta=delta.loglik,
                               perf=function(p, y) -loglik01(p, y),
                               beta=1e-7, batch=TRUE, eps=250, niter=1e6)

  # training set error
err(predict(gdl.th$model, lcdat.train[,1:4]), lcdat.train$c)
err(ustep(predict(gdl.lt$model, lcdat.train[,1:4]), 0.5), lcdat.train$c)

err(predict(gdl.th.ls$model, lcdat.train.ls[,1:4]), lcdat.train.ls$c)
err(ustep(predict(gdl.lt.ls$model, lcdat.train.ls[,1:4]), 0.5), lcdat.train.ls$c)

err(factor(predict(pid.gdl.th$model, pid.train[,-9]),
           levels=0:1, labels=levels(pid.train$diabetes)),
           pid.train$diabetes)
err(factor(ustep(predict(pid.gdl.lt$model, pid.train[,-9]), 0.5),
           levels=0:1, labels=levels(pid.train$diabetes)),
    pid.train$diabetes)

  # training set loglikelihood
loglik01(predict(gdl.th$model, lcdat.train[,1:4]), lcdat.train$c)
loglik01(predict(gdl.lt$model, lcdat.train[,1:4]), lcdat.train$c)

loglik01(predict(gdl.th.ls$model, lcdat.train.ls[,1:4]), lcdat.train.ls$c)
loglik01(predict(gdl.lt.ls$model, lcdat.train.ls[,1:4]), lcdat.train.ls$c)

loglik01(predict(pid.gdl.th$model, pid.train[,-9]), pid.train$diabetes)
loglik01(predict(pid.gdl.lt$model, pid.train[,-9]), pid.train$diabetes)

  # test set error
err(predict(gdl.th$model, lcdat.test[,1:4]), lcdat.test$c)
err(ustep(predict(gdl.lt$model, lcdat.test[,1:4]), 0.5), lcdat.test$c)

err(predict(gdl.th.ls$model, lcdat.test.ls[,1:4]), lcdat.test.ls$c)
err(ustep(predict(gdl.lt.ls$model, lcdat.test.ls[,1:4]), 0.5), lcdat.test.ls$c)

err(factor(predict(pid.gdl.th$model, pid.test[,-9]),
           levels=0:1, labels=levels(pid.train$diabetes)),
    pid.test$diabetes)
err(factor(ustep(predict(pid.gdl.lt$model, pid.test[,-9]), 0.5),
           levels=0:1, labels=levels(pid.train$diabetes)),
    pid.test$diabetes)

  # test set loglikelihood
loglik01(predict(gdl.th$model, lcdat.test[,1:4]), lcdat.test$c)
loglik01(predict(gdl.lt$model, lcdat.test[,1:4]), lcdat.test$c)

loglik01(predict(gdl.th.ls$model, lcdat.test.ls[,1:4]), lcdat.test.ls$c)
loglik01(predict(gdl.lt.ls$model, lcdat.test.ls[,1:4]), lcdat.test.ls$c)

loglik01(predict(pid.gdl.th$model, pid.test[,-9]), pid.test$diabetes)
loglik01(predict(pid.gdl.lt$model, pid.test[,-9]), pid.test$diabetes)
