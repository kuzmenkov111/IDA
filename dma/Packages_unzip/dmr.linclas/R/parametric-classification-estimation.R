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


if (FALSE)
{

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

}
