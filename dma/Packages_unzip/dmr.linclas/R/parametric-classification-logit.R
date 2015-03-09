## logit representation function
repf.logit <- function(repf) { function(data, w) logit.inv(repf(data, w)) }

## logit representation gradient
grad.logit <- function(repf, grad)
{ function(data, w) rmm(grad(data, w), (p <- repf.logit(repf)(data, w))*(1-p)) }


if (FALSE)
{

  # perfect logit model
perf.logit <- `class<-`(list(repf=repf.logit(repf.perf), w=w.perf), "par")
  # test set error
err(ustep(predict(perf.logit, pcdat.test[,1:4]), 0.5), pcdat.test$c)
  # test set loglikelihood
loglik01(predict(perf.logit, pcdat.test[,1:4]), pcdat.test$c)

}
