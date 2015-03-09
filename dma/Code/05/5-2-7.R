## logit representation function
repf.logit <- function(repf) { function(data, w) logit.inv(repf(data, w)) }

  # "perfect" logit model
perf.logit <- `class<-`(list(repf=repf.logit(repf.linear), w=w.perf), "par")
  # test set error
err(ustep(predict(perf.logit, lcdat.test[,1:4]), 0.5), lcdat.test$c)
  # test set loglikelihood
loglik01(predict(perf.logit, lcdat.test[,1:4]), lcdat.test$c)
