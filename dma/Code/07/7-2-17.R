## loglikelihood for probabilistic classifier evaluation
## assuming eps for 0 probabilities
loglik <- function(prob.y, true.y, eps=.Machine$double.eps)
{
  sum(log(pmax(sapply(1:length(tn <- as.numeric(true.y)),
                      function(i) prob.y[i,tn[i]]), eps)))
}

## loglikelihood for probabilistic binary classifier evaluation
## assuming eps for 0 probabilities
loglik01 <- function(prob.y, true.y, eps=.Machine$double.eps)
{
  sum((t01 <- as.num0(true.y))*log(py <- pmin(pmax(prob.y, eps), 1-eps))+
      (1-t01)*log(1-py))
}

  # loglikelihood for the Soybean data
loglik(predict(s.tree, s.test), s.test$Class)
loglik(predict(s01.tree, s01.test), s01.test$Class)
loglik01(predict(s01.tree, s01.test)[,2], s01.test$Class)
