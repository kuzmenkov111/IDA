## likelihood for probabilistic classifier evaluation
## assuming eps for 0 probabilities
lik <- function(prob.y, true.y, eps=.Machine$double.eps)
{
  prod(pmax(sapply(1:length(tn <- as.numeric(true.y)),
                   function(i) prob.y[i,tn[i]]), eps))
}

## likelihood for probabilistic binary classifier evaluation
## assuming eps for 0 probabilities
lik01 <- function(prob.y, true.y, eps=.Machine$double.eps)
{
  prod((py <- pmin(pmax(prob.y, eps), 1-eps))^(t01 <- as.num0(true.y))*(1-py)^(1-t01))
}


if (FALSE)
{

  # likelihood for the Soybean data
lik(predict(s.tree, s.test), s.test$Class)
lik(predict(s01.tree, s01.test), s01.test$Class)
lik01(predict(s01.tree, s01.test)[,2], s01.test$Class)

}
