prob.ci.boot <- function(v, v1=1, delta=0.05, m=1000)
{
  q <- unname(quantile(sapply(1:m, function(i) prob(sample(v, replace=TRUE), v1)),
                       probs=c(delta/2, 1-delta/2)))
  list(p=prob(v, v1), low=q[1], high=q[2])
}

  # demonstration
prob.ci.boot(weather$play, "yes")
prob.ci.boot(weather$play, "yes", delta=0.01)
prob.ci.boot(weather$play, "yes", delta=0.1)
