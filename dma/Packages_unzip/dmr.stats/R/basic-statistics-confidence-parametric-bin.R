prob.ci.par <- function(v, v1=1, delta=0.05)
{
  list(p=(p <- prob(v, v1)),
       low=p-(u <- qnorm(1-delta/2))*(sp <- sqrt(p*(1-p)/length(v))),
       high=p+u*sp)
}


if (FALSE)
{

  # demonstration
prob.ci.par(weather$play, "yes")
prob.ci.par(weather$play, "yes", delta=0.01)
prob.ci.par(weather$play, "yes", delta=0.1)

}
