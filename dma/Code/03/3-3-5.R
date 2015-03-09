weighted.impurity <- function(pd1, n1, pd0, n0, imp=entropy.p)
{
  weighted.mean(c(imp(pd1), imp(pd0)), c(n1, n0))
}

  # weighted impurity of play for outlook=overcast and outlook!=overcast
weighted.impurity(pdisc(weather$play[weather$outlook=="overcast"]),
                  sum(weather$outlook=="overcast"),
                  pdisc(weather$play[weather$outlook!="overcast"]),
                  sum(weather$outlook!="overcast"))
