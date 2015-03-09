## Gini index for discrete probability distributions
gini.p <- function(p) { 1-sum(p^2) }

gini <- function(v) { gini.p(pdisc(v)) }

  # demonstration
gini.p(c(1/5, 2/5, 3/5))
gini(weather$outlook)
gini(weather$play)
gini(weather$play[weather$outlook=="overcast"])
gini(weather$play[weather$outlook!="overcast"])
