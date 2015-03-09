## entropy for discrete probability distributions
entropy.p <- function(p) { sum(-plogp(p)) }

entropy <- function(v) { entropy.p(pdisc(v)) }

  # demonstration
entropy.p(c(1/5, 2/5, 3/5))
entropy(weather$outlook)
entropy(weather$play)
entropy(weather$play[weather$outlook=="overcast"])
entropy(weather$play[weather$outlook!="overcast"])
