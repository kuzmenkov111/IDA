mutinfo <- function(v1, v2)
{
  p12 <- pdisc(v1, v2)
  p1 <- rowSums(p12)
  p2 <- colSums(p12)
  sum(p12*logp(p12/(p1%o%p2)), na.rm=TRUE)
}

  # demonstration
mutinfo(weather$outlook, weather$play)
  # this should be the same
entropy(weather$play)-entropy.cond(weather$play, weather$outlook)
entropy(weather$outlook)-entropy.cond(weather$outlook, weather$play)
g.test(weather$outlook, weather$play)$statistic/(2*log(2)*nrow(weather))
