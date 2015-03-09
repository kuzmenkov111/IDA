bs.kruskal.test <- function(v1, v2)
{
  subsets <- split(v1, v2)
  ranks <- split((rank.all <- rank(v1)), v2)
  cn <- unname(sapply(subsets, length))
  mr <- sapply(ranks, mean)
  mr.a <- mean(rank.all)

  k <- (length(v1)-1)*sum(cn*(mr-mr.a)^2)/
         sum(sapply(ranks, function(r) sum((r-mr.a)^2)))
  list(statistic=k, p.value=1-pchisq(k, length(subsets)-1))
}

  # demonstration
bs.kruskal.test(weatherc$temperature, weatherc$play)
kruskal.test(temperature~play, weatherc)
bs.kruskal.test(weatherc$temperature, weatherc$outlook)
kruskal.test(temperature~outlook, weatherc)
