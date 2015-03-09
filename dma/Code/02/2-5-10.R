bs.wilcox.test <- function(v, v01)
{
  subsets <- split(v, v01)
  ranks <- unname(split(rank(v), v01))
  cn <- unname(sapply(subsets, length))
  mu <- cn[1]*cn[2]/2
  su <- sqrt(cn[1]*cn[2]*(cn[1]+cn[2]+1)/12)

  u <- sum(ranks[[1]])-cn[1]*(cn[1]+1)/2
#  u <- sum(sapply(subsets[[2]],
#                  function(v2) sum(v2<subsets[[1]])+sum(v2==subsets[[1]])/2))
  list(statistic=u, p.value=2*(1-pnorm(abs(u-mu)/su)))
}

  # demonstration
bs.wilcox.test(weatherc$temperature, weatherc$play)
wilcox.test(temperature~play, weatherc, exact=FALSE, correct=FALSE)
