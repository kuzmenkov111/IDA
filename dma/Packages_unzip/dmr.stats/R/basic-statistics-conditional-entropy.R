entropy.cond <- function(v1, v2)
{
  p12 <- pdisc(v1, v2)
  p2 <- colSums(p12)
  sum(p2*mapply(function(i, p2i) entropy.p(p12[,i]/p2i), 1:ncol(p12), p2))
}


if (FALSE)
{

  # demonstration
entropy.cond(weather$play, weather$outlook)
entropy.cond(weather$play, weather$outlook=="rainy")

}
