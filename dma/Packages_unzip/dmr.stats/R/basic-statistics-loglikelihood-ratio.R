g.test <- function(v1, v2)
{
  o12 <- table(v1, v2)
  e12 <- table(v1)%*%t(table(v2))/sum(o12)
  g <- 2*sum(o12*log(o12/e12), na.rm=TRUE)
  list(statistic=g, p.value=1-pchisq(g, (nrow(o12)-1)*(ncol(o12)-1)))
}


if (FALSE)
{

  # demonstration
g.test(weather$outlook, weather$play)

}
