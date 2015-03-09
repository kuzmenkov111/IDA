bs.chisq.test <- function(v1, v2)
{
  o12 <- table(v1, v2)
  e12 <- table(v1)%*%t(table(v2))/sum(o12)
  chi2 <- sum((o12-e12)^2/e12)
  list(statistic=chi2, p.value=1-pchisq(chi2, (nrow(o12)-1)*(ncol(o12)-1)))
}

  # demonstration
bs.chisq.test(weather$outlook, weather$play)
chisq.test(weather$outlook, weather$play)
