f.test <- function(v1, v2)
{
  subsets <- split(v1, v2)
  m <- unname(sapply(subsets, mean))
  cn <- unname(sapply(subsets, length))
  m.a <- mean(v1)
  cn.a <- length(v1)

  f <- (sum(cn*(m-m.a)^2)/((k <- length(subsets))-1))/
         (sum(sapply(1:length(subsets),
                     function(i) sum((subsets[[i]]-m[i])^2)))/((cn.a-k)))
  list(statistic=f, p.value=1-pf(f, k-1, cn.a-k))
}

  # demonstration
f.test(weatherc$temperature, weatherc$outlook)
f.test(weatherc$temperature, weatherc$play)
anova(lm(temperature~outlook, weatherc))
anova(lm(temperature~play, weatherc))
abs(sqrt(f.test(weatherc$temperature, weatherc$play)$statistic)-
  abs(t.test(temperature~play, weatherc, var.equal=TRUE)$statistic))
