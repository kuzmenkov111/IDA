weighted.var <- function(v, w=rep(1, length(v)))
{
  sw <- sum(w)
  ssw <- sum(w^2)
  wm <- weighted.mean(v, w)
  sw/(sw^2-ssw)*sum(w*(v-wm)^2)
}

  # demonstration
weighted.var(weatherr$playability)
weighted.var(weatherr$playability, ifelse(weatherr$outlook=="rainy", 2, 1))
