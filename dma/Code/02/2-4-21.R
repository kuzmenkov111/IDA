prob <- function(v, v1) { sum(v==v1)/length(v) }

  # demonstration
prob(weather$outlook, "rainy")
