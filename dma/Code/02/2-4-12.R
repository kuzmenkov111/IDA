bs.sd <- function(v) { sqrt(sum((v-mean(v))^2)/(length(v)-1)) }

  # demonstration
bs.sd(weatherr$playability)
sd(weatherr$playability)
