bs.mean <- function(v) { sum(v)/length(v) }

  # demonstration
bs.mean(weatherc$temperature)
mean(weatherc$temperature)
