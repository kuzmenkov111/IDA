varcoef <- function(v) { sqrt(sum((v-(m <- mean(v)))^2)/(length(v)-1))/m }

  # demonstration
varcoef(weatherr$playability)
varcoef(-weatherr$playability)
