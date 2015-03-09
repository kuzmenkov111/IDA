varcoef <- function(v) { sqrt(sum((v-(m <- mean(v)))^2)/(length(v)-1))/m }


if (FALSE)
{

  # demonstration
varcoef(weatherr$playability)
varcoef(-weatherr$playability)

}
