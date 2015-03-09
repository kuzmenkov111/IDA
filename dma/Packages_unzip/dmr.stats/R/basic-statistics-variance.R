bs.var <- function(v) { sum((v-mean(v))^2)/(length(v)-1) }


if (FALSE)
{

  # demonstration
bs.var(weatherr$playability)
var(weatherr$playability)

}
