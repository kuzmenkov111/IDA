bs.sd <- function(v) { sqrt(sum((v-mean(v))^2)/(length(v)-1)) }


if (FALSE)
{

  # demonstration
bs.sd(weatherr$playability)
sd(weatherr$playability)

}
