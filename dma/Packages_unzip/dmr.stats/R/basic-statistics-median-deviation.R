bs.mad <- function(v, scale=1/qnorm(0.75)) { scale*median(abs(v-median(v))) }


if (FALSE)
{

  # demonstration
bs.mad(weatherr$playability, scale=1)
mad(weatherr$playability, constant=1)
bs.mad(weatherr$playability)
mad(weatherr$playability)
sd(weatherr$playability)

}
