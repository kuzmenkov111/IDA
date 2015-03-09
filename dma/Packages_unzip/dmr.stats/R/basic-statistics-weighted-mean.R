bs.weighted.mean <- function(v, w=rep(1, length(v))) { sum(w*v)/sum(w) }


if (FALSE)
{

  # demonstration
bs.weighted.mean(weatherc$temperature, ifelse(weatherc$play=="yes", 5, 1))
weighted.mean(weatherc$temperature, ifelse(weatherc$play=="yes", 5, 1))

}
