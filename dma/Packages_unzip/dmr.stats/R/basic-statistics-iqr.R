iqr <- function(v)  { unname(diff(quantile(v, c(0.25, 0.75)))) }


if (FALSE)
{

  # demonstration
iqr(weatherc$temperature)

}
