modal <- function(v)
{
  m <- which.max(table(v))
  if (is.factor(v))
    flevels(v)[m]
  else
    sort(unique(v))[m]
}


if (FALSE)
{

  # demonstration
modal(weather$outlook)
modal(weatherr$temperature)

}
