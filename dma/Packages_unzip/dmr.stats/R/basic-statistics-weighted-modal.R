## weighted modal value
weighted.modal <- function(v, w=rep(1, length(v)))
{
  m <- which.max(weighted.table(v, w=w))
  if (is.factor(v))
    factor(levels(v)[m], levels=levels(v))
  else
    sort(unique(v))[m]
}


if (FALSE)
{

  # demonstration
weighted.modal(weather$outlook)
weighted.modal(weather$outlook, w=ifelse(weather$play=="yes", 2, 1))

}
