bs.rank <- function(v)
{
  r.min <- match(v, sort(v))
  r.max <- length(v)+1-match(v, rev(sort(v)))
  (r.min+r.max)/2
}

  # demonstration
bs.rank(weatherr$playability)
rank(weatherr$playability)
