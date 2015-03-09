weighted.prob <- function(v, v1, w=rep(1, length(v))) { sum(w[v==v1])/sum(w) }


if (FALSE)
{

  # demonstration
weighted.prob(weather$outlook, "rainy")
weighted.prob(weather$outlook, "rainy", w=ifelse(weather$play=="yes", 2, 1))

}
