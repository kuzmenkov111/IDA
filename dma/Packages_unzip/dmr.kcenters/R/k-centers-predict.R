predict.k.centers <- function(model, data, diss=euc.dist)
{
  k.centers.assign(model$centers, data, diss)
}
