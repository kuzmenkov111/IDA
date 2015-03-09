## conditional probability distribution P(v1|v2)
pcond <- function(v1, v2)
{
  t(apply(count <- table(v1, v2, dnn=NULL), 1, "/", colSums(count)))
}

  # demonstration
pcond(weather$outlook, weather$play)
