minvar <- 0.005
minsplit <- 2
maxdepth <- 8

stop.criteria <- function(n)
{
  n>=2^maxdepth || tree$count[tree$node==n]<minsplit ||
    tree$variance[tree$node==n]<minvar
}

stop.criteria(n)
